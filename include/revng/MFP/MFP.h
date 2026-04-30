#pragma once

#pragma clang optimize off

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <compare>
#include <cstddef>
#include <map>
#include <queue>
#include <type_traits>
#include <unordered_map>
#include <utility>

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/raw_ostream.h"

#include "revng/ADT/Concepts.h"
#include "revng/ADT/GenericGraph.h"
#include "revng/ADT/ReversePostOrderTraversal.h"

// WIP: lowercase
namespace MFP {

/// Position relative to a `Key` recorded into an `ExtraState`.
enum class Position : unsigned char {
  Before,
  After
};

/// Sentinel `ExtraStateType` for MFIs that do not need to record any
/// intermediate state. MFIs that don't care declare
/// `using ExtraStateType = MFP::NoExtraState;` and take `NoExtraState &` as
/// the third argument of `applyTransferFunction`.
struct NoExtraState {};

/// Recording surface that `applyTransferFunction` writes to so callers can
/// inspect intermediate lattice values at finer granularity than a `Label`
/// (e.g. instruction-level state inside a basic-block label).
///
/// The caller marks the `(Key, Position)` pairs of interest with
/// `registerAsInteresting{Before,After}` before passing the instance to
/// `getMaximalFixedPoint`. The transfer function then calls
/// `register{Before,After}` for each `(Key, Position)` it observes; only the
/// pairs already in the map are stored. After convergence the caller reads
/// the fixed-point values back via `get{Before,After}`.
template<typename KeyT, typename LatticeElement>
class ExtraState {
public:
  using Key = KeyT;
  using KeyAndPosition = std::pair<Key, Position>;

private:
  struct Hash {
    size_t operator()(const KeyAndPosition &P) const noexcept {
      return llvm::hash_combine(P.first, static_cast<int>(P.second));
    }
  };

  /// Use `llvm::DenseMap` when the lattice element is small enough to be
  /// stored cheaply inline (DenseMap reserves space for empty/tombstone slots
  /// proportional to the value size); fall back to `std::unordered_map`
  /// otherwise. The `Key` type must have a `llvm::DenseMapInfo`
  /// specialization to take the `DenseMap` path.
  using MapType = std::conditional_t<
    (sizeof(LatticeElement) <= 2 * sizeof(void *)),
    llvm::DenseMap<KeyAndPosition, LatticeElement>,
    std::unordered_map<KeyAndPosition, LatticeElement, Hash>>;

  /// The presence of a `(Key, Position)` entry marks it as interesting; the
  /// stored value is the most recently recorded one.
  MapType Map;

public:
  /// @{
  /// Mark a `(Key, Position)` pair as interesting. Caller-side.
  void registerAsInterestingBefore(const Key &K) {
    Map.try_emplace({ K, Position::Before });
  }

  void registerAsInterestingAfter(const Key &K) {
    Map.try_emplace({ K, Position::After });
  }
  /// @}

  /// @{
  /// Record `Value` for `(K, Position)`. No-op if not interesting. Called
  /// from inside `applyTransferFunction`.
  void registerBefore(const Key &K, const LatticeElement &Value) {
    auto It = Map.find({ K, Position::Before });
    if (It != Map.end())
      It->second = Value;
  }

  void registerAfter(const Key &K, const LatticeElement &Value) {
    auto It = Map.find({ K, Position::After });
    if (It != Map.end())
      It->second = Value;
  }
  /// @}

  /// @{
  /// Retrieve the recorded value. Caller-side, after `getMaximalFixedPoint`.
  const LatticeElement &getBefore(const Key &K) const {
    auto It = Map.find({ K, Position::Before });
    revng_assert(It != Map.end());
    return It->second;
  }

  const LatticeElement &getAfter(const Key &K) const {
    auto It = Map.find({ K, Position::After });
    revng_assert(It != Map.end());
    return It->second;
  }
  /// @}
};

inline Logger NullLogger("");

template<typename T>
void dump(llvm::raw_ostream &Stream, unsigned Indent, const T &Element) {
  for (unsigned I = 0; I < Indent; ++I)
    Stream << "  ";
  Stream << "(not implemented)\n";
}

template<typename T>
void dumpLabel(llvm::raw_ostream &Stream, const T &Element) {
  Stream << "(not implemented)";
}

template<typename LatticeElement>
struct MFPResult {
  LatticeElement InValue;
  LatticeElement OutValue;
};

/// GT is an instance of llvm::GraphTraits e.g. llvm::GraphTraits<GraphType>
template<typename GT>
auto successors(typename GT::NodeRef From) {
  return llvm::make_range(GT::child_begin(From), GT::child_end(From));
}

template<typename MFI, typename LatticeElement = typename MFI::LatticeElement>
concept MonotoneFrameworkInstance = requires(const MFI &I,
                                             LatticeElement E1,
                                             LatticeElement E2,
                                             typename MFI::Label L,
                                             typename MFI::ExtraStateType &ES) {
  /// To compute the reverse post order traversal of the graph starting from
  /// the extremal nodes, we need that the nodes also represent a subgraph
  typename llvm::GraphTraits<typename MFI::Label>::NodeRef;

  std::same_as<typename MFI::Label,
               typename llvm::GraphTraits<typename MFI::GraphType>::NodeRef>;
  { I.combineValues(E1, E2) } -> std::same_as<LatticeElement>;
  { I.isLessOrEqual(E1, E2) } -> std::same_as<bool>;
  { I.applyTransferFunction(L, E2, ES) } -> std::same_as<LatticeElement>;
};

template<typename GT>
concept HasNodeRange = requires() {
  { GT::nodes_begin };
  { GT::nodes_end };
};

template<typename Label, typename LatticeElement>
using ResultMap = std::map<Label, MFPResult<LatticeElement>>;

template<MonotoneFrameworkInstance MFI>
using MFIResultMap = ResultMap<typename MFI::Label,
                               typename MFI::LatticeElement>;

/// Compute the solution to the given instance of a monotone framework.
///
/// \tparam MFIType the type of the monotone framework instance. See the
//          MonotoneFrameworkInstance concept.
/// \tparam GT the GraphTraits to use to use. Defaults to
///         GraphTraits<MFIType::GraphType>. To navigate the inverse graph pass
///         GraphTraits<Inverse<...>>.
///
/// \param MFI the monotone framework instance.
/// \param Flow the graph on which the monotone framework will run.
/// \param Bottom the value that will be used to initialize all the non-extremal
//         nodes.
/// \param ExtremalValue
/// \param ExtremalLabels
/// \param EntryNodes
/// \param Logger a logger where the advancement of the MFP algorithm should be
//         reported.

// WIP: reorder arguments
// WIP: can we make InitialNodes optional (instead of having an overload)?

// include/revng/RestructureCFG/RegionCFGTreeImpl.h
// ({}, &Graph, {}, {}, {}, Exits)

// lib/Canonicalize/SwitchToStatements.cpp
// ({},
//   TheGraph,
//   Bottom,
//   Empty,
//   { TheGraph->getEntryNode() })

// lib/EarlyFunctionAnalysis/AnalyzeRegisterUsage.cpp
// (Liveness,
// &Function.Function,
// Liveness.defaultValue(),
// Liveness.defaultValue(),
// { Function.ReturnNode })

// (ReachingDefinitions,
// &Function.Function,
// DefaultValue,
// DefaultValue,
// { EntryNode })

// // lib/FunctionIsolation/PromoteCSVs.cpp
// ({},
// &CallGraph,
// {},
// {},
// {},
// {})
// // lib/PromoteStackPointer/SegregateStackAccessesPass.cpp
// ({},
// &F,
// {},
// {},
// { Entry })

// // lib/TypeShrinking/BitLiveness.cpp
// ({},
// &DataFlowGraph,
// 0,
// Top,
// ExtremalLabels)

// // lib/ValueMaterializer/AdvancedValueInfo.cpp
// (AVIMFI,
// &CFEG,
// {},
// ExtremalValue,
// InitialNodes,
// InitialNodes,
// AVILogger)

// // tests/unit/RegisterUsageAnalyses.cpp
// (LA,
// &Function,
// LA.defaultValue(),
// LA.defaultValue(),
// { Entry })

// (RD,
// &F.Function,
// RD.defaultValue(),
// RD.defaultValue(),
// { F.Entry })

/// Compute the maximum fixed points of an instance of monotone framework GT an
/// instance of llvm::GraphTraits that tells us how to visit the graph LGT a
/// graph type that tells us how to visit the subgraph induced by a node in the
/// graph. This is needed for the RPOT because for certain graph (e.g.
/// Inverse<...>) the nodes don't necessary carry all the information that
/// GraphType has.
template<MonotoneFrameworkInstance MFIType,
         typename GT = llvm::GraphTraits<typename MFIType::GraphType>>
MFIResultMap<MFIType>
getMaximalFixedPointImpl(const MFIType &MFI,
                         typename MFIType::GraphType Flow,
                         typename MFIType::LatticeElement Bottom,
                         typename MFIType::LatticeElement ExtremalValue,
                         const std::vector<typename MFIType::Label>
                           &ExtremalLabels,
                         const std::vector<typename MFIType::Label> &EntryNodes,
                         typename MFIType::ExtraStateType &ExtraState,
                         Logger &Logger = NullLogger) {
  using Label = typename MFIType::Label;
  using LatticeElement = typename MFIType::LatticeElement;

  if (Logger.isEnabled()) {
    revng_log(Logger, "Initializing extremal labels");
    LoggerIndent Indent(Logger);
    Logger << "Extremal value:\n";
    MFP::dump(*Logger.getAsLLVMStream(), 1, ExtremalValue);
    Logger << DoLog;

    Logger << "Extremal labels:" << DoLog;
    LoggerIndent Indent2(Logger);
    for (Label ExtremalLabel : ExtremalLabels) {
      MFP::dumpLabel(*Logger.getAsLLVMStream(), ExtremalLabel);
      Logger << DoLog;
    }

    revng_log(Logger, "Initializing initial nodes");
    LoggerIndent Indent3(Logger);
    Logger << "Initial value:\n";
    MFP::dump(*Logger.getAsLLVMStream(), 1, Bottom);
    Logger << DoLog;

    Logger << "Initial labels:" << DoLog;
    LoggerIndent Indent4(Logger);
    for (Label InitialNode : EntryNodes) {
      MFP::dumpLabel(*Logger.getAsLLVMStream(), InitialNode);
      Logger << DoLog;
    }
  }

  std::map<Label, MFPResult<LatticeElement>> AnalysisResult;

  // Initialize the state of the analysis: associate extremal labels to extremal
  // values
  for (Label ExtremalLabel : ExtremalLabels)
    AnalysisResult[ExtremalLabel].InValue = ExtremalValue;

  struct WorklistItem {
    size_t Priority;
    Label Item;

    std::weak_ordering operator<=>(const WorklistItem &) const = default;
  };
  std::set<WorklistItem> Worklist;
  std::map<Label, size_t> LabelPriority;

  //
  // Initialize the worklist with the nodes in reverse post order.
  // Also, record the visit order as priority.
  //
  // If the graph has multiple initial nodes, we perform a reverse post order
  // visit from each initial node, sharing the list of visited nodes with
  // previous visits.
  {
    using NodeSet = llvm::SmallSet<Label, 8>;
    NodeSet Visited;
    for (Label Start : EntryNodes) {

      if (Visited.contains(Start))
        continue;

      ReversePostOrderTraversalExt<Label, GT, NodeSet> RPOT(Start, Visited);
      for (Label Node : RPOT) {
        LabelPriority[Node] = LabelPriority.size();
        Worklist.insert({ LabelPriority.at(Node), Node });

        // Initialize the analysis value for non extremal nodes
        if (not AnalysisResult.contains(Node))
          AnalysisResult[Node].InValue = Bottom;
      }
    }
  }

  // Step 2 iterations
  revng_log(Logger, "Starting the iterations");
  LoggerIndent Indent(Logger);

  unsigned IterationIndex = 0;
  while (not Worklist.empty()) {
    // Fetch the next label from the worklist
    WorklistItem First = *Worklist.begin();
    Label Start = First.Item;
    Worklist.erase(First);

    auto &LabelAnalysis = AnalysisResult.at(Start);

    if (Logger.isEnabled()) {
      Logger << "Iteration #" << IterationIndex << " on ";
      MFP::dumpLabel(*Logger.getAsLLVMStream(), Start);
      Logger << DoLog;
    }

    LoggerIndent Indent(Logger);

    if (Logger.isEnabled()) {
      Logger << "Initial value:\n";
      MFP::dump(*Logger.getAsLLVMStream(), 1, LabelAnalysis.InValue);
      Logger << DoLog;

      Logger << "Final value:\n";
      MFP::dump(*Logger.getAsLLVMStream(), 1, LabelAnalysis.OutValue);
      Logger << DoLog;
    }

    // Run the transfer function.
    revng_log(Logger, "Running the transfer function");
    Logger.indent();
    const auto New = MFI.applyTransferFunction(Start,
                                               LabelAnalysis.InValue,
                                               ExtraState);
    Logger.unindent();

    if (Logger.isEnabled()) {
      LoggerIndent Indent(Logger);
      Logger << "New final value:\n";
      MFP::dump(*Logger.getAsLLVMStream(), 1, New);
      Logger << DoLog;
    }

    // TODO: assert that MFI.isLessOrEqual(LabelAnalysis.OutValue, New)

    // Save the new final value
    LabelAnalysis.OutValue = New;

    // Enqueue successors that need to be recomputed
    revng_log(Logger, "Processing successors:");
    LoggerIndent Indent2(Logger);
    for (Label Successor : successors<GT>(Start)) {
      auto &SuccessorResults = AnalysisResult.at(Successor);

      if (Logger.isEnabled()) {
        Logger << "Considering successor ";
        MFP::dumpLabel(*Logger.getAsLLVMStream(), Successor);
        Logger << DoLog;

        Logger << "Initial value:\n";
        LoggerIndent Indent(Logger);
        Logger << DoLog;
        MFP::dump(*Logger.getAsLLVMStream(), 1, SuccessorResults.InValue);
        Logger << DoLog;
      }
      LoggerIndent Indent(Logger);

      if (not MFI.isLessOrEqual(LabelAnalysis.OutValue,
                                SuccessorResults.InValue)) {
        // We need to re-enqueue

        // Combine the old value with the new incoming value and update it
        SuccessorResults.InValue = MFI.combineValues(SuccessorResults.InValue,
                                                     LabelAnalysis.OutValue);

        if (Logger.isEnabled()) {
          Logger << "Enqueuing. New initial value:\n";
          MFP::dump(*Logger.getAsLLVMStream(), 1, SuccessorResults.InValue);
          Logger << DoLog;
        }

        // Enqueue in the list
        Worklist.insert({ LabelPriority.at(Successor), Successor });
      } else {
        revng_log(Logger, "Ignoring");
      }
    }

    ++IterationIndex;
  }

  return AnalysisResult;
}

#if 0
template<MonotoneFrameworkInstance MFI,
         typename GT = llvm::GraphTraits<typename MFI::GraphType>>
MFIResultMap<MFI>
getMaximalFixedPoint(const MFI &Instance,
                     typename MFI::GraphType Flow,
                     typename MFI::LatticeElement InitialValue,
                     typename MFI::LatticeElement ExtremalValue,
                     const std::vector<typename MFI::Label> &ExtremalLabels,
                     Logger &Logger = NullLogger) {
  using Label = typename MFI::Label;
  std::vector<Label> InitialNodes(ExtremalLabels);

  // Handle the special case that the graph has a single entry node
  if (GT::getEntryNode(Flow) != typename GT::NodeRef{})
    InitialNodes.push_back(GT::getEntryNode(Flow));

  // Start visits for nodes that we still haven't visited prioritizing extremal
  // nodes
  for (Label Node :
       llvm::make_range(GT::nodes_begin(Flow), GT::nodes_end(Flow))) {
    InitialNodes.push_back(Node);
  }

  return getMaximalFixedPoint<MFI, GT>(Instance,
                                       Flow,
                                       InitialValue,
                                       ExtremalValue,
                                       ExtremalLabels,
                                       InitialNodes,
                                       Logger);
}
#endif

template<MonotoneFrameworkInstance MFIType>
struct MFPConfiguration {
  const MFIType *Instance = nullptr;
  typename MFIType::GraphType Flow;
  const typename MFIType::LatticeElement *Bottom = nullptr;
  const typename MFIType::LatticeElement *ExtremalValue = nullptr;
  const std::vector<typename MFIType::Label> *ExtremalLabels = nullptr;
  const std::vector<typename MFIType::Label> *EntryLabels = nullptr;
  Logger *Logger = nullptr;
};

template<MonotoneFrameworkInstance MFIType,
         typename GT = llvm::GraphTraits<typename MFIType::GraphType>>
MFIResultMap<MFIType>
getMaximalFixedPoint(MFPConfiguration<MFIType> Configuration,
                     typename MFIType::ExtraStateType *ExtraState = nullptr) {
  std::optional<MFIType> DefaultInstance;
  if constexpr (std::is_default_constructible_v<MFIType>) {
    if (Configuration.Instance == nullptr)
      Configuration.Instance = &DefaultInstance.emplace();
  } else {
    revng_assert(Configuration.Instance != nullptr);
  }

  using LatticElement = typename MFIType::LatticeElement;
  std::optional<LatticElement> DefaultBottom;
  std::optional<typename MFIType::LatticeElement> DefaultExtremalValue;
  if constexpr (std::is_default_constructible_v<LatticElement>) {

    if (Configuration.Bottom == nullptr)
      Configuration.Bottom = &DefaultBottom.emplace();

    if (Configuration.ExtremalValue == nullptr)
      Configuration.ExtremalValue = &DefaultExtremalValue.emplace();

  } else {
    revng_assert(Configuration.Bottom != nullptr);
    revng_assert(Configuration.ExtremalValue != nullptr);
  }

  std::vector<typename MFIType::Label> DefaultExtremalLabels;
  if (Configuration.ExtremalLabels == nullptr)
    Configuration.ExtremalLabels = &DefaultExtremalLabels;

  std::vector<typename MFIType::Label> DefaultEntryNodes;
  if (Configuration.EntryLabels == nullptr) {
    auto Entry = GT::getEntryNode(Configuration.Flow);

    // If we have an entry point, use it, otherwise add all the nodes
    if (Entry != typename GT::NodeRef{}) {
      DefaultEntryNodes.push_back(Entry);
    }
    // WIP: else
    {
      if constexpr (HasNodeRange<GT>) {
        auto Flow = Configuration.Flow;
        for (auto Node :
             llvm::make_range(GT::nodes_begin(Flow), GT::nodes_end(Flow))) {
          DefaultEntryNodes.push_back(Node);
        }
      } else {
        revng_abort();
      }
    }

    Configuration.EntryLabels = &DefaultEntryNodes;
  }

  if (Configuration.Logger == nullptr)
    Configuration.Logger = &NullLogger;

  // If the caller didn't supply an ExtraState, default-construct one. For
  // ExtraStateType == NoExtraState this is a no-op; for a real ExtraState an
  // empty (no interesting points) instance behaves like NoExtraState.
  typename MFIType::ExtraStateType DefaultExtraState{};
  typename MFIType::ExtraStateType &EffectiveExtraState = ExtraState ?
                                                            *ExtraState :
                                                            DefaultExtraState;

  return getMaximalFixedPointImpl<MFIType, GT>(*Configuration.Instance,
                                               Configuration.Flow,
                                               *Configuration.Bottom,
                                               *Configuration.ExtremalValue,
                                               *Configuration.ExtremalLabels,
                                               *Configuration.EntryLabels,
                                               EffectiveExtraState,
                                               *Configuration.Logger);
}

} // namespace MFP

namespace llvm {
template<>
struct DenseMapInfo<MFP::Position> {
  static MFP::Position getEmptyKey() {
    return static_cast<MFP::Position>(static_cast<unsigned char>(-1));
  }
  static MFP::Position getTombstoneKey() {
    return static_cast<MFP::Position>(static_cast<unsigned char>(-2));
  }
  static unsigned getHashValue(const MFP::Position &P) {
    return static_cast<unsigned char>(P);
  }
  static bool isEqual(const MFP::Position &LHS, const MFP::Position &RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm
