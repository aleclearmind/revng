/// \file TypesDeduplication.cpp
/// \brief Implementation of deduplication of identical types.

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/EquivalenceClasses.h"
#include "llvm/ADT/PostOrderIterator.h"

#include "revng/ADT/GenericGraph.h"
#include "revng/ADT/STLExtras.h"
#include "revng/Model/Processing.h"
#include "revng/Support/Debug.h"

#include "TypesDeduplication.h"

using namespace llvm;
using namespace model;

static Logger<> Log("model-types-deduplication");

inline void
compareAll(SmallVector<model::Type *> &ToTest,
           std::function<bool(model::Type *, model::Type *)> Compare) {
  for (auto It = ToTest.begin(), End = ToTest.end(); It != End; ++It) {
    model::Type *Left = *It;

    auto IsDuplicate = [Compare, Left](model::Type *Right) {
      return Compare(Left, Right);
    };

    // Compare with all those after It and delete them if match
    End = ToTest.erase(std::remove_if(It + 1, End, IsDuplicate), End);
  }
}

// WIP: move
static StringRef typeCustomName(const model::Type *T) {
  return upcast(
    T,
    [](auto &Upcasted) -> StringRef {
      using UpcastedType = std::decay_t<decltype(Upcasted)>;
      if constexpr (std::is_same_v<UpcastedType, model::PrimitiveType>) {
        return "";
      } else {
        return Upcasted.CustomName;
      }
    },
    StringRef(""));
}

class TypeSystemDeduplicator {
private:
  struct TypeNode {
    model::Type *T;
  };
  using Node = ForwardNode<TypeNode>;
  using Graph = GenericGraph<Node>;

private:
  std::vector<model::Type *> Types;
  EquivalenceClasses<model::Type *> StrongEquivalence;
  EquivalenceClasses<model::Type *> WeakEquivalence;
  Graph TypeGraph;
  std::map<model::Type *, Node *> TypeToNode;
  std::vector<model::Type *> VisitOrder;

private:
  TypeSystemDeduplicator(TupleTree<model::Binary> &Model) {
    for (auto &T : Model->Types)
      Types.push_back(&*T);
  }

public:
  static EquivalenceClasses<model::Type *>
  run(TupleTree<model::Binary> &Model) {
    TypeSystemDeduplicator Helper(Model);
    Helper.computeWeakEquivalenceClasses();
    Helper.createTypeGraph();
    Helper.computeVisitOrder();
    Helper.computeStrongEquivalenceClasses();
    return std::move(Helper.StrongEquivalence);
  }

private:
  void computeWeakEquivalenceClasses() {
    revng_log(Log, "Computing weak equivalence classes");
    LoggerIndent Indent(Log);

    auto ComputeKey = [](model::Type *T) {
      return std::pair{ typeCustomName(T), T->Kind };
    };

    // Sort types by the key (the name)
    llvm::sort(Types, [&ComputeKey](model::Type *Left, model::Type *Right) {
      return ComputeKey(Left) < ComputeKey(Right);
    });

    auto GroupStart = Types.begin();
    auto End = Types.end();
    while (GroupStart != End) {
      // Find group end and collect types
      auto GroupKey = ComputeKey(*GroupStart);
      std::string GroupName = (TypeKind::getName(GroupKey.second) + " "
                               + GroupKey.first.str())
                                .str();
      revng_log(Log, "Considering \"" << GroupName << "\"");
      LoggerIndent Indent2(Log);

      auto GroupEnd = GroupStart;
      SmallVector<model::Type *> ToTest;
      do {
        ToTest.push_back(*GroupEnd);
        ++GroupEnd;
      } while (GroupEnd != End and ComputeKey(*GroupEnd) == GroupKey);

      if (not GroupKey.first.empty()) {

        auto Compare = [this](model::Type *Left, model::Type *Right) -> bool {
          revng_assert(Left != Right
                       and not WeakEquivalence.isEquivalent(Left, Right));
          if (localCompare(Left, Right)) {
            revng_log(Log,
                      Left->ID << " and " << Right->ID
                               << " are weakly equivalent");

            // Record as weakly equivalent
            WeakEquivalence.unionSets(Left, Right);

            return true;
          } else {
            // This is kind of unusual
            if (Log.isEnabled()) {
              Log << "The following types have same kind and name but are "
                     "locally different.";
              model::Type *M = Left;
              upcast(M, [](auto &U) { serialize(Log, U); });
              upcast(Right, [](auto &U) { serialize(Log, U); });
              Log << DoLog;
            }

            return false;
          }
        };

        compareAll(ToTest, Compare);

        revng_log(Log,
                  GroupName << " has " << ToTest.size()
                            << " non-weakly equivalent types");
      }

      GroupStart = GroupEnd;
    }
  }

  /// Create a graph of the non-local parts (including pointers)
  void createTypeGraph() {
    revng_log(Log, "Building the type graph");

    // Create nodes
    for (model::Type *T : Types)
      TypeToNode[T] = TypeGraph.addNode(TypeNode{ T });

    // WIP: we need to factor this code out
    for (model::Type *T : Types) {
      if (auto *Primitive = dyn_cast<model::PrimitiveType>(T)) {
        // Nothing to do here
      } else if (auto *Struct = dyn_cast<model::StructType>(T)) {
        for (model::StructField &Field : Struct->Fields)
          addEdge(T, Field.Type);
      } else if (auto *Union = dyn_cast<model::UnionType>(T)) {
        for (model::UnionField &Field : Union->Fields)
          addEdge(T, Field.Type);
      } else if (auto *Enum = dyn_cast<model::EnumType>(T)) {
        model::QualifiedType QT{ Enum->UnderlyingType };
        addEdge(T, QT);
      } else if (auto *Typedef = dyn_cast<model::TypedefType>(T)) {
        addEdge(T, Typedef->UnderlyingType);
      } else if (auto *RFT = dyn_cast<model::RawFunctionType>(T)) {
        for (model::NamedTypedRegister &Argument : RFT->Arguments)
          addEdge(T, Argument.Type);
        for (model::TypedRegister &RV : RFT->ReturnValues)
          addEdge(T, RV.Type);
      } else if (auto *CAFT = dyn_cast<model::CABIFunctionType>(T)) {
        for (model::Argument &Argument : CAFT->Arguments)
          addEdge(T, Argument.Type);
        addEdge(T, CAFT->ReturnType);
      } else {
        revng_abort();
      }
    }
  }

  /// Compute a visit order: post order in the leaders of WeakEquivalence
  void computeVisitOrder() {
    revng_log(Log, "Computing visit order");

    Node *Entry = TypeGraph.addNode(TypeNode{ nullptr });
    for (Node *N : TypeGraph.nodes())
      Entry->addSuccessor(N);

    std::set<model::Type *> Inserted;
    for (Node *N : post_order(Entry)) {

      auto LeaderIt = WeakEquivalence.findLeader(N->T);
      if (LeaderIt != WeakEquivalence.member_end()
          and Inserted.count(*LeaderIt) == 0) {
        VisitOrder.push_back(*LeaderIt);
        Inserted.insert(*LeaderIt);
      }
    }

    TypeGraph.removeNode(Entry);
  }

  void computeStrongEquivalenceClasses() {
    revng_log(Log, "Computing strong equivalence classes");
    LoggerIndent Indent(Log);

    for (model::Type *Leader : VisitOrder) {
      revng_log(Log, "Considering " << typeCustomName(Leader).str());
      LoggerIndent Indent2(Log);

      revng_assert(not typeCustomName(Leader).empty());
      auto LeaderIt = WeakEquivalence.findValue(Leader);
      revng_assert(LeaderIt->isLeader());

      SmallVector<model::Type *> ToTest;
      std::copy(WeakEquivalence.member_begin(LeaderIt),
                WeakEquivalence.member_end(),
                std::back_inserter(ToTest));

      auto Compare = [this](model::Type *Left, model::Type *Right) {
        if (Left == Right or StrongEquivalence.isEquivalent(Left, Right))
          return true;

        revng_log(Log, "Comparing " << Left->ID << " and " << Right->ID);
        LoggerIndent Indent(Log);

        bool Result = deepCompare(Left, Right);

        revng_log(Log, "Comparison result: " << (Result ? "true" : "false"));

        return Result;
      };

      compareAll(ToTest, Compare);

    }
  }

private:
  void addEdge(model::Type *T, model::QualifiedType &QT) {
    auto *DependantType = QT.UnqualifiedType.get();
    TypeToNode.at(T)->addSuccessor(TypeToNode.at(DependantType));
  }

  bool localCompare(const PrimitiveType *Left, const PrimitiveType *Right) {
    return (Left->PrimitiveKind == Right->PrimitiveKind
            and Left->Size == Right->Size);
  }

  bool
  localCompare(const model::StructType *Left, const model::StructType *Right) {
    return (Left->Fields.size() == Right->Fields.size()
            and Left->Size == Right->Size);
  }

  bool localCompare(const UnionType *Left, const UnionType *Right) {
    return Left->Fields.size() == Right->Fields.size();
  }

  bool localCompare(const EnumType *Left, const EnumType *Right) {
    return Left->Entries == Right->Entries;
  }

  bool localCompare(const TypedefType *Left, const TypedefType *Right) {
    return true;
  }

  bool localCompare(const RawFunctionType *Left, const RawFunctionType *Right) {
    if (not(Left->PreservedRegisters == Right->PreservedRegisters
            and Left->FinalStackOffset == Right->FinalStackOffset
            and Left->Arguments.size() == Right->Arguments.size()
            and Left->ReturnValues.size() == Right->ReturnValues.size()))
      return false;

    for (const auto &[LeftArgument, RightArgument] :
         zip(Left->Arguments, Right->Arguments))
      if (not(LeftArgument.CustomName == RightArgument.CustomName
              and LeftArgument.Location == RightArgument.Location))
        return false;

    for (const auto &[LeftReturnValue, RightReturnValue] :
         zip(Left->ReturnValues, Right->ReturnValues))
      if (LeftReturnValue.Location != RightReturnValue.Location)
        return false;

    return true;
  }

  bool
  localCompare(const CABIFunctionType *Left, const CABIFunctionType *Right) {
    if (Left->ABI != Right->ABI)
      return false;

    for (const auto &[LeftArgument, RightArgument] :
         zip(Left->Arguments, Right->Arguments))
      if (not(LeftArgument.Index == RightArgument.Index
              and LeftArgument.CustomName == RightArgument.CustomName))
        return false;

    return true;
  }

  bool localCompare(const model::Type *Left, const model::Type *Right) {
    // TODO: transform me in model::Type::localCompare

    // Do they have the same kind?
    if (Left->Kind != Right->Kind)
      return false;

    if (typeCustomName(Left) != typeCustomName(Right))
      return false;

    auto UpcastedCompare = [this, Right](auto &LeftUpcasted) -> bool {
      using UpcastedType = std::remove_cvref_t<decltype(LeftUpcasted)>;
      auto &RightUpcasted = *cast<UpcastedType>(Right);
      return localCompare(&LeftUpcasted, &RightUpcasted);
    };
    return upcast(Left, UpcastedCompare, false);
  }

  bool deepCompare(model::Type *LeftType, model::Type *RightType) {
    Node *Left = TypeToNode.at(LeftType);
    Node *Right = TypeToNode.at(RightType);

    // Create a bidirectional map associating left and right nodes
    std::map<Node *, Node *> LeftToRight;
    std::map<Node *, Node *> RightToLeft;

    // Initialize the map with the two considered nodes
    LeftToRight[Left] = Right;
    RightToLeft[Right] = Left;

    // Perform a depth-first visit from here.
    // Note: we will stop the visit on successors that are already known to be
    // strongly equivalent.
    df_iterator_default_set<Node *> Visited;

    for (Node *LeftNode : depth_first_ext(Left, Visited)) {
      revng_log(Log, "Visiting " << LeftNode->T->ID);
      LoggerIndent Indent2(Log);

      auto RightIt = LeftToRight.find(LeftNode);
      if (RightIt == LeftToRight.end()) {
        revng_log(Log, "Mismatch");
        return false;
      }
      Node *RightNode = RightIt->second;

      // Zip out edges of the node pair: consider the destinations.
      for (auto [LeftSuccessor, RightSuccessor] :
           zip(LeftNode->successors(), RightNode->successors())) {
        revng_log(Log, "Visiting successor " << LeftSuccessor->T->ID);
        if (not compareSuccessor(LeftToRight,
                                 RightToLeft,
                                 Visited,
                                 LeftSuccessor,
                                 RightSuccessor))
          return false;
      }
    }

    // All those in the map are strongly equivalent
    for (auto [LeftNode, RightMode] : LeftToRight)
      StrongEquivalence.unionSets(Left->T, Right->T);

    return true;
  }

  /// Support function for deepCompare
  bool compareSuccessor(std::map<Node *, Node *> &LeftToRight,
                        std::map<Node *, Node *> &RightToLeft,
                        df_iterator_default_set<Node *> &Visited,
                        Node *Left,
                        Node *Right) {
    // If any of them is in the associating map, the other needs to match.
    // If it doesn't, the two nodes are not equivalent.
    auto LeftToRightIt = LeftToRight.find(Left);
    if (LeftToRightIt != LeftToRight.end()) {
      if (LeftToRightIt->second != Right) {
        revng_log(Log,
                  "We were expecting " << LeftToRightIt->second->T->ID
                                       << " but we got " << Right->T->ID);
        return false;
      } else {
        revng_assert(RightToLeft.at(Right) == Left);
        return true;
      }
    }

    auto RightToLeftIt = RightToLeft.find(Right);
    if (RightToLeftIt != RightToLeft.end()) {
      if (RightToLeftIt->second != Left) {
        revng_log(Log,
                  "We were expecting " << RightToLeftIt->second->T->ID
                                       << " but we got " << Left->T->ID);
        return false;
      } else {
        revng_assert(LeftToRight.at(Left) == Right);
        return true;
      }
    }

    // Record the match before actually verifying it. In any case,
    // even if we fail we'll discard this.
    LeftToRight[Left] = Right;
    RightToLeft[Right] = Left;

    // Verify the match
    if (Right->T == Left->T
        or StrongEquivalence.isEquivalent(Right->T, Left->T)) {
      // Strong equivalence, no need to visit me
      Visited.insert(Left);
      return true;
    } else if (WeakEquivalence.isEquivalent(Right->T, Left->T)
               or (typeCustomName(Left->T).empty()
                   and typeCustomName(Right->T).empty()
                   and localCompare(Left->T, Right->T))) {
      // Weak equivalence
      return true;
    } else {
      // Otherwise, the nodes are not equivalent
      revng_assert(not localCompare(Left->T, Right->T));
      revng_log(Log,
                Left->T->ID << " and " << Right->T->ID
                            << " are locally different");
      return false;
    }
  }
};

void deduplicateEquivalentTypesImpl(TupleTree<model::Binary> &Model) {
  revng_log(Log, "Deduplicating the model");
  LoggerIndent Indent(Log);

  auto EquivalentTypes = TypeSystemDeduplicator::run(Model);

  std::set<model::Type *> ToErase;
  std::map<TypePath, TypePath> Replacements;
  for (auto LeaderIt = EquivalentTypes.begin(), End = EquivalentTypes.end();
       LeaderIt != End;
       ++LeaderIt) {

    if (!LeaderIt->isLeader())
      continue;

    model::Type *Leader = LeaderIt->getData();
    model::TypePath LeaderPath = Model->getTypePath(Leader);

    for (model::Type *ToCollapse :
         make_range(++EquivalentTypes.member_begin(LeaderIt),
                    EquivalentTypes.member_end())) {
      Replacements[Model->getTypePath(ToCollapse)] = LeaderPath;
      ToErase.insert(ToCollapse);
    }
  }

  revng_log(Log, "Dropping " << ToErase.size() << " duplicated types");

  // Update references
  Model.replaceReferences(Replacements);

  // Actually drop the types
  llvm::erase_if(Model->Types, [&ToErase](UpcastablePointer<model::Type> &P) {
    return ToErase.count(P.get()) != 0;
  });
}
