/// \file Processing.cpp
/// \brief A collection of helper functions to improve the quality of the
///        model/make it valid

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

using namespace llvm;

namespace model {

unsigned dropTypesDependingOnTypes(TupleTree<model::Binary> &Model,
                                   const std::set<const model::Type *> &Types) {
  struct TypeNode {
    const model::Type *T;
  };

  using Graph = GenericGraph<ForwardNode<TypeNode>>;

  Graph ReverseDependencyGraph;

  // Create nodes in reverse dependency graph
  std::map<const model::Type *, ForwardNode<TypeNode> *> TypeToNode;
  for (UpcastablePointer<model::Type> &T : Model->Types)
    TypeToNode[T.get()] = ReverseDependencyGraph.addNode(TypeNode{ T.get() });

  auto RegisterDependency = [&](UpcastablePointer<model::Type> &T,
                                const model::QualifiedType &QT) {
    auto *DependantType = QT.UnqualifiedType.get();
    TypeToNode.at(DependantType)->addSuccessor(TypeToNode.at(T.get()));
  };

  // Populate the graph
  for (UpcastablePointer<model::Type> &T : Model->Types) {

    // Ignore dependencies of
    if (Types.count(T.get()) != 0)
      continue;

    if (auto *Primitive = dyn_cast<model::PrimitiveType>(T.get())) {
      // Nothing to do here
    } else if (auto *Struct = dyn_cast<model::StructType>(T.get())) {
      for (const model::StructField &Field : Struct->Fields)
        RegisterDependency(T, Field.Type);
    } else if (auto *Union = dyn_cast<model::UnionType>(T.get())) {
      for (const model::UnionField &Field : Union->Fields)
        RegisterDependency(T, Field.Type);
    } else if (auto *Enum = dyn_cast<model::EnumType>(T.get())) {
      RegisterDependency(T, model::QualifiedType(Enum->UnderlyingType, {}));
    } else if (auto *Typedef = dyn_cast<model::TypedefType>(T.get())) {
      RegisterDependency(T, Typedef->UnderlyingType);
    } else if (auto *RFT = dyn_cast<model::RawFunctionType>(T.get())) {
      for (const model::NamedTypedRegister &Argument : RFT->Arguments)
        RegisterDependency(T, Argument.Type);
      for (const model::TypedRegister &RV : RFT->ReturnValues)
        RegisterDependency(T, RV.Type);
    } else if (auto *CAFT = dyn_cast<model::CABIFunctionType>(T.get())) {
      for (const model::Argument &Argument : CAFT->Arguments)
        RegisterDependency(T, Argument.Type);
      RegisterDependency(T, CAFT->ReturnType);
    } else {
      revng_abort();
    }
  }

  // Prepare for deletion all the nodes reachable from Types
  std::set<const model::Type *> ToDelete;
  for (const model::Type *Type : Types) {
    for (const auto *Node : depth_first(TypeToNode.at(Type))) {
      ToDelete.insert(Node->T);
    }
  }

  // Purge dynamic functions depending on Types
  auto Begin = Model->ImportedDynamicFunctions.begin();
  for (auto It = Begin; It != Model->ImportedDynamicFunctions.end(); /**/) {
    if (ToDelete.count(It->Prototype.get()) == 0) {
      ++It;
    } else {
      It = Model->ImportedDynamicFunctions.erase(It);
    }
  }

  // Purge types depending on unresolved Types
  for (auto It = Model->Types.begin(); It != Model->Types.end();) {
    if (ToDelete.count(It->get()) != 0)
      It = Model->Types.erase(It);
    else
      ++It;
  }

  return ToDelete.size();
}

void deduplicateNames(TupleTree<model::Binary> &Model) {
  // TODO: collapse uint8_t typedefs into the primitive type

  std::set<std::string> UsedNames;

  for (auto &Type : Model->Types) {
    model::Type *T = Type.get();
    if (isa<model::PrimitiveType>(T)) {
      UsedNames.insert(T->name().str().str());
    }
  }

  for (auto &Type : Model->Types) {
    model::Type *T = Type.get();
    if (isa<model::PrimitiveType>(T))
      continue;

    std::string Name = T->name().str().str();
    while (UsedNames.count(Name) != 0) {
      Name += "_";
    }

    // Rename
    upcast(T, [&Name](auto &Upcasted) {
      using UpcastedType = std::remove_cvref_t<decltype(Upcasted)>;
      if constexpr (not std::is_same_v<model::PrimitiveType, UpcastedType>) {
        Upcasted.CustomName = Name;
      } else {
        revng_abort();
      }
    });

    // Record new name
    UsedNames.insert(Name);
  }
}

// WIP: move to my own file
class TypeSystemDeduplicator {
private:
  struct TypeNode {
    model::Type *T;
  };
  using Node = ForwardNode<TypeNode>;
  using Graph = GenericGraph<Node>;

private:
  std::vector<Type *> Types;
  EquivalenceClasses<Type *> StrongEquivalence;
  EquivalenceClasses<Type *> WeakEquivalence;
  Graph TypeGraph;
  std::map<Type *, Node *> TypeToNode;
  std::vector<Type *> VisitOrder;

private:
  TypeSystemDeduplicator(TupleTree<model::Binary> &Model) {
    for (auto &T : Model->Types)
      Types.push_back(&*T);
  }

public:
  static EquivalenceClasses<Type *> run(TupleTree<model::Binary> &Model) {
    TypeSystemDeduplicator Helper(Model);
    Helper.computeWeakEquivalenceClasses();
    Helper.createTypeGraph();
    Helper.computeVisitOrder();
    Helper.computeStrongEquivalenceClasses();
    return std::move(Helper.StrongEquivalence);
  }

private:
  void computeWeakEquivalenceClasses() {
    // Sort types by the key (the name)
    llvm::sort(Types, [](Type *Left, Type *Right) {
      return Left->name() < Right->name();
    });

    std::string LastName;
    auto GroupStart = Types.begin();
    auto End = Types.end();
    while (GroupStart != End) {
      // Find group end
      StringRef GroupName = (*GroupStart)->name();
      auto GroupEnd = GroupStart;
      ++GroupEnd;
      while (GroupEnd != End and (*GroupEnd)->name() == GroupName)
        ++GroupEnd;

      for (auto It = GroupStart; It != GroupEnd; ++It) {
        Type *Left = *It;
        auto GroupNext = It;
        ++GroupNext;
        for (Type *Right : make_range(GroupNext, GroupEnd)) {
          if (not WeakEquivalence.isEquivalent(Left, Right)
              and localCompare(Left, Right)) {
            WeakEquivalence.unionSets(Left, Right);
          }
        }
      }

      GroupStart = GroupEnd;
    }
  }

  /// Create a graph of the non-local parts (including pointers)
  void createTypeGraph() {
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

  void computeVisitOrder() {
    Node *Entry = TypeGraph.addNode(TypeNode{ nullptr });
    for (Node *N : TypeGraph.nodes())
      Entry->addSuccessor(N);

    std::set<Type *> Inserted;
    for (Node *N : skip(1, post_order(Entry))) {
      Type *Leader = WeakEquivalence.getLeaderValue(N->T);
      if (Inserted.count(Leader) == 0) {
        VisitOrder.push_back(Leader);
        Inserted.insert(Leader);
      }
    }

    TypeGraph.removeNode(Entry);
  }

  void computeStrongEquivalenceClasses() {
    for (Type *Leader : VisitOrder) {
      auto LeaderIt = WeakEquivalence.findValue(Leader);
      revng_assert(LeaderIt->isLeader());

      auto GroupStart = WeakEquivalence.member_begin(LeaderIt);
      auto GroupEnd = WeakEquivalence.member_begin(LeaderIt);
      for (auto It = GroupStart; It != GroupEnd; ++It) {
        Type *Left = *It;
        auto GroupNext = It;
        ++GroupNext;
        for (Type *Right : make_range(GroupNext, GroupEnd)) {
          if (not StrongEquivalence.isEquivalent(Left, Right)) {
            deepCompare(Left, Right);
          }
        }
      }
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

  bool localCompare(const StructType *Left, const StructType *Right) {
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

  bool localCompare(const Type *Left, const Type *Right) {
    // TODO: transform me in model::Type::localCompare

    // Do they have the same kind?
    if (Left->Kind != Right->Kind)
      return false;

    revng_assert(Left->name() == Right->name());

    auto UpcastedCompare = [this, Right](auto &LeftUpcasted) -> bool {
      using UpcastedType = std::remove_cvref_t<decltype(LeftUpcasted)>;
      auto &RightUpcasted = *cast<UpcastedType>(Right);
      return localCompare(&LeftUpcasted, &RightUpcasted);
    };
    return upcast(Left, UpcastedCompare, false);
  }

  bool deepCompare(Type *LeftType, Type *RightType) {
    Node *Left = TypeToNode.at(LeftType);
    Node *Right = TypeToNode.at(RightType);

    // Create a bidirectional map associating left and right nodes
    std::map<Node *, Node *> LeftToRight;
    std::map<Node *, Node *> RightToLeft;

    // Initialize the map with the two considered nodes
    LeftToRight[Left] = Right;
    RightToLeft[Right] = Left;

    // Do a dfs
    df_iterator_default_set<Node *> Visited;
    for (Node *LeftNode : depth_first_ext(Left, Visited)) {
      auto RightIt = LeftToRight.find(LeftNode);
      if (RightIt == LeftToRight.end())
        return false;
      Node *RightNode = RightIt->second;

      // Zip out edges of the node pair: consider the destinations.
      for (auto [LeftSuccessor, RightSuccessor] :
           zip(LeftNode->successors(), RightNode->successors())) {

        // If any of them is in the associating map, the other needs to match.
        // If it doesn't, the two nodes are not equivalent.
        auto LeftToRightIt = LeftToRight.find(LeftSuccessor);
        if (LeftToRightIt != LeftToRight.end()) {
          if (LeftToRightIt->second != Right) {
            return false;
          } else {
            revng_assert(RightToLeft.at(Right) == Left);
            continue;
          }
        }

        auto RightToLeftIt = RightToLeft.find(RightSuccessor);
        if (RightToLeftIt != RightToLeft.end()) {
          if (RightToLeftIt->second != Left) {
            return false;
          } else {
            revng_assert(LeftToRight.at(Left) == Right);
            continue;
          }
        }

        // Check if they are strongly/weakly equivalent.
        // If so, insert them in the associating map and proceed.
        if (StrongEquivalence.isEquivalent(Right->T, Left->T)) {
          // Strong equivalence, no need to visit me
          Visited.insert(Left);
          LeftToRight[Left] = Right;
          RightToLeft[Right] = Left;
        } else if (WeakEquivalence.isEquivalent(Right->T, Left->T)) {
          LeftToRight[Left] = Right;
          RightToLeft[Right] = Left;
        } else {
          // Otherwise, the nodes are not equivalent
          return false;
        }
      }
    }

    // All those in the map are strongly equivalent
    for (auto [LeftNode, RightMode] : LeftToRight)
      StrongEquivalence.unionSets(Left->T, Right->T);

    return true;
  }
};

void deduplicateEquivalentTypes(TupleTree<model::Binary> &Model) {
  auto EquivalentTypes = TypeSystemDeduplicator::run(Model);

  std::map<TypePath, TypePath> Replacements;
  for (auto LeaderIt = EquivalentTypes.begin(), End = EquivalentTypes.end();
       LeaderIt != End;
       ++LeaderIt) {

    if (!LeaderIt->isLeader())
      continue;

    Type *Leader = LeaderIt->getData();
    model::TypePath LeaderPath = Model->getTypePath(Leader);

    for (Type *ToCollapse : make_range(EquivalentTypes.member_begin(LeaderIt),
                                       EquivalentTypes.member_end())) {
      // WIP: drop me once tested
      revng_assert(ToCollapse != Leader);
      Replacements[Model->getTypePath(ToCollapse)] = LeaderPath;
    }
  }

  Model.replaceReferences(Replacements);
}

} // namespace model
