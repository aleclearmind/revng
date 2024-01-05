/// \file EFA4.cpp

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <cstdint>
#include <iterator>

#include "llvm/ADT/BitVector.h"
#include "llvm/IR/Function.h"

#include "revng/ADT/GenericGraph.h"
#include "revng/Model/Generated/Early/Register.h"
#include "revng/Model/Register.h"
#include "revng/MFP/MFP.h"

using namespace llvm;

namespace efa {

namespace OperationType {
enum Values : uint8_t {
    Invalid,
    Read,
    Write,
    Clobber
};
}

class Operation {
public:
  // WIP: collapse in a single byte
  OperationType::Values Type = OperationType::Invalid;
  uint8_t Target = model::Register::Invalid;
};

static_assert(sizeof(Operation) == 2);

struct Block {
public:
  using OperationsVector = SmallVector<Operation, 8>;
  using iterator = OperationsVector::iterator;

public:
  OperationsVector Operations;

public:
  Block() = default;

public:
  auto begin() const { return Operations.begin(); }
  auto end() const { return Operations.end(); }

  auto begin() { return Operations.begin(); }
  auto end() { return Operations.end(); }

  auto rbegin() const { return Operations.rbegin(); }
  auto rend() const { return Operations.rend(); }

  auto rbegin() { return Operations.rbegin(); }
  auto rend() { return Operations.rend(); }

};

using BlockNode = BidirectionalNode<Block>;

// WIP: implement DOTGraphTraits
struct Function : GenericGraph<BlockNode> {
private:
  // WIP: DenseMap<model::Register::Values, uint8_t> RegisterToIndex;

public:
  static Function fromLLVMFunction(llvm::Function &F) {
    // WIP
    revng_abort();
  }

public:
  uint8_t registersCount() const {
    // WIP
    revng_abort();
  }

public:
  // WIP: do we actually need this?
  BlockNode &splitAt(BlockNode &Original, BlockNode::iterator SplitBefore) {
    BlockNode *NewNode = addNode();

    // Copy successor of the original node into the new node
    for (BlockNode *Successor : Original.successors())
      NewNode->addSuccessor(Successor);

    // Clear the successors of the original node
    Original.clearSuccessors();

    // Add the new node as a successor of the original node
    Original.addSuccessor(NewNode);

    // Move instructions over from the original node to the new node
    std::copy(SplitBefore, Original.end(), std::back_inserter(NewNode->Operations));
    Original.Operations.erase(SplitBefore, Original.end());

    return *NewNode;
  }

};

class LivenessAnalysis {
private:
  using Set = BitVector;
  using RegisterSet = Set;

public:
  using LatticeElement = Set;
  using GraphType = Inverse<const BlockNode *>;
  using Label = const BlockNode *;

public:
  Set combineValues(const Set &LHS, const Set &RHS) const {
    Set Result = LHS;
    Result |= RHS;
    return Result;
  }

  bool isLessOrEqual(const Set &LHS, const Set &RHS) const {
    // RHS must contain or be equal to LHS
    Set Intersection = RHS;
    Intersection &= LHS;
    return Intersection == LHS;
  }

  RegisterSet applyTransferFunction(const BlockNode *Block,
                                    const RegisterSet &InitialState) const {
    RegisterSet Result = InitialState;

    for (const Operation &Operation:
         make_range(Block->rbegin(), Block->rend())) {

      switch (Operation.Type) {
      case OperationType::Read:
        Result.set(Operation.Target);
        break;

      case OperationType::Write:
      case OperationType::Clobber:
        Result.reset(Operation.Target);
        break;

      case OperationType::Invalid:
        revng_abort();
        break;

      }
    }

    return Result;

  }

};

static_assert(MFP::MonotoneFrameworkInstance<LivenessAnalysis>);

class ReachingDefinitions {
private:
  DenseMap<const Operation *, uint8_t> WriteToIndex;

public:
  ReachingDefinitions(efa::Function &F) {
    // Populate WriteToIndex
    SmallVector<int> RegisterWriteIndex(F.registersCount(), 0);
    for (Block *Block : F.nodes()) {
      for (Operation &Operation : Block->Operations) {
        WriteToIndex[&Operation] = RegisterWriteIndex[Operation.Target];
        RegisterWriteIndex[Operation.Target] += 1;
      }
    }
  }

public:
  struct RegisterWriters {
    BitVector Reaching;
    BitVector Read;

    bool operator==(const RegisterWriters &Other) const = default;

    RegisterWriters &operator|=(const RegisterWriters &Other) {
      Reaching |= Other.Reaching;
      Read |= Other.Read;
      return *this;
    }

    RegisterWriters &operator&=(const RegisterWriters &Other) {
      Reaching &= Other.Reaching;
      Read &= Other.Read;
      return *this;
    }
  };

  using WritersSet = SmallVector<RegisterWriters, 16>;
  using LatticeElement = WritersSet;
  using GraphType = BlockNode *;
  using Label = BlockNode *;

public:
  WritersSet combineValues(const WritersSet &LHS, const WritersSet &RHS) const {
    WritersSet Result = LHS;
    for (const auto &[ResultEntry, RHSEntry] : zip(Result, RHS))
      ResultEntry |= RHSEntry;
    return Result;
  }

  bool isLessOrEqual(const WritersSet &LHS, const WritersSet &RHS) const {
    for (const auto &[LHSEntry, RHSEntry] : zip(LHS, RHS)) {
      auto Intersection = LHSEntry;
      Intersection &= RHSEntry;
      if (Intersection != LHSEntry)
        return false;
    }

    return true;
  }

  WritersSet applyTransferFunction(const Block *Block,
                                   const WritersSet &InitialState) const {
    WritersSet Result = InitialState;

    for (const Operation &Operation : *Block) {

      switch (Operation.Type) {
      case OperationType::Write:
      case OperationType::Clobber: {
        RegisterWriters &Writes = Result[Operation.Target];

        Writes.Reaching.clear();

        if (Operation.Type == OperationType::Write)
          Writes.Reaching.set(WriteToIndex.find(&Operation)->second);

      } break;
      case OperationType::Read: {
        RegisterWriters &Writes = Result[Operation.Target];
        Writes.Read |= Writes.Reaching;
      } break;
      case OperationType::Invalid:
        revng_abort();
        break;
      }

    }

    return Result;
  }

};

static_assert(MFP::MonotoneFrameworkInstance<ReachingDefinitions>);

inline void asd() {
  Function Graph;
}

}
