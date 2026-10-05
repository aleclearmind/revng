#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <functional>

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/ValueLattice.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/SCCPSolver.h"

namespace TypeShrinking {

/// Wrapper around SCCPSolver to easily provide ranges for a `Value`
class ValueRanges {
private:
  using LibraryInfo = llvm::TargetLibraryInfo;
  using LibraryInfoGetter = std::function<
    const LibraryInfo &(llvm::Function &)>;

private:
  llvm::SCCPSolver Solver;

public:
  ValueRanges(llvm::Function &F, const LibraryInfo &TLI) :
    Solver(F.getParent()->getDataLayout(),
           getLibraryInfo(TLI),
           F.getContext()) {
    Solver.markBlockExecutable(&F.getEntryBlock());

    for (auto &Argument : F.args())
      Solver.markOverdefined(&Argument);

    do {
      Solver.solve();
    } while (Solver.resolvedUndefsIn(F));
  }

public:
  /// Return a nonempty range at the original scalar integer width, valid at
  /// every use. Unknown values have the full range.
  llvm::ConstantRange range(const llvm::Value *V) const {
    if (auto *C = llvm::dyn_cast<llvm::ConstantInt>(V))
      return llvm::ConstantRange(C->getValue());

    // Other constants, including undef and poison, provide no usable bound.
    if (llvm::isa<llvm::Constant>(V))
      return fullRange(V);

    // SCCP does not populate instruction states in non-executable blocks.
    if (auto *I = llvm::dyn_cast<llvm::Instruction>(V)) {
      auto *Block = const_cast<llvm::BasicBlock *>(I->getParent());
      if (not Solver.isBlockExecutable(Block))
        return fullRange(V);
    }

    const auto &State = Solver.getLatticeValueFor(const_cast<llvm::Value *>(V));
    if (State.isConstantRange(false)) {
      llvm::ConstantRange Range = State.getConstantRange(false);
      if (not Range.isEmptySet())
        return Range;
    }
    return fullRange(V);
  }

private:
  static LibraryInfoGetter getLibraryInfo(const LibraryInfo &TLI) {
    return [&TLI](llvm::Function &) -> const LibraryInfo & { return TLI; };
  }

  static llvm::ConstantRange fullRange(const llvm::Value *V) {
    return llvm::ConstantRange::getFull(V->getType()->getIntegerBitWidth());
  }
};

} // namespace TypeShrinking
