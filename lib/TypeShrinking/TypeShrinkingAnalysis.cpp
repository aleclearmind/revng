/// Plan integer rewrites using LLVM value ranges and demanded bits.

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <algorithm>
#include <optional>

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/DemandedBits.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormattedStream.h"

#include "revng/Support/CommandLine.h"
#include "revng/Support/Debug.h"
#include "revng/TypeShrinking/TypeShrinkingAnalysis.h"

#include "ValueRanges.h"

using namespace llvm;
using std::max;
using std::min;

static cl::opt<uint32_t> MinimumWidth("min-width",
                                      cl::init(8),
                                      cl::desc("Minimum emitted integer width"),
                                      cl::value_desc("min-width"),
                                      cl::cat(MainCategory));

namespace TypeShrinking {

AnalysisKey TypeShrinkingAnalysisPass::Key;
char TypeShrinkingAnalysisWrapperPass::ID = 0;
using Register = RegisterPass<TypeShrinkingAnalysisWrapperPass>;
static Register
  X("type-shrinking-analysis", "Plan integer type shrinking", true, true);

/// Return the integer width, or zero for a non-integer value.
static unsigned width(const Value *V) {
  return V->getType()->isIntegerTy() ? V->getType()->getIntegerBitWidth() : 0;
}

/// Whether the opcode is integer division or remainder.
static bool isDivision(unsigned Opcode) {
  return Opcode == Instruction::UDiv or Opcode == Instruction::SDiv
         or Opcode == Instruction::URem or Opcode == Instruction::SRem;
}

static unsigned roundWidth(unsigned Required, unsigned Original) {
  Required = max(Required, MinimumWidth.getValue());

  for (unsigned Candidate : { 8U, 16U, 32U, 64U }) {
    if (Candidate >= Required and Candidate < Original)
      return Candidate;
  }
  return Original;
}

std::optional<RewritePlan> RewritePlan::create(Instruction &I,
                                               unsigned Demand,
                                               const ValueRanges &Ranges) {
  if (Demand == 0)
    return std::nullopt;

  // Comparisons preserve the operands' values, rather than their low bits.
  if (auto *Compare = dyn_cast<ICmpInst>(&I)) {
    unsigned OperandWidth = width(Compare->getOperand(0));
    if (OperandWidth == 0)
      return std::nullopt;
    ConstantRange A = Ranges.range(Compare->getOperand(0));
    ConstantRange B = Ranges.range(Compare->getOperand(1));
    unsigned U = max(A.getActiveBits(), B.getActiveBits());
    unsigned S = max(A.getMinSignedBits(), B.getMinSignedBits());
    U = roundWidth(U, OperandWidth);
    S = roundWidth(S, OperandWidth);

    // A narrower unsigned representation makes both operands nonnegative
    // at their original width, so unsigned predicates preserve their order.
    auto Predicate = Compare->getPredicate();
    if (U <= S and U < OperandWidth)
      Predicate = Compare->getUnsignedPredicate();

    return RewritePlan(ComparisonRewrite{ min(U, S), Predicate });
  }

  unsigned W = width(&I);
  if (W == 0)
    return std::nullopt;

  // Only plan operations supported by the rebuilder.
  switch (I.getOpcode()) {
  case Instruction::PHI:
  case Instruction::Select:
  case Instruction::Trunc:
  case Instruction::ZExt:
  case Instruction::SExt:
  case Instruction::Add:
  case Instruction::Sub:
  case Instruction::Mul:
  case Instruction::And:
  case Instruction::Or:
  case Instruction::Xor:
  case Instruction::Shl:
  case Instruction::LShr:
  case Instruction::AShr:
  case Instruction::UDiv:
  case Instruction::URem:
  case Instruction::SDiv:
  case Instruction::SRem:
    break;
  default:
    return std::nullopt;
  }

  // Keep the cheaper zero/sign representation of the demanded result bits.
  ConstantRange Bounds = Ranges.range(&I);
  unsigned U = roundWidth(min(Demand, Bounds.getActiveBits()), W);
  unsigned S = roundWidth(min(Demand, Bounds.getMinSignedBits()), W);
  unsigned ResultWidth = min(U, S);
  IntegerRewrite P{ ResultWidth,
                    ResultWidth,
                    S < U ? ExtensionKind::Sign : ExtensionKind::Zero };
  unsigned Required = min(Demand, P.ResultWidth);

  // Shift execution must retain the source bits and exceed every shift amount.
  if (I.isShift()) {
    ConstantRange Amounts = Ranges.range(I.getOperand(1));
    unsigned Maximum = Amounts.getUnsignedMax().getLimitedValue(W);
    if (Maximum >= W) {
      // The range does not prove that narrowing keeps the shift defined.
      P.ComputationWidth = W;
    } else {
      unsigned Input = Required;
      if (I.getOpcode() != Instruction::Shl) {
        // Right shifts pull in high bits, capped by the exact input width.
        ConstantRange InputBounds = Ranges.range(I.getOperand(0));
        unsigned Exact = I.getOpcode() == Instruction::LShr ?
                           InputBounds.getActiveBits() :
                           InputBounds.getMinSignedBits();
        Input = min(Required + Maximum, Exact);
      }
      unsigned Execution = max({ P.ResultWidth, Input, Maximum + 1 });
      P.ComputationWidth = roundWidth(Execution, W);
    }
  } else if (isDivision(I.getOpcode())) {
    // Division and remainder need exact operands even for a partial result.
    bool Signed = I.getOpcode() == Instruction::SDiv
                  or I.getOpcode() == Instruction::SRem;
    ConstantRange Left = Ranges.range(I.getOperand(0));
    ConstantRange Right = Ranges.range(I.getOperand(1));
    unsigned Input = Signed ?
                       max(Left.getMinSignedBits(), Right.getMinSignedBits()) :
                       max(Left.getActiveBits(), Right.getActiveBits());
    P.ComputationWidth = roundWidth(max(P.ResultWidth, Input), W);

    // Narrowing must not introduce signed-minimum / -1 overflow.
    if (Signed) {
      while (P.ComputationWidth < W) {
        APInt Minimum = APInt::getSignedMinValue(P.ComputationWidth).sext(W);
        bool CanOverflow = Left.contains(Minimum)
                           and Right.contains(APInt::getAllOnes(W));
        if (not CanOverflow)
          break;

        P.ComputationWidth = roundWidth(P.ComputationWidth + 1, W);
      }
    }
  }
  return RewritePlan(P);
}

TypeShrinkingAnalysisPass::Result
TypeShrinkingAnalysisPass::run(Function &F, FunctionAnalysisManager &) {
  AssumptionCache AC(F);
  DominatorTree DT(F);
  TargetLibraryInfoImpl Impl(Triple(F.getParent()->getTargetTriple()));
  TargetLibraryInfo TLI(Impl);

  // Solve ranges with SCCP, including loop invariants. Use them to refine
  // backward demand before creating rewrite plans.
  ValueRanges Ranges(F, TLI);
  DemandedBits Demands(F, AC, DT, [&Ranges](const Use &U) {
    return Ranges.range(U.get());
  });

  // Discard dead users before narrowing shared producers. Collect the whole
  // dead set, including unreachable users, so deletion leaves no dangling uses.
  TypeShrinkingAnalysisResults Result;
  for (Instruction &I : instructions(F)) {
    if (Demands.isInstructionDead(&I))
      Result.DeadInstructions.push_back(&I);
  }

  // Retain a low-bit prefix covering LLVM's demanded mask. Range queries
  // finish before rebuilding invalidates either analysis.
  ReversePostOrderTraversal<Function *> Blocks(&F);
  for (BasicBlock *Block : Blocks) {
    for (Instruction &I : *Block) {
      if (not I.getType()->isIntegerTy() or Demands.isInstructionDead(&I))
        continue;

      unsigned Demand = Demands.getDemandedBits(&I).getActiveBits();
      if (auto Plan = RewritePlan::create(I, Demand, Ranges))
        Result.Plans.emplace(&I, *Plan);
    }
  }
  return Result;
}

bool TypeShrinkingAnalysisWrapperPass::runOnFunction(Function &F) {
  FunctionAnalysisManager FAM;
  Result = TypeShrinkingAnalysisPass().run(F, FAM);
  return false;
}

class TypeShrinkingAnnotatedWriter : public AssemblyAnnotationWriter {
private:
  const RewritePlans &Plans;

public:
  TypeShrinkingAnnotatedWriter(const RewritePlans &Plans) : Plans(Plans) {}

public:
  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &Stream) final {
    auto It = Plans.find(const_cast<Instruction *>(I));
    if (It == Plans.end())
      return;

    if (auto *P = It->second.getIntegerRewrite()) {
      bool Signed = P->ResultExtension == ExtensionKind::Sign;
      StringRef Extension = Signed ? "sign" : "zero";
      Stream << "  ; Result width: " << P->ResultWidth << "\n";
      Stream << "  ; Computation width: " << P->ComputationWidth << "\n";
      Stream << "  ; Result extension: " << Extension << "\n";
    } else {
      const auto &Compare = *It->second.getComparisonRewrite();
      Stream << "  ; Operand width: " << Compare.OperandWidth << "\n";
      Stream << "  ; Predicate: "
             << CmpInst::getPredicateName(Compare.Predicate) << "\n";
    }
  }
};

void TypeShrinkingAnalysisWrapperPass::dump(Function &F) const {
  TypeShrinkingAnnotatedWriter Annotator(Result.Plans);
  raw_os_ostream Stream(dbg);
  F.print(Stream, &Annotator);
}

} // namespace TypeShrinking
