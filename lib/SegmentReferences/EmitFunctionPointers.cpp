//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/ABI/FunctionType/Layout.h"
#include "revng/ABI/ModelHelpers.h"
#include "revng/Model/NameBuilder.h"
#include "revng/Pipeline/Contract.h"
#include "revng/Pipeline/RegisterPipe.h"
#include "revng/Pipes/Kinds.h"
#include "revng/SegmentReferences/SegmentUsesEnumerator.h"
#include "revng/Support/IRBuilder.h"
#include "revng/Support/IRHelpers.h"
#include "revng/Support/MetaAddress/MetaAddressRangeSet.h"

using namespace llvm;

static Logger Log("emit-function-pointers");

// WIP FINAL: implement piperuns
/// \note Do not run this before segregate-stack-access, this function needs to
//        create LLVM functions on the fly and it does so assuming we're dealing
//        with the final form of LLVM functions.
class EmitFunctionPointers {
private:
  const model::Binary &Binary;
  MetaAddressRangeSet ExecutableRanges;
  SegmentUsesEnumerator SegmentUses;

public:
  EmitFunctionPointers(const model::Binary &Binary) :
    Binary(Binary),
    ExecutableRanges(Binary.executableRanges()),
    SegmentUses(Binary, SegmentUsesEnumerator::SegmentAccess::ExecutableOnly) {}

  void run(llvm::Module &M,
           model::Architecture::Values Architecture,
           llvm::Function *LimitTo = nullptr) {
    revng::IRBuilder B(M.getContext());

    for (auto &&SegmentUse : SegmentUses.getUses(M, LimitTo)) {
      auto Address = SegmentUse.Address;
      auto &Use = SegmentUse.TheUse;
      revng_log(Log,
                "Considering segment use " << getName(Use->getUser())
                                           << ". Address is "
                                           << Address.toString() << ".");
      LoggerIndent Indent(Log);

      // Check if it falls in an executable segment
      //
      // We could skip doing this, but this reduces the amount of invalidation
      // data we track due to the next check.
      if (not ExecutableRanges.contains(Address)) {
        revng_log(Log, "Not executable, bailing out");
        continue;
      }

      // Check if it matches a function entry
      using model::Function;
      auto PC = MetaAddress::fromPC(Architecture,
                                    Address.address(),
                                    Address.epoch(),
                                    Address.addressSpace());
      revng_log(Log, "Looking for function " << PC.toString());
      const Function *Function = Binary.Functions().tryGet(PC);
      if (Function == nullptr) {
        revng_log(Log, "No corresponding function entry, bailing out");
        continue;
      }

      // OK, this is a pointer to a function entry, let's get the LLVM
      // function
      llvm::Function &LLVMFunction = getOrCreateFunctionFor(M, *Function);

      // And let's emit a reference to it
      B.SetInsertPoint(cast<Instruction>(Use->getUser()));
      Type *UseType = Use->get()->getType();
      Value *NewValue = nullptr;
      if (UseType->isIntegerTy()) {
        NewValue = B.CreatePtrToInt(&LLVMFunction, UseType);
      } else {
        revng_assert(UseType->isPointerTy());
        NewValue = &LLVMFunction;
      }

      revng_log(Log, "Replacing with function reference");
      Use->set(NewValue);
    }

  }

private:
  llvm::Function &
  getOrCreateFunctionFor(llvm::Module &M,
                         const model::Function &ModelFunction) const {
    auto *Prototype = Binary.prototypeOrDefault(ModelFunction.prototype());
    auto TheLayout = abi::FunctionType::Layout::make(notNull(Prototype));
    auto &FT = layoutToLLVMFunctionType<false>(M.getContext(),
                                               Binary.Architecture(),
                                               TheLayout);
    std::string LLVMName = llvmName(ModelFunction);
    llvm::Function *Result = M.getFunction(LLVMName);

    if (Result != nullptr) {
      revng_assert(Result->getFunctionType() == &FT);
    } else {
      Result = Function::Create(&FT, GlobalValue::ExternalLinkage, LLVMName, M);
    }

    return *Result;
  }
};

namespace revng::pipes {

class EmitFunctionPointers {
public:
  static constexpr auto Name = "emit-function-pointers";

  std::array<pipeline::ContractGroup, 1> getContract() const {
    return { pipeline::ContractGroup({ kinds::StackAccessesSegregated,
                                       0,
                                       kinds::StackAccessesSegregated,
                                       0 }) };
  }

  void run(pipeline::ExecutionContext &EC,
           pipeline::LLVMContainer &ModuleContainer) {
    llvm::Module &M = ModuleContainer.getModule();
    auto ContainerName = ModuleContainer.name();
    ::EmitFunctionPointers Replacer(*getModelFromContext(EC));

    for (auto &&[ModelFunction, LLVMFunction] :
         kinds::TaggedFunctionKind::getFunctionsAndCommit(EC,
                                                          M,
                                                          ContainerName)) {
      Replacer.run(*LLVMFunction->getParent(),
                   ModelFunction->Entry().arch(),
                   LLVMFunction);
    }
  }
};

} // namespace revng::pipes

static pipeline::RegisterPipe<revng::pipes::EmitFunctionPointers> E;
