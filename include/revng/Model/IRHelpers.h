#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"

#include "revng/Model/Binary.h"
#include "revng/Support/Assert.h"
#include "revng/Support/CommonOptions.h"
#include "revng/Support/IRHelpers.h"
#include "revng/Support/MetaAddress.h"

constexpr const char *PrototypeMDName = "revng.prototype";

[[nodiscard]] inline std::string llvmName(const model::Function &Function) {
  if (DebugNames and not Function.Name().empty())
    return "local_" + Function.Name();
  else
    return "local_" + Function.Entry().toIdentifier();
}

template<ConstOrNot<model::Binary> T>
inline ConstPtrIfConst<T, model::TypeDefinition>
getCallSitePrototype(T &Binary, const llvm::Instruction *Call) {
  revng_assert(llvm::isa<llvm::CallInst>(Call));

  llvm::StringRef SerializedRef = fromStringMetadata(Call, PrototypeMDName);
  auto Result = model::DefinitionReference::fromString(&Binary, SerializedRef);

  if constexpr (std::is_const_v<T>)
    return Result.getConst();
  else
    return Result.get();
}

inline model::Function *llvmToModelFunction(model::Binary &Binary,
                                            const llvm::Function &F) {
  auto MaybeMetaAddress = getMetaAddressMetadata(&F, FunctionEntryMDName);
  if (MaybeMetaAddress == MetaAddress::invalid())
    return nullptr;
  if (auto It = Binary.Functions().tryGet(MaybeMetaAddress); It != nullptr)
    return It;

  return nullptr;
}

inline const model::Function *llvmToModelFunction(const model::Binary &Binary,
                                                  const llvm::Function &F) {
  auto MaybeMetaAddress = getMetaAddressMetadata(&F, FunctionEntryMDName);
  if (MaybeMetaAddress == MetaAddress::invalid())
    return nullptr;
  if (auto It = Binary.Functions().find(MaybeMetaAddress);
      It != Binary.Functions().end())
    return &*It;

  return nullptr;
}

inline llvm::IntegerType *getLLVMIntegerTypeFor(llvm::LLVMContext &Context,
                                                const model::Type &Type) {
  revng_assert(Type.size());
  return llvm::IntegerType::getIntNTy(Context, *Type.size() * 8);
}

inline llvm::IntegerType *getLLVMTypeForScalar(llvm::LLVMContext &Context,
                                               const model::Type &Type) {
  revng_assert(Type.isScalar());
  return getLLVMIntegerTypeFor(Context, Type);
}

inline llvm::SmallVector<llvm::Type *>
toLLVMTypes(llvm::LLVMContext &Context,
            const llvm::SmallVector<model::Register::Values> &Registers) {
  using namespace llvm;
  SmallVector<llvm::Type *> Result;
  auto IntoLLVMType = [&Context](model::Register::Values V) -> Type * {
    return IntegerType::getIntNTy(Context, 8 * model::Register::getSize(V));
  };
  std::ranges::copy(Registers | std::views::transform(IntoLLVMType),
                    std::back_inserter(Result));
  return Result;
}
