#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/IR/Instructions.h"

#include "revng/EarlyFunctionAnalysis/FunctionMetadata.h"
#include "revng/Model/Binary.h"
#include "revng/Model/IRHelpers.h"
#include "revng/Support/Assert.h"
#include "revng/Support/IRHelpers.h"
#include "revng/Support/MetaAddress.h"
#include "revng/TupleTree/TupleTree.h"

namespace detail {

inline TupleTree<efa::FunctionMetadata>
extractFunctionMetadata(llvm::MDNode *MD) {
  using namespace llvm;

  efa::FunctionMetadata FM;
  const MDOperand &Op = MD->getOperand(0);
  revng_assert(MD != nullptr && isa<MDString>(Op));

  StringRef YAMLString = cast<MDString>(Op)->getString();
  auto MaybeParsed = TupleTree<efa::FunctionMetadata>::deserialize(YAMLString);
  revng_assert(MaybeParsed);
  MaybeParsed->verify();
  return std::move(MaybeParsed.get());
}

} // namespace detail

inline TupleTree<efa::FunctionMetadata>
extractFunctionMetadata(const llvm::Function *F) {
  auto *MDNode = F->getMetadata(FunctionMetadataMDName);
  return detail::extractFunctionMetadata(MDNode);
}

inline TupleTree<efa::FunctionMetadata>
extractFunctionMetadata(llvm::BasicBlock *BB) {
  auto *MDNode = BB->getTerminator()->getMetadata(FunctionMetadataMDName);
  return detail::extractFunctionMetadata(MDNode);
}

/// \brief Given a Call instruction and the model type of its parent function,
///        return the edge on the model that represents that call, or nullptr if
///        this doesn't exist.
inline std::pair<efa::CallEdge *, MetaAddress>
getCallEdge(const model::Binary &Binary, const llvm::CallInst *Call) {
  using namespace llvm;

  MetaAddress BlockAddress = getMetaAddressMetadata(Call,
                                                    CallerBlockStartMDName);
  if (BlockAddress.isInvalid())
    return { nullptr, BlockAddress };

  auto *ParentFunction = Call->getParent()->getParent();
  efa::FunctionMetadata FM = *extractFunctionMetadata(ParentFunction).get();
  efa::BasicBlock Block = FM.ControlFlowGraph.at(BlockAddress);

  // Find the call edge
  efa::CallEdge *ModelCall = nullptr;
  for (auto &Edge : Block.Successors) {
    if (auto *CE = dyn_cast<efa::CallEdge>(Edge.get())) {
      revng_assert(ModelCall == nullptr);
      ModelCall = CE;
    }
  }
  revng_assert(ModelCall != nullptr);

  return { ModelCall, Block.Start };
}

/// \return the prototype associated to a CallInst.
///
/// \note If the model type of the parent function is not provided, this will be
///       deduced using the Call instruction's parent function.
///
/// \note If the callsite has no associated prototype, e.g. the called functions
///       is not an isolated function, a null pointer is returned.
inline model::TypePath
getCallSitePrototype(const model::Binary &Binary,
                     const llvm::CallInst *Call,
                     const model::Function *ParentFunction = nullptr) {
  if (not ParentFunction)
    ParentFunction = llvmToModelFunction(Binary, *Call->getFunction());

  if (not ParentFunction)
    return {};

  const auto &[Edge, BlockAddress] = getCallEdge(Binary, Call);
  if (not Edge)
    return {};

  return getPrototype(Binary, ParentFunction->Entry, BlockAddress, *Edge);
}
