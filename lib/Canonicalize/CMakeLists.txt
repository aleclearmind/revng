#
# This file is distributed under the MIT License. See LICENSE.md for details.
#

revng_add_analyses_library_internal(
  revngCanonicalize
  ExitSSAPass.cpp
  HoistStructPhis.cpp
  LoopRewriteWithCanonicalIV.cpp
  PeepholeOptimizationPass.cpp
  RemoveConstantArrayReturnsPass.cpp
  RemoveLLVMAssumeCallsPass.cpp
  SimplifySwitchPass.cpp
  SplitOverflowIntrinsicsPass.cpp
  SwitchToStatements.cpp
  TernaryReductionPass.cpp)

target_link_libraries(
  revngCanonicalize
  revngSupport
  revngABI
  revngLocalVariables
  revngModel
  revngPipes
  revngPTML
  revngLift
  ${LLVM_LIBRARIES})
