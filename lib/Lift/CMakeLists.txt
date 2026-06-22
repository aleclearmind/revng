#
# This file is distributed under the MIT License. See LICENSE.md for details.
#

revng_add_library_internal(
  revngLift
  SHARED
  CodeGenerator.cpp
  CSVOffsets.cpp
  ExternalJumpsHandler.cpp
  InstructionTranslator.cpp
  IRAnnotators.cpp
  LibTcg.cpp
  Lift.cpp
  LiftPipe.cpp
  LinkSupportPipe.cpp
  LoadBinaryPass.cpp
  JumpTargetManager.cpp
  PostLiftVerifyPass.cpp
  RootAnalyzer.cpp
  SelfReferencingDbgAnnotationWriter.cpp
  ValueMaterializerPass.cpp
  VariableManager.cpp)

if(WIN32)
  # `dl` and `m` are POSIX-only — Windows has no libdl (we use
  # LoadLibrary directly in LibTcg.cpp) and the math routines live in
  # msvcrt.dll, not a separate libm.
  set(_revngLift_platform_libs "")
else()
  set(_revngLift_platform_libs dl m)
endif()
target_link_libraries(
  revngLift
  PUBLIC ${_revngLift_platform_libs}
         revngABI
         revngBasicAnalyses
         revngModelImporterDebugInfo
         revngFunctionCallIdentification
         revngModel
         revngPipes
         revngSupport
         revngTypeShrinking
         revngEarlyFunctionAnalysis
         revngValueMaterializer
         ${LLVM_LIBRARIES})
