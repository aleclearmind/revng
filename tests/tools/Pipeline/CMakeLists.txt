#
# This file is distributed under the MIT License. See LICENSE.md for details.
#

# Define StringContainerLibrary
revng_add_library_internal(StringContainerLibrary
  SHARED
  ${CMAKE_CURRENT_LIST_DIR}/StringContainerLibrary.cpp
)
target_link_libraries(StringContainerLibrary
  revngPipeline
  revngPipes
)

macro(add_pipeline_test
  TEST_NAME
  PIPELINE_FILE
  TARGETS
  INPUTS_LIST
  OUTPUTS_LIST
  FLAGS
)
  add_test(NAME "${TEST_NAME}"
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bin/revng-pipeline"
      -P "${CMAKE_CURRENT_LIST_DIR}/${PIPELINE_FILE}"
      ${TARGETS}
      -i "${INPUTS_LIST}"
      -o "${OUTPUTS_LIST}"
      -l "$<TARGET_FILE:StringContainerLibrary>"
      -p "${CMAKE_CURRENT_BINARY_DIR}/${TEST_NAME}"
      -f "${FLAGS}"
  )
  set_tests_properties("${TEST_NAME}" PROPERTIES LABELS "pipeline;run")
endmacro()

macro(add_pipeline_dump_test TEST_NAME PIPELINE_FILE)
  add_test(NAME "${TEST_NAME}"
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bin/revng-pipeline"
      -P "${CMAKE_CURRENT_LIST_DIR}/${PIPELINE_FILE}"
      dc:dc:Root
      -l "$<TARGET_FILE:StringContainerLibrary>"
      -d
  )
  set_tests_properties("${TEST_NAME}" PROPERTIES LABELS "pipeline;dump")
endmacro()

macro(test_pipeline_output
  TEST_NAME
  PIPELINE_FILE
  TARGETS
  INPUTS_LIST
  OUTPUTS_LIST
  FLAGS
  FILE_TO_CHECK
  EXPECTED_OUTPUT
)

  add_pipeline_test("${TEST_NAME}"
    "${PIPELINE_FILE}"
    "${TARGETS}"
    "${INPUTS_LIST}"
    "${OUTPUTS_LIST}"
    "${FLAGS}"
  )
  add_test(NAME "${TEST_NAME}-check"
    COMMAND diff -u "${FILE_TO_CHECK}" "${EXPECTED_OUTPUT}")

  set_tests_properties("${TEST_NAME}-check"
    PROPERTIES
    DEPENDS "${TEST_NAME}"
    LABELS "pipeline;check"
  )
endmacro()

macro(ensure_pipeline_failure
  TEST_NAME
  PIPELINE_FILE
  TARGETS
  INPUTS_LIST
  OUTPUTS_LIST
  FLAGS
)
  add_pipeline_test("${TEST_NAME}"
    "${PIPELINE_FILE}"
    "${TARGETS}"
    "${INPUTS_LIST}"
    "${OUTPUTS_LIST}"
    "${FLAGS}"
    "${EXPECTED_OUTPUT}"
  )
  set_tests_properties("${TEST_NAME}" PROPERTIES WILL_FAIL TRUE)
endmacro()

set(COPY_PIPE_TEST_INPUTS "FirstStep:Strings1:${CMAKE_CURRENT_LIST_DIR}/CopyPipeTestInput.txt")
set(COPY_PIPE_TEST_OUTPUTS "End:Strings2:${CMAKE_CURRENT_BINARY_DIR}/CopyPipeTestOutput.txt")

test_pipeline_output(pipeline-copy-pipe
  CopyPipeTestPipeline.yml
  Strings2:Root:Root
  "${COPY_PIPE_TEST_INPUTS}"
  "${COPY_PIPE_TEST_OUTPUTS}"
  None
  CopyPipeTestOutput.txt
  "${CMAKE_CURRENT_LIST_DIR}/CopyPipeTestExpected.txt"
)

# Try to produce a target produced by a non-disabled pipe
test_pipeline_output(pipeline-copy-pipe-flag
  CopyPipeFlagTestPipeline.yml
  Strings2:Root:Root
  "${COPY_PIPE_TEST_INPUTS}"
  "${COPY_PIPE_TEST_OUTPUTS}"
  None
  CopyPipeTestOutput.txt
  "${CMAKE_CURRENT_LIST_DIR}/CopyPipeTestExpected.txt"
)

# Try to produce a target produced by a disabled pipe
ensure_pipeline_failure(pipeline-flag-fail-test
  CopyPipeFlagTestPipeline.yml
  Strings2:Root:Root
  "${COPY_PIPE_TEST_INPUTS}"
  "${COPY_PIPE_TEST_OUTPUTS}"
  DisableCopyPipe
)

# Try to invoke a non-existing pass using a PureLLVMPipe
ensure_pipeline_failure(pipeline-test-missing-pipe
  MissingPassPipelineTest.yml
  Strings2:Root:Root
  "${COPY_PIPE_TEST_INPUTS}"
  "${COPY_PIPE_TEST_OUTPUTS}"
  DontCare
)

# Test an LLVMPass pipe
add_pipeline_dump_test(pipeline-llvm-pass-test PassPipelineTest.yml)

# Test running revng-pipeline in multiple invocation
add_test(NAME pipeline-multistep-test
  COMMAND
    "${CMAKE_CURRENT_LIST_DIR}/MultiStepPipelineTest.sh"
      --load $<TARGET_FILE:StringContainerLibrary>
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
)
set_tests_properties(pipeline-multistep-test
  PROPERTIES
  DEPENDS revng-pipeline
  LABELS "pipeline"
)

revng_add_library_internal(FunctionCounter
  SHARED
  ${CMAKE_CURRENT_LIST_DIR}/ModelFunctionCounter.cpp
)
target_link_libraries(FunctionCounter revngPipeline)
