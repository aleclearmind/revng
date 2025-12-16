#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/Model/ModelGlobalVariableBuilder.h"
#include "revng/SegmentReferences/SegmentUsesEnumerator.h"

class RawBinaryView;

class DetectCStrings {
private:
  SegmentUsesEnumerator SegmentUses;
  RawBinaryView &BinaryView;
  ModelGlobalVariableBuilder GlobalBuilder;

public:
  DetectCStrings(model::Binary &Binary, RawBinaryView &BinaryView) :
    SegmentUses(Binary, SegmentUsesEnumerator::SegmentAccess::ReadOnly),
    BinaryView(BinaryView),
    GlobalBuilder(Binary) {}

  void run(llvm::Module &M, llvm::Function *LimitTo = nullptr);
};
