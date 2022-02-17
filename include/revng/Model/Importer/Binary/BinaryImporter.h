#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/ADT/StringRef.h"
#include "llvm/Object/Binary.h"

#include "revng/Model/Binary.h"

namespace llvm {
namespace object {
class ObjectFile;
}
} // namespace llvm

void importBinary(TupleTree<model::Binary> &Model,
                  const llvm::object::ObjectFile &BinaryHandle);
void importBinary(TupleTree<model::Binary> &Model, llvm::StringRef Path);
