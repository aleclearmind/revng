#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/Object/Binary.h"
#include "llvm/ADT/StringRef.h"

#include "revng/Model/Binary.h"

namespace llvm {
namespace object {
class ObjectFile;
}
}

void importBinary(TupleTree<model::Binary> &Model,
                  const llvm::object::ObjectFile &BinaryHandle);
void importBinary(TupleTree<model::Binary> &Model, llvm::StringRef Path);
