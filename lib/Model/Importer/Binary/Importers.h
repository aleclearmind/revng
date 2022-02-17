#pragma once

#include <cstdint>

// WIP: header
#include "revng/Model/Binary.h"

namespace llvm {
namespace object {
class ObjectFile;
}
} // namespace llvm

void importELF(TupleTree<model::Binary> &Model,
               const llvm::object::ObjectFile &TheBinary,
               uint64_t PreferedBaseAddress);
void importPECOFF(TupleTree<model::Binary> &Model,
                  const llvm::object::ObjectFile &TheBinary,
                  uint64_t PreferedBaseAddress);
void importMachO(TupleTree<model::Binary> &Model,
                 const llvm::object::ObjectFile &TheBinary,
                 uint64_t PreferedBaseAddress);
