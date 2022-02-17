/// \file BinaryImporter.cpp
/// \brief

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/Object/ObjectFile.h"

#include "revng/Model/Importer/Binary/BinaryImporter.h"

#include "Importers.h"

using namespace llvm;

void importBinary(TupleTree<model::Binary> &Model,
                  const llvm::object::ObjectFile &TheBinary) {
  Model->Architecture = model::Architecture::fromLLVMArchitecture(
    TheBinary.getArch());

  // WIP
  uint64_t PreferedBaseAddress = 0;
  if (TheBinary.isELF()) {
    importELF(Model, TheBinary, PreferedBaseAddress);
  } else if (TheBinary.isCOFF()) {
    importPECOFF(Model, TheBinary, PreferedBaseAddress);
  } else if (TheBinary.isMachO()) {
    importMachO(Model, TheBinary, PreferedBaseAddress);
  } else {
    // WIP: soft fail
    revng_abort();
  }
}

void importBinary(TupleTree<model::Binary> &Model, llvm::StringRef Path) {
  auto BinaryOrErr = object::createBinary(Path);
  revng_check(BinaryOrErr, "Couldn't open the input file");
  importBinary(Model, *cast<object::ObjectFile>(BinaryOrErr->getBinary()));
}
