/// \file PECOFF.cpp
/// \brief

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/Object/COFF.h"
#include "llvm/Object/ObjectFile.h"

#include "revng/Model/Binary.h"
#include "revng/Support/Debug.h"

// WIP
#include "Importers.h"
#include "MyFile.h"

using namespace llvm;

class PECOFF : public Lol {
private:
  MyFile File;
  TupleTree<model::Binary> &Model;
  const object::ObjectFile &TheBinary;
  uint64_t PreferedBaseAddress;

public:
  PECOFF(TupleTree<model::Binary> &Model,
         const object::ObjectFile &TheBinary,
         uint64_t PreferedBaseAddress) :
    File(*Model, {}),
    Model(Model),
    TheBinary(TheBinary),
    PreferedBaseAddress(PreferedBaseAddress) {}

  void import() {
    // WIP
    (void) File;
    (void) Model;
    (void) TheBinary;
    (void) PreferedBaseAddress;

    // WIP: set PreferedBaseAddress if PIC
    revng_assert(Model->Architecture != model::Architecture::Invalid);
    Architecture = Model->Architecture;
#if 0
    revng_assert(TheArchitecture.pointerSize() == 32
                   || TheArchitecture.pointerSize() == 64,
                 "Only 32/64-bit COFF files are supported");
    revng_assert(TheArchitecture.isLittleEndian() == true,
                 "Only Little-Endian COFF files are supported");
#endif
    // TODO handle relocations
    parseCOFF();
  }

  void parseCOFF();
};

inline void PECOFF::parseCOFF() {
  using object::COFFObjectFile;

  // WIP: upcast?
  auto TheCOFFOrErr = COFFObjectFile::create(TheBinary.getMemoryBufferRef());
  if (not TheCOFFOrErr) {
    logAllUnhandledErrors(TheCOFFOrErr.takeError(), errs(), "");
    revng_abort();
  }

  COFFObjectFile &TheCOFF = *TheCOFFOrErr.get();
  const object::pe32_header *PE32Header = TheCOFF.getPE32Header();

  MetaAddress ImageBase = MetaAddress::invalid();
  if (PE32Header) {
    // TODO: ImageBase should aligned to 4kb pages, should we check that?
    ImageBase = fromPC(PE32Header->ImageBase);

    Model->EntryPoint = ImageBase + u64(PE32Header->AddressOfEntryPoint);
#if 0
    ProgramHeaders.Count = PE32Header->NumberOfRvaAndSize;
    ProgramHeaders.Size = PE32Header->SizeOfHeaders;
#endif
  } else {
    const object::pe32plus_header *PE32PlusHeader = TheCOFF.getPE32PlusHeader();
    if (!PE32PlusHeader) {
      revng_abort("Invalid PE Header");
      return;
    }

    // PE32+ Header
    ImageBase = fromPC(PE32PlusHeader->ImageBase);
    Model->EntryPoint = ImageBase + u64(PE32PlusHeader->AddressOfEntryPoint);
#if 0
    ProgramHeaders.Count = PE32PlusHeader->NumberOfRvaAndSize;
    ProgramHeaders.Size = PE32PlusHeader->SizeOfHeaders;
#endif
  }

  // Read sections
  for (const llvm::object::SectionRef &SecRef : TheCOFF.sections()) {
    unsigned Id = TheCOFF.getSectionID(SecRef);
    Expected<const object::coff_section *> SecOrErr = TheCOFF.getSection(Id);
    if (not SecOrErr) {
      logAllUnhandledErrors(SecOrErr.takeError(), errs(), "");
      revng_abort();
    }
    const object::coff_section *CoffRef = *SecOrErr;

    // VirtualSize might be larger than SizeOfRawData (extra data at the end of
    // the section) or viceversa (data mapped in memory but not present in
    // memory, e.g., .bss)
    uint64_t SegmentSize = std::min(CoffRef->VirtualSize,
                                    CoffRef->SizeOfRawData);

    using namespace nooverflow;
    MetaAddress Start = ImageBase + u64(CoffRef->VirtualAddress);
    MetaAddress End = Start + u64(CoffRef->VirtualSize);
    model::Segment Segment({ Start, End });

    Segment.StartOffset = CoffRef->PointerToRawData;
    Segment.EndOffset = Segment.StartOffset + SegmentSize;

    Segment.IsReadable = CoffRef->Characteristics & COFF::IMAGE_SCN_MEM_READ;
    Segment.IsWriteable = CoffRef->Characteristics & COFF::IMAGE_SCN_MEM_WRITE;
    Segment.IsExecutable = CoffRef->Characteristics
                           & COFF::IMAGE_SCN_MEM_EXECUTE;

    Model->Segments.insert(std::move(Segment));

#if 0
    SegmentInfo Segment;
    Segment.StartVirtualAddress = ImageBase + u64(CoffRef->VirtualAddress);
    Segment.EndVirtualAddress = Segment.StartVirtualAddress
                                + u64(CoffRef->VirtualSize);
    Segment.StartFileOffset = CoffRef->PointerToRawData;
    Segment.EndFileOffset = Segment.StartFileOffset + SegmentSize;
    Segment.IsExecutable = CoffRef->Characteristics
                           & COFF::IMAGE_SCN_MEM_EXECUTE;
    Segment.IsReadable = CoffRef->Characteristics & COFF::IMAGE_SCN_MEM_READ;
    Segment.IsWriteable = CoffRef->Characteristics & COFF::IMAGE_SCN_MEM_WRITE;

    StringRef StringDataRef = SecRef.getObject()->getData();
    auto RawDataRef = ArrayRef<uint8_t>(StringDataRef.bytes_begin(),
                                        StringDataRef.size());

    Segment.Data = ArrayRef<uint8_t>(*add(RawDataRef.begin(),
                                          CoffRef->PointerToRawData),
                                     SegmentSize);

    revng_assert(contains(RawDataRef, Segment.Data));

    Segments.push_back(Segment);
#endif
  }
}

void importPECOFF(TupleTree<model::Binary> &Model,
                  const object::ObjectFile &TheBinary,
                  uint64_t PreferedBaseAddress) {
  PECOFF X(Model, TheBinary, PreferedBaseAddress);
  X.import();
}
