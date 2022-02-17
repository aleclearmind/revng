/// \file ELF.cpp
/// \brief

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <optional>

#include "llvm/ADT/DenseMap.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ObjectFile.h"

#include "revng/Model/Binary.h"
#include "revng/Support/Debug.h"

// WIP
#include "Importers.h"
#include "MyFile.h"

using namespace llvm;

static Logger<> EhFrameLog("ehframe");

// WIP: error reporting and no hard failures

// WIP: outline
static bool shouldIgnoreSymbol(StringRef Name) {
  return Name == "$a" or Name == "$d";
}

template<typename T>
static void logAddress(T &Logger, const char *Name, MetaAddress Address) {
  if (Logger.isEnabled()) {
    Logger << Name;
    Address.dump(Logger);
    Logger << DoLog;
  }
}

class FilePortion {
private:
  const MyFile &File;
  bool HasAddress;
  bool HasSize;
  uint64_t Size;
  MetaAddress Address;

public:
  FilePortion(const MyFile &File) :
    File(File),
    HasAddress(false),
    HasSize(false),
    Size(0),
    Address(MetaAddress::invalid()) {}

public:
  void setAddress(MetaAddress Address) {
    HasAddress = true;
    this->Address = Address;
  }

  void setSize(uint64_t Size) {
    HasSize = true;
    this->Size = Size;
  }

  MetaAddress addressAtOffset(uint64_t Offset) {
    revng_assert(HasAddress and HasSize);
    revng_assert(Offset <= Size);
    return Address + Offset;
  }

  template<typename T>
  MetaAddress addressAtIndex(uint64_t Index) {
    revng_assert(HasAddress and HasSize);
    uint64_t Offset = Index * sizeof(T);
    revng_assert(Offset <= Size);
    return Address + Offset;
  }

  bool isAvailable() const { return HasAddress; }

  bool isExact() const {
    revng_assert(HasAddress);
    return HasSize;
  }

  StringRef extractString() const {
    ArrayRef<uint8_t> Data = extractData();
    const char *AsChar = reinterpret_cast<const char *>(Data.data());
    return StringRef(AsChar, Data.size());
  }

  template<typename T>
  ArrayRef<T> extractAs() const {
    ArrayRef<uint8_t> Data = extractData();
    const size_t TypeSize = sizeof(T);
    revng_assert(Data.size() % TypeSize == 0);
    return ArrayRef<T>(reinterpret_cast<const T *>(Data.data()),
                       Data.size() / TypeSize);
  }

  ArrayRef<uint8_t> extractData() const {
    revng_assert(HasAddress);

    if (HasSize) {
      return File.getByAddress(Address, Size);
    } else {
      return File.getFromAddressOn(Address);
    }
  }
};

template<typename T, bool HasAddend>
uint64_t symbolsCount(const FilePortion &Relocations) {
  using Elf_Rel = llvm::object::Elf_Rel_Impl<T, HasAddend>;

  if (not Relocations.isAvailable())
    return 0;

  uint32_t SymbolsCount = 0;
  revng_assert(Relocations.isExact());
  for (Elf_Rel Relocation : Relocations.extractAs<Elf_Rel>())
    SymbolsCount = std::max(SymbolsCount, Relocation.getSymbol(false) + 1);

  return SymbolsCount;
}

// WIP: outline
namespace SymbolType {

enum Values { Unknown, Code, Data, Section, File };

inline const char *getName(Values V) {
  switch (V) {
  case Unknown:
    return "Unknown";
  case Code:
    return "Code";
  case Data:
    return "Data";
  case Section:
    return "Section";
  case File:
    return "File";
  }

  revng_abort();
}

inline SymbolType::Values fromELF(unsigned char ELFSymbolType) {
  switch (ELFSymbolType) {
  case llvm::ELF::STT_FUNC:
    return SymbolType::Code;

  case llvm::ELF::STT_OBJECT:
    return SymbolType::Data;

  case llvm::ELF::STT_SECTION:
    return SymbolType::Section;

  case llvm::ELF::STT_FILE:
    return SymbolType::File;

  default:
    return SymbolType::Unknown;
  }
}

} // namespace SymbolType

/// \brief A pair on steroids to wrap a value or a pointer to a value
class Pointer {
public:
  Pointer() : IsIndirect(false), Value(MetaAddress::invalid()) {}

  Pointer(bool IsIndirect, MetaAddress Value) :
    IsIndirect(IsIndirect), Value(Value) {}

  bool isIndirect() const { return IsIndirect; }
  MetaAddress value() const { return Value; }

private:
  bool IsIndirect;
  MetaAddress Value;
};

template<typename T>
inline bool isNull(const ArrayRef<T> &Array) {
  bool IsNull = Array.data() == nullptr;
  revng_assert(not IsNull or Array.size() == 0);
  return IsNull;
}

//
// What follows is a set of functions we use to read an integer of a specified
// (or pointer) size using the appropriate endianess associated to an ELF type.
//

template<typename T, typename EE>
struct Endianess {
  /// \brief Reads an integer of type T, using the endianess of the ELF type EE
  static uint64_t read(const uint8_t *Buf);
};

template<typename T>
struct Endianess<T, llvm::object::ELF32LE> {
  static uint64_t read(const uint8_t *Buf) {
    using namespace llvm::support;
    return llvm::support::endian::read<T, little, unaligned>(Buf);
  }
};

template<typename T>
struct Endianess<T, llvm::object::ELF64LE> {
  static uint64_t read(const uint8_t *Buf) {
    using namespace llvm::support;
    return llvm::support::endian::read<T, little, unaligned>(Buf);
  }
};

template<typename T>
struct Endianess<T, llvm::object::ELF32BE> {
  static uint64_t read(const uint8_t *Buf) {
    using namespace llvm::support;
    return llvm::support::endian::read<T, big, unaligned>(Buf);
  }
};

template<typename T>
struct Endianess<T, llvm::object::ELF64BE> {
  static uint64_t read(const uint8_t *Buf) {
    using namespace llvm::support;
    return llvm::support::endian::read<T, big, unaligned>(Buf);
  }
};

/// \brief Read a pointer-sized integer according to the given ELF type EE
template<typename EE>
inline uint64_t readPointer(const uint8_t *Buf);

template<>
inline uint64_t readPointer<llvm::object::ELF32LE>(const uint8_t *Buf) {
  return Endianess<uint32_t, llvm::object::ELF32LE>::read(Buf);
}

template<>
inline uint64_t readPointer<llvm::object::ELF32BE>(const uint8_t *Buf) {
  return Endianess<uint32_t, llvm::object::ELF32BE>::read(Buf);
}

template<>
inline uint64_t readPointer<llvm::object::ELF64LE>(const uint8_t *Buf) {
  return Endianess<uint64_t, llvm::object::ELF64LE>::read(Buf);
}

template<>
inline uint64_t readPointer<llvm::object::ELF64BE>(const uint8_t *Buf) {
  return Endianess<uint64_t, llvm::object::ELF64BE>::read(Buf);
}

// WIP: rename
class ELFImporter : public Lol {
private:
  MyFile File;
  TupleTree<model::Binary> &Model;
  const object::ObjectFile &TheBinary;
  uint64_t PreferedBaseAddress;

public:
  // WIP: is this OK?
  // WIP: drop PreferedBaseAddress
  ELFImporter(TupleTree<model::Binary> &Model,
              const object::ObjectFile &TheBinary,
              uint64_t PreferedBaseAddress) :
    File(*Model,
         makeArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(
                                 TheBinary.getData().data()),
                               TheBinary.getData().size())),
    Model(Model),
    TheBinary(TheBinary),
    PreferedBaseAddress(PreferedBaseAddress) {}

private:
  template<typename T, bool Addend>
  using Elf_Rel_Array = llvm::ArrayRef<llvm::object::Elf_Rel_Impl<T, Addend>>;

public:
  template<typename T, bool HasAddend>
  void import();

private:
  template<typename T>
  MetaAddress getGenericPointer(Pointer Ptr) const {
    if (not Ptr.isIndirect())
      return Ptr.value();

    llvm::ArrayRef<uint8_t> Pointer = File.getFromAddressOn(Ptr.value());
    revng_assert(not isNull(Pointer), "Pointer not available in any segment");

    return fromGeneric(::readPointer<T>(Pointer.data()));
  }

  template<typename T>
  MetaAddress getCodePointer(Pointer Ptr) const {
    return getGenericPointer<T>(Ptr).toPC(
      model::Architecture::toLLVMArchitecture(Model->Architecture));
  }

  /// \brief Parse the .eh_frame_hdr section to obtain the address and the
  ///        number of FDEs in .eh_frame
  ///
  /// \return a pair containing the pointer to the .eh_frame section and the
  ///         count of FDEs in the .eh_frame_hdr section (which should match the
  ///         number of FDEs in .eh_frame)
  template<typename T>
  std::pair<MetaAddress, uint64_t>
  ehFrameFromEhFrameHdr(MetaAddress EHFrameHdrAddress);

  /// \brief Parse the .eh_frame section to collect all the landing pads
  ///
  /// \param EHFrameAddress the address of the .eh_frame section
  /// \param FDEsCount the count of FDEs in the .eh_frame section
  /// \param EHFrameSize the size of the .eh_frame section
  ///
  /// \note Either \p FDEsCount or \p EHFrameSize have to be specified
  template<typename T>
  void parseEHFrame(MetaAddress EHFrameAddress,
                    Optional<uint64_t> FDEsCount,
                    Optional<uint64_t> EHFrameSize);

  /// \brief Parse an LSDA to collect its landing pads
  ///
  /// \param FDEStart the start address of the FDE to which this LSDA is
  ///        associated
  /// \param LSDAAddress the address of the target LSDA
  template<typename T>
  void parseLSDA(MetaAddress FDEStart, MetaAddress LSDAAddress);

  /// \brief Register a label for each input relocation
  template<typename T, bool HasAddend>
  void registerRelocations(Elf_Rel_Array<T, HasAddend> Relocations,
                           const FilePortion &Dynsym,
                           const FilePortion &Dynstr);
};

template<typename T, bool HasAddend>
inline void ELFImporter::import() {
  // Parse the ELF file
  auto TheELFOrErr = object::ELFFile<T>::create(TheBinary.getData());
  if (not TheELFOrErr) {
    logAllUnhandledErrors(TheELFOrErr.takeError(), errs(), "");
    revng_abort();
  }
  object::ELFFile<T> &TheELF = *TheELFOrErr;

  revng_assert(Model->Architecture != model::Architecture::Invalid);
  Architecture = Model->Architecture;

  // BaseAddress makes sense only for shared (relocatable, PIC) objects
  auto Type = TheELF.getHeader().e_type;
  if (Type == ELF::ET_DYN) {
    BaseAddress = PreferedBaseAddress;
  }

  revng_assert(Type == ELF::ET_DYN or Type == ELF::ET_EXEC,
               "rev.ng currently handles executables and "
               "dynamic libraries only.");

  // Look for static or dynamic symbols and relocations
  using ConstElf_ShdrPtr = const typename object::ELFFile<T>::Elf_Shdr *;
  using Elf_PhdrPtr = const typename object::ELFFile<T>::Elf_Phdr *;
  ConstElf_ShdrPtr SymtabShdr = nullptr;
  Elf_PhdrPtr DynamicPhdr = nullptr;
  Optional<MetaAddress> DynamicAddress;
  Optional<MetaAddress> EHFrameAddress;
  Optional<uint64_t> EHFrameSize;
  Optional<MetaAddress> EHFrameHdrAddress;

  auto Sections = TheELF.sections();
  if (not Sections) {
    logAllUnhandledErrors(std::move(Sections.takeError()), errs(), "");
  } else {
    for (auto &Section : *Sections) {
      auto NameOrErr = TheELF.getSectionName(Section);
      if (NameOrErr) {
        auto &Name = *NameOrErr;
        if (Name == ".symtab") {
          // TODO: check dedicated field in section header
          revng_assert(SymtabShdr == nullptr, "Duplicate .symtab");
          SymtabShdr = &Section;
        } else if (Name == ".eh_frame") {
          revng_assert(not EHFrameAddress, "Duplicate .eh_frame");
          EHFrameAddress = relocate(fromGeneric(Section.sh_addr));
          EHFrameSize = static_cast<uint64_t>(Section.sh_size);
        } else if (Name == ".dynamic") {
          revng_assert(not DynamicAddress, "Duplicate .dynamic");
          DynamicAddress = relocate(fromGeneric(Section.sh_addr));
        }
      }
    }
  }

  // If we found a symbol table
  if (SymtabShdr != nullptr && SymtabShdr->sh_link != 0) {
    // Obtain a reference to the string table
    auto Strtab = TheELF.getSection(SymtabShdr->sh_link);
    if (not Strtab) {
      logAllUnhandledErrors(std::move(Strtab.takeError()), errs(), "");
      revng_abort();
    }
    auto StrtabArray = TheELF.getSectionContents(**Strtab);
    if (not StrtabArray) {
      logAllUnhandledErrors(std::move(StrtabArray.takeError()), errs(), "");
      revng_abort();
    }
    StringRef StrtabContent(reinterpret_cast<const char *>(StrtabArray->data()),
                            StrtabArray->size());

    // Collect symbol names
    auto ELFSymbols = TheELF.symbols(SymtabShdr);
    if (not ELFSymbols) {
      logAllUnhandledErrors(std::move(ELFSymbols.takeError()), errs(), "");
      revng_abort();
    }
    for (auto &Symbol : *ELFSymbols) {
      auto Name = Symbol.getName(StrtabContent);
      if (not Name) {
        logAllUnhandledErrors(std::move(Name.takeError()), errs(), "");
        revng_abort();
      }

      auto SymbolType = SymbolType::fromELF(Symbol.getType());
      if (shouldIgnoreSymbol(*Name) or Symbol.st_shndx == ELF::SHN_UNDEF)
        continue;

      MetaAddress Address = MetaAddress::invalid();

      if (SymbolType == SymbolType::Code)
        Address = relocate(fromPC(Symbol.st_value));
      else
        Address = relocate(fromGeneric(Symbol.st_value));

      if (SymbolType == SymbolType::Code) {
        auto It = Model->Functions.find(Address);
        if (It == Model->Functions.end())
          Model->Functions[Address].Type = model::FunctionType::Invalid;
      }

#if 0
      registerLabel(Label::createSymbol(LabelOrigin::StaticSymbol,
                                        Address,
                                        Symbol.st_size,
                                        *Name,
                                        SymbolType));
#endif
    }
  }

  const auto &ElfHeader = TheELF.getHeader();
  Model->EntryPoint = relocate(fromPC(ElfHeader.e_entry));

  // Loop over the program headers looking for PT_LOAD segments, read them out
  // and create a global variable for each one of them (writable or read-only),
  // assign them a section and output information about them in the linking info
  // CSV
  using Elf_Phdr = const typename object::ELFFile<T>::Elf_Phdr;
  using Elf_Dyn = const typename object::ELFFile<T>::Elf_Dyn;
  using Elf_Addr = const typename object::ELFFile<T>::Elf_Addr;

  auto ProgHeaders = TheELF.program_headers();
  if (not ProgHeaders) {
    logAllUnhandledErrors(std::move(ProgHeaders.takeError()), errs(), "");
    revng_abort();
  }

#if 0
  auto RawDataRef = ArrayRef<uint8_t>(TheELF.base(), TheELF.getBufSize());
#endif

  for (Elf_Phdr &ProgramHeader : *ProgHeaders) {
    switch (ProgramHeader.p_type) {
    case ELF::PT_LOAD: {
      auto Start = relocate(fromGeneric(ProgramHeader.p_vaddr));
      using namespace nooverflow;
      auto EndVirtualAddress = *add(Start, u64(ProgramHeader.p_memsz));
      model::Segment NewSegment({ Start, EndVirtualAddress });

      NewSegment.StartOffset = ProgramHeader.p_offset;
      NewSegment.EndOffset = *add(ProgramHeader.p_offset,
                                  ProgramHeader.p_filesz);

      NewSegment.IsReadable = ProgramHeader.p_flags & ELF::PF_R;
      NewSegment.IsWriteable = ProgramHeader.p_flags & ELF::PF_W;
      NewSegment.IsExecutable = ProgramHeader.p_flags & ELF::PF_X;

#if 0
      SegmentInfo Segment;
      auto Start = relocate(fromGeneric(ProgramHeader.p_vaddr));
      Segment.StartVirtualAddress = Start;
      Segment.EndVirtualAddress = Start + u64(ProgramHeader.p_memsz);
      Segment.StartFileOffset = ProgramHeader.p_offset;
      Segment.EndFileOffset = *add(ProgramHeader.p_offset,
                                   ProgramHeader.p_filesz);
      Segment.IsReadable = ProgramHeader.p_flags & ELF::PF_R;
      Segment.IsWriteable = ProgramHeader.p_flags & ELF::PF_W;
      Segment.IsExecutable = ProgramHeader.p_flags & ELF::PF_X;
      Segment.Data = ArrayRef<uint8_t>(*add(RawDataRef.begin(),
                                            ProgramHeader.p_offset),
                                       ProgramHeader.p_filesz);

      revng_assert(contains(RawDataRef, Segment.Data));
#endif

      // WIP: import all sections
      // WIP: use sections on the CodeGenerator.cpp side
#if 0
      // If it's an executable segment, and we've been asked so, register
      // which sections actually contain code
      if (Sections and not IgnoreDebugSymbols and NewSegment.IsExecutable) {
        using Elf_Shdr = const typename object::ELFFile<T>::Elf_Shdr;
        auto Inserter = std::back_inserter(Segment.ExecutableSections);
        for (Elf_Shdr &SectionHeader : *Sections) {
          if (SectionHeader.sh_flags & ELF::SHF_EXECINSTR) {
            auto SectionStart = relocate(fromGeneric(SectionHeader.sh_addr));
            auto SectionEnd = SectionStart + u64(SectionHeader.sh_size);
            Inserter = make_pair(SectionStart, SectionEnd);
          }
        }
      }
#endif

#if 0
      Segments.push_back(Segment);
#endif

      Model->Segments.insert(std::move(NewSegment));

#if 0
      // Check if it's the segment containing the program headers
      auto ProgramHeaderStart = ProgramHeader.p_offset;
      auto ProgramHeaderEnd = ProgramHeader.p_offset
                              + u64(ProgramHeader.p_filesz);
      if (ProgramHeaderStart <= ElfHeader.e_phoff
          && ElfHeader.e_phoff < ProgramHeaderEnd) {
        MetaAddress PhdrAddress = (relocate(fromGeneric(ProgramHeader.p_vaddr))
                                   + u64(ElfHeader.e_phoff)
                                   - u64(ProgramHeader.p_offset));
        ProgramHeaders.Address = PhdrAddress;
      }
#endif
    } break;

    case ELF::PT_GNU_EH_FRAME:
      revng_assert(!EHFrameHdrAddress);
      EHFrameHdrAddress = relocate(fromGeneric(ProgramHeader.p_vaddr));
      break;

    case ELF::PT_DYNAMIC:
      revng_assert(DynamicPhdr == nullptr, "Duplicate .dynamic program header");
      DynamicPhdr = &ProgramHeader;
      MetaAddress DynamicPhdrMA = relocate(fromGeneric(DynamicPhdr->p_vaddr));
      revng_assert(not DynamicAddress or DynamicPhdrMA == *DynamicAddress,
                   ".dynamic and PT_DYNAMIC have different addresses");
      DynamicAddress = relocate(DynamicPhdrMA);
      break;
    }
  }

  revng_assert((DynamicPhdr != nullptr) == (DynamicAddress.hasValue()));

  Optional<uint64_t> FDEsCount;
  if (EHFrameHdrAddress) {
    MetaAddress Address = MetaAddress::invalid();

    std::tie(Address,
             FDEsCount) = this->ehFrameFromEhFrameHdr<T>(*EHFrameHdrAddress);
    if (EHFrameAddress) {
      revng_assert(*EHFrameAddress == Address);
    }

    EHFrameAddress = Address;
  }
  if (EHFrameAddress)
    parseEHFrame<T>(*EHFrameAddress, FDEsCount, EHFrameSize);

  // Parse the .dynamic table
  if (DynamicPhdr != nullptr) {
    SmallVector<uint64_t, 10> NeededLibraryNameOffsets;

    FilePortion DynstrPortion(File);
    FilePortion DynsymPortion(File);
    FilePortion ReldynPortion(File);
    FilePortion RelpltPortion(File);
    FilePortion GotPortion(File);
    Optional<uint64_t> SymbolsCount;
    Optional<uint64_t> MIPSFirstGotSymbol;
    Optional<uint64_t> MIPSLocalGotEntries;
    bool IsMIPS = (Model->Architecture == model::Architecture::mips
                   or Model->Architecture == model::Architecture::mipsel);

    auto DynamicEntries = TheELF.dynamicEntries();
    if (not DynamicEntries) {
      logAllUnhandledErrors(std::move(DynamicEntries.takeError()), errs(), "");
      revng_abort();
    }
    for (Elf_Dyn &DynamicTag : *DynamicEntries) {

      auto TheTag = DynamicTag.getTag();
      MetaAddress Relocated = relocate(fromGeneric(DynamicTag.getPtr()));
      switch (TheTag) {
      case ELF::DT_NEEDED:
        NeededLibraryNameOffsets.push_back(DynamicTag.getVal());
        break;

      case ELF::DT_STRTAB:
        DynstrPortion.setAddress(Relocated);
        break;

      case ELF::DT_STRSZ:
        DynstrPortion.setSize(DynamicTag.getVal());
        break;

      case ELF::DT_SYMTAB:
        DynsymPortion.setAddress(Relocated);
        break;

      case ELF::DT_JMPREL:
        RelpltPortion.setAddress(Relocated);
        break;

      case ELF::DT_PLTRELSZ:
        RelpltPortion.setSize(DynamicTag.getVal());
        break;

      case ELF::DT_REL:
      case ELF::DT_RELA:
        revng_assert(TheTag == (HasAddend ? ELF::DT_RELA : ELF::DT_REL));
        ReldynPortion.setAddress(Relocated);
        break;

      case ELF::DT_RELSZ:
      case ELF::DT_RELASZ:
        revng_assert(TheTag == (HasAddend ? ELF::DT_RELASZ : ELF::DT_RELSZ));
        ReldynPortion.setSize(DynamicTag.getVal());
        break;

      case ELF::DT_PLTGOT:
        GotPortion.setAddress(Relocated);

        // WIP
#if 0
        // Obtaint the canonical value of the global pointer in MIPS
        if (IsMIPS)
          CanonicalValues["gp"] = (Relocated + 0x7ff0).address();
#endif
        break;

      case ELF::DT_MIPS_SYMTABNO:
        if (IsMIPS)
          SymbolsCount = DynamicTag.getVal();
        break;

      case ELF::DT_MIPS_GOTSYM:
        if (IsMIPS)
          MIPSFirstGotSymbol = DynamicTag.getVal();
        break;

      case ELF::DT_MIPS_LOCAL_GOTNO:
        if (IsMIPS)
          MIPSLocalGotEntries = DynamicTag.getVal();
        break;
      }
    }

#if 0
    if (NeededLibraryNames.size() > 0)
      revng_assert(DynstrPortion.isAvailable());
#endif

    // In MIPS the GOT has one entry per symbol
    if (IsMIPS and SymbolsCount and MIPSFirstGotSymbol
        and MIPSLocalGotEntries) {
      uint32_t GotEntries = (*MIPSLocalGotEntries
                             + (*SymbolsCount - *MIPSFirstGotSymbol));
      GotPortion.setSize(GotEntries * sizeof(Elf_Addr));
    }

    StringRef Dynstr;

    if (DynstrPortion.isAvailable()) {
      Dynstr = DynstrPortion.extractString();
      for (auto Offset : NeededLibraryNameOffsets) {
        StringRef LibraryName = Dynstr.slice(Offset, Dynstr.size());
        // WIP: use batch inserter
        Model->ImportedLibraries.insert(LibraryName.data());
      }
    }

    // Collect symbols count and code pointers in image base-relative
    // relocations

    if (not SymbolsCount) {
      SymbolsCount = std::max(symbolsCount<T, HasAddend>(ReldynPortion),
                              symbolsCount<T, HasAddend>(RelpltPortion));
    }

    // Collect function addresses contained in dynamic symbols
    if (SymbolsCount and *SymbolsCount > 0 and DynsymPortion.isAvailable()) {
      using Elf_Sym = llvm::object::Elf_Sym_Impl<T>;
      DynsymPortion.setSize(*SymbolsCount * sizeof(Elf_Sym));
      ArrayRef<Elf_Sym> Symbols = DynsymPortion.extractAs<Elf_Sym>();
      for (Elf_Sym Symbol : Symbols) {
        auto MaybeName = Symbol.getName(Dynstr);
        if (not MaybeName) {
          logAllUnhandledErrors(std::move(MaybeName.takeError()), errs(), "");
          revng_abort();
        }
        auto Name = *MaybeName;

        auto SymbolType = SymbolType::fromELF(Symbol.getType());

        if (shouldIgnoreSymbol(Name))
          continue;

        if (Symbol.st_shndx == ELF::SHN_UNDEF) {
          if (SymbolType == SymbolType::Code) {
            // Create dynamic function symbol
            Model->ImportedDynamicFunctions[Name.str()];
          } else {
            // TODO: create dynamic global variable
          }
        } else {
          MetaAddress Address = MetaAddress::invalid();

          if (SymbolType == SymbolType::Code) {
            Address = relocate(fromPC(Symbol.st_value));
            // WIP: record that's dynamic
            auto It = Model->Functions.find(Address);
            if (It == Model->Functions.end())
              Model->Functions[Address].Type = model::FunctionType::Invalid;
          } else {
            Address = relocate(fromGeneric(Symbol.st_value));
            // WIP: create field in segment struct
          }
        }

#if 0
        registerLabel(Label::createSymbol(LabelOrigin::DynamicSymbol,
                                          Address,
                                          Symbol.st_size,
                                          *Name,
                                          SymbolType));
#endif
      }

      using Elf_Rel = llvm::object::Elf_Rel_Impl<T, HasAddend>;
      if (ReldynPortion.isAvailable()) {
        auto Relocations = ReldynPortion.extractAs<Elf_Rel>();
        registerRelocations<T, HasAddend>(Relocations,
                                          DynsymPortion,
                                          DynstrPortion);
      }

      if (RelpltPortion.isAvailable()) {
        auto Relocations = RelpltPortion.extractAs<Elf_Rel>();
        registerRelocations<T, HasAddend>(Relocations,
                                          DynsymPortion,
                                          DynstrPortion);
      }

      if (IsMIPS and GotPortion.isAvailable()) {
        std::vector<Elf_Rel> MIPSImplicitRelocations;
        uint32_t GotIndex = 0;

        // Perform local relocations on GOT
        if (MIPSLocalGotEntries) {
          for (; GotIndex < *MIPSLocalGotEntries; GotIndex++) {
            auto Address = GotPortion.addressAtIndex<Elf_Addr>(GotIndex);
            Elf_Rel NewRelocation;
            NewRelocation.r_offset = Address.address();
            NewRelocation.setSymbolAndType(0, R_MIPS_IMPLICIT_RELATIVE, false);
            MIPSImplicitRelocations.push_back(NewRelocation);
          }
        }

        // Relocate the remaining entries of the GOT with global symbols
        if (MIPSFirstGotSymbol and SymbolsCount and DynstrPortion.isAvailable()
            and DynsymPortion.isAvailable()) {
          for (uint32_t SymbolIndex = *MIPSFirstGotSymbol;
               SymbolIndex < *SymbolsCount;
               SymbolIndex++, GotIndex++) {
            auto Address = GotPortion.addressAtIndex<Elf_Addr>(GotIndex);

            Elf_Rel NewRelocation;
            NewRelocation.r_offset = Address.address();
            NewRelocation.setSymbolAndType(SymbolIndex,
                                           llvm::ELF::R_MIPS_JUMP_SLOT,
                                           false);
            MIPSImplicitRelocations.push_back(NewRelocation);
          }
        }

        auto Relocations = ArrayRef<Elf_Rel>(MIPSImplicitRelocations);
        registerRelocations<T, HasAddend>(Relocations,
                                          DynsymPortion,
                                          DynstrPortion);
      }
    }
  }

#if 0
  for (Label &L : Labels) {
    MetaAddress MA = MetaAddress::invalid();

    if (L.isSymbol() and L.isCode())
      MA = relocate(L.address());
    else if (L.isBaseRelativeValue())
      MA = relocate(fromPC(L.value()));

    if (MA.isValid())
      CodePointers.insert(MA);
  }
#endif
}

// WIP
#include "DwarfReader.h"

template<typename T>
std::pair<MetaAddress, uint64_t>
ELFImporter::ehFrameFromEhFrameHdr(MetaAddress EHFrameHdrAddress) {
  auto EHFrameHdr = File.getFromAddressOn(EHFrameHdrAddress);
  revng_assert(not isNull(EHFrameHdr),
               ".eh_frame_hdr section not available in any segment");

  DwarfReader<T> EHFrameHdrReader(model::Architecture::toLLVMArchitecture(
                                    Architecture),
                                  EHFrameHdr,
                                  EHFrameHdrAddress);

  uint64_t VersionNumber = EHFrameHdrReader.readNextU8();
  revng_assert(VersionNumber == 1);

  // ExceptionFrameEncoding
  uint64_t ExceptionFrameEncoding = EHFrameHdrReader.readNextU8();

  // FDEsCountEncoding
  unsigned FDEsCountEncoding = EHFrameHdrReader.readNextU8();

  // LookupTableEncoding
  EHFrameHdrReader.readNextU8();

  Pointer EHFramePointer = EHFrameHdrReader.readPointer(ExceptionFrameEncoding);
  uint64_t FDEsCount = EHFrameHdrReader.readUnsignedValue(FDEsCountEncoding);

  return { getGenericPointer<T>(EHFramePointer), FDEsCount };
}

template<typename T>
void ELFImporter::parseEHFrame(MetaAddress EHFrameAddress,
                               Optional<uint64_t> FDEsCount,
                               Optional<uint64_t> EHFrameSize) {
  revng_assert(FDEsCount || EHFrameSize);

  // Sometimes the .eh_frame section is present but not mapped in memory. This
  // means it cannot be used at runtime, therefore we can ignore it.
  llvm::ArrayRef<uint8_t> EHFrame = File.getFromAddressOn(EHFrameAddress);

  if (isNull(EHFrame))
    return;

  auto Architecture = model::Architecture::toLLVMArchitecture(
    Model->Architecture);

  DwarfReader<T> EHFrameReader(Architecture, EHFrame, EHFrameAddress);

  // A few fields of the CIE are used when decoding the FDE's.  This struct
  // will cache those fields we need so that we don't have to decode it
  // repeatedly for each FDE that references it.
  struct DecodedCIE {
    Optional<uint32_t> FDEPointerEncoding;
    Optional<uint32_t> LSDAPointerEncoding;
    bool HasAugmentationLength;
  };

  // Map from the start offset of the CIE to the cached data for that CIE.
  DenseMap<uint64_t, DecodedCIE> CachedCIEs;
  unsigned FDEIndex = 0;

  while (!EHFrameReader.eof()
         && ((FDEsCount && FDEIndex < *FDEsCount)
             || (EHFrameSize && EHFrameReader.offset() < *EHFrameSize))) {

    uint64_t StartOffset = EHFrameReader.offset();

    // Read the length of the entry
    uint64_t Length = EHFrameReader.readNextU32();
    if (Length == 0xffffffff)
      Length = EHFrameReader.readNextU64();

    // Compute the end offset of the entry
    uint64_t OffsetAfterLength = EHFrameReader.offset();
    uint64_t EndOffset = OffsetAfterLength + Length;

    // Zero-sized entry, skip it
    if (Length == 0) {
      revng_assert(EHFrameReader.offset() == EndOffset);
      continue;
    }

    // Get the entry ID, 0 means it's a CIE, otherwise it's a FDE
    uint32_t ID = EHFrameReader.readNextU32();
    if (ID == 0) {
      // This is a CIE
      revng_log(EhFrameLog, "New CIE");

      // Ensure the version is the one we expect
      uint32_t Version = EHFrameReader.readNextU8();
      revng_assert(Version == 1);

      // Parse a null terminated augmentation string
      SmallString<8> AugmentationString;
      for (uint8_t Char = EHFrameReader.readNextU8(); Char != 0;
           Char = EHFrameReader.readNextU8())
        AugmentationString.push_back(Char);

      // Optionally parse the EH data if the augmentation string says it's
      // there
      if (StringRef(AugmentationString).count("eh") != 0)
        EHFrameReader.readNextU();

      // CodeAlignmentFactor
      EHFrameReader.readULEB128();

      // DataAlignmentFactor
      EHFrameReader.readULEB128();

      // ReturnAddressRegister
      EHFrameReader.readNextU8();

      Optional<uint64_t> AugmentationLength;
      Optional<uint32_t> LSDAPointerEncoding;
      Optional<uint32_t> PersonalityEncoding;
      Optional<uint32_t> FDEPointerEncoding;
      if (!AugmentationString.empty() && AugmentationString.front() == 'z') {
        AugmentationLength = EHFrameReader.readULEB128();

        // Walk the augmentation string to get all the augmentation data.
        for (unsigned I = 1, E = AugmentationString.size(); I != E; ++I) {
          char Char = AugmentationString[I];
          switch (Char) {
          case 'e':
            revng_assert((I + 1) != E && AugmentationString[I + 1] == 'h',
                         "Expected 'eh' in augmentation string");
            break;
          case 'L':
            // This is the only information we really care about, all the rest
            // is processed just so we can get here
            revng_assert(!LSDAPointerEncoding, "Duplicate LSDA encoding");
            LSDAPointerEncoding = EHFrameReader.readNextU8();
            break;
          case 'P': {
            revng_assert(!PersonalityEncoding, "Duplicate personality");
            PersonalityEncoding = EHFrameReader.readNextU8();
            // Personality
            Pointer Personality;
            Personality = EHFrameReader.readPointer(*PersonalityEncoding);
            auto PersonalityPtr = getCodePointer<T>(Personality);
            logAddress(EhFrameLog, "Personality function: ", PersonalityPtr);

            // Register in the model for exploration
            Model->ExtraCodeAddresses.insert(PersonalityPtr);
#if 0
            // TODO: technically this is not a landing pad
            LandingPads.insert(PersonalityPtr);
#endif
            break;
          }
          case 'R':
            revng_assert(!FDEPointerEncoding, "Duplicate FDE encoding");
            FDEPointerEncoding = EHFrameReader.readNextU8();
            break;
          case 'z':
            revng_unreachable("'z' must be first in the augmentation string");
          }
        }
      }

      // Cache this entry
      CachedCIEs[StartOffset] = { FDEPointerEncoding,
                                  LSDAPointerEncoding,
                                  AugmentationLength.hasValue() };

    } else {
      // This is an FDE
      FDEIndex++;

      // The CIE pointer for an FDE is the same location as the ID which we
      // already read
      uint64_t CIEOffset = OffsetAfterLength - ID;

      // Ensure we already met this CIE
      auto CIEIt = CachedCIEs.find(CIEOffset);
      revng_assert(CIEIt != CachedCIEs.end(),
                   "Couldn't find CIE at offset in to __eh_frame section");

      // Ensure we have at least the pointer encoding
      const DecodedCIE &CIE = CIEIt->getSecond();
      revng_assert(CIE.FDEPointerEncoding,
                   "FDE references CIE which did not set pointer encoding");

      // PCBegin
      auto PCBeginPointer = EHFrameReader.readPointer(*CIE.FDEPointerEncoding);
      MetaAddress PCBegin = getGenericPointer<T>(PCBeginPointer);
      logAddress(EhFrameLog, "PCBegin: ", PCBegin);

      // PCRange
      EHFrameReader.readPointer(*CIE.FDEPointerEncoding);

      if (CIE.HasAugmentationLength)
        EHFrameReader.readULEB128();

      // Decode the LSDA if the CIE augmentation string said we should.
      if (CIE.LSDAPointerEncoding) {
        auto LSDAPointer = EHFrameReader.readPointer(*CIE.LSDAPointerEncoding);
        parseLSDA<T>(PCBegin, getGenericPointer<T>(LSDAPointer));
      }
    }

    // Skip all the remaining parts
    EHFrameReader.moveTo(EndOffset);
  }
}

template<typename T>
void ELFImporter::parseLSDA(MetaAddress FDEStart, MetaAddress LSDAAddress) {
  logAddress(EhFrameLog, "LSDAAddress: ", LSDAAddress);

  llvm::ArrayRef<uint8_t> LSDA = File.getFromAddressOn(LSDAAddress);
  revng_assert(not isNull(LSDA), "LSDA not available in any segment");

  auto Architecture = model::Architecture::toLLVMArchitecture(
    Model->Architecture);
  DwarfReader<T> LSDAReader(Architecture, LSDA, LSDAAddress);

  uint32_t LandingPadBaseEncoding = LSDAReader.readNextU8();
  MetaAddress LandingPadBase = MetaAddress::invalid();
  if (LandingPadBaseEncoding != dwarf::DW_EH_PE_omit) {
    auto LandingPadBasePointer = LSDAReader.readPointer(LandingPadBaseEncoding);
    LandingPadBase = getGenericPointer<T>(LandingPadBasePointer);
  } else {
    LandingPadBase = FDEStart;
  }

  logAddress(EhFrameLog, "LandingPadBase: ", LandingPadBase);

  uint32_t TypeTableEncoding = LSDAReader.readNextU8();
  if (TypeTableEncoding != dwarf::DW_EH_PE_omit)
    LSDAReader.readULEB128();

  uint32_t CallSiteTableEncoding = LSDAReader.readNextU8();
  uint64_t CallSiteTableLength = LSDAReader.readULEB128();
  uint64_t CallSiteTableEnd = LSDAReader.offset() + CallSiteTableLength;

  while (LSDAReader.offset() < CallSiteTableEnd) {
    // InstructionStart
    LSDAReader.readPointer(CallSiteTableEncoding);

    // InstructionEnd
    LSDAReader.readPointer(CallSiteTableEncoding);

    // LandingPad
    Pointer LandingPadPointer = LSDAReader.readPointer(CallSiteTableEncoding,
                                                       LandingPadBase);
    MetaAddress LandingPad = getCodePointer<T>(LandingPadPointer);

    // Action
    LSDAReader.readULEB128();

    if (LandingPad.isValid()) {
      auto &ExtraCodeAddresses = Model->ExtraCodeAddresses;
      if (ExtraCodeAddresses.count(LandingPad) == 0)
        logAddress(EhFrameLog, "New landing pad found: ", LandingPad);

      ExtraCodeAddresses.insert(LandingPad);
    }
  }
}

template<typename T, bool HasAddend>
struct RelocationHelper {
  static uint64_t getAddend(llvm::object::Elf_Rel_Impl<T, HasAddend>);
};

template<typename T>
struct RelocationHelper<T, true> {
  static uint64_t getAddend(llvm::object::Elf_Rel_Impl<T, true> Relocation) {
    return Relocation.r_addend;
  }
};

template<typename T>
struct RelocationHelper<T, false> {
  static uint64_t getAddend(llvm::object::Elf_Rel_Impl<T, false>) { return 0; }
};

template<typename T, bool HasAddend>
void ELFImporter::registerRelocations(Elf_Rel_Array<T, HasAddend> Relocations,
                                      const FilePortion &Dynsym,
                                      const FilePortion &Dynstr) {
  using Elf_Rel = llvm::object::Elf_Rel_Impl<T, HasAddend>;
  using Elf_Sym = llvm::object::Elf_Sym_Impl<T>;

  model::Segment *LowestSegment = nullptr;
  if (auto It = Model->Segments.begin(); It != Model->Segments.end())
    LowestSegment = &*It;

  ArrayRef<Elf_Sym> Symbols;
  if (Dynsym.isAvailable())
    Symbols = Dynsym.extractAs<Elf_Sym>();

  for (Elf_Rel Relocation : Relocations) {
    auto Type = static_cast<unsigned char>(Relocation.getType(false));
    uint64_t Addend = RelocationHelper<T, HasAddend>::getAddend(Relocation);
    MetaAddress Address = relocate(fromGeneric(Relocation.r_offset));

    StringRef SymbolName;
    uint64_t SymbolSize = 0;
    unsigned char SymbolType = llvm::ELF::STT_NOTYPE;
    if (Dynsym.isAvailable() and Dynstr.isAvailable()) {
      uint32_t SymbolIndex = Relocation.getSymbol(false);
      revng_check(SymbolIndex < Symbols.size());
      const Elf_Sym &Symbol = Symbols[SymbolIndex];
      auto MaybeName = Symbol.getName(Dynstr.extractString());
      if (MaybeName)
        SymbolName = *MaybeName;
      SymbolSize = Symbol.st_size;
      SymbolType = Symbol.getType();
    }

    auto RelocationType = model::RelocationType::
      fromELFRelocation(Model->Architecture, Type);
    // WIP: handle RelocationType == model::RelocationType::Invalid
    model::Relocation NewRelocation(Address, RelocationType, Addend);

    if (SymbolName.size() != 0) {
      if (SymbolType == SymbolType::Code) {
        auto It = Model->ImportedDynamicFunctions.find(SymbolName.str());
        if (It != Model->ImportedDynamicFunctions.end()) {
          auto &Relocations = It->Relocations;
          auto RelocationType = model::RelocationType::
            fromELFRelocation(Model->Architecture, Type);
          // WIP: handle RelocationType == model::RelocationType::Invalid
          Relocations.insert(NewRelocation);
        }
      } else {
        // TODO: register relocation for dynamic global variable
      }
    } else {
      // WIP: ensure the relocation is not symbol relative
      // We have a base relative relocation
      if (LowestSegment != nullptr) {
        LowestSegment->Relocations.insert(NewRelocation);
      } else {
        // WIP: warning
      }
    }

#if 0
    registerLabel(parseRelocation(Type,
                                  Address,
                                  Addend,
                                  SymbolName,
                                  SymbolSize,
                                  SymbolType::fromELF(SymbolType)));
#endif
  }
}

void importELF(TupleTree<model::Binary> &Model,
               const object::ObjectFile &TheBinary,
               uint64_t PreferedBaseAddress) {
  ELFImporter X(Model, TheBinary, PreferedBaseAddress);

  bool IsLittleEndian = model::Architecture::isLittleEndian(
    Model->Architecture);
  size_t PointerSize = model::Architecture::getPointerSize(Model->Architecture);
  bool HasRelocationAddend = model::Architecture::hasELFRelocationAddend(
    Model->Architecture);

  if (PointerSize == 4) {
    if (IsLittleEndian) {
      if (HasRelocationAddend) {
        X.import<object::ELF32LE, true>();
      } else {
        X.import<object::ELF32LE, false>();
      }
    } else {
      if (HasRelocationAddend) {
        X.import<object::ELF32BE, true>();
      } else {
        X.import<object::ELF32BE, false>();
      }
    }
  } else if (PointerSize == 8) {
    if (IsLittleEndian) {
      if (HasRelocationAddend) {
        X.import<object::ELF64LE, true>();
      } else {
        X.import<object::ELF64LE, false>();
      }
    } else {
      if (HasRelocationAddend) {
        X.import<object::ELF64BE, true>();
      } else {
        X.import<object::ELF64BE, false>();
      }
    }
  } else {
    revng_abort("Unexpect address size");
  }
}
