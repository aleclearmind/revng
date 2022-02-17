/// \file PECOFF.cpp
/// \brief

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/Object/MachO.h"
#include "llvm/Object/ObjectFile.h"

#include "revng/Support/Debug.h"
#include "revng/Model/Binary.h"

// WIP
#include "MyFile.h"
#include "Importers.h"

using namespace llvm;

template<typename R>
inline void swapBytes(R &Value) {
  swapStruct(Value);
}

template<>
inline void swapBytes<uint32_t>(uint32_t &Value) {
  sys::swapByteOrder(Value);
}

template<typename T>
class ArrayRefReader {
private:
  ArrayRef<T> Array;
  const T *Cursor;
  bool Swap;

public:
  ArrayRefReader(ArrayRef<T> Array, bool Swap) :
    Array(Array), Cursor(Array.begin()), Swap(Swap) {}

  bool eof() const { return Cursor == Array.end(); }

  template<typename R>
  R read() {
    revng_check(Cursor + sizeof(R) > Cursor);
    revng_check(Cursor + sizeof(R) <= Array.end());

    R Result;
    memcpy(&Result, Cursor, sizeof(R));

    if (Swap)
      swapBytes<R>(Result);

    Cursor += sizeof(R);

    return Result;
  }
};

static MetaAddress
getInitialPC(model::Architecture::Values Arch, bool Swap, ArrayRef<uint8_t> Command) {
  using namespace llvm::MachO;

  ArrayRefReader<uint8_t> Reader(Command, Swap);
  uint32_t Flavor = Reader.read<uint32_t>();
  uint32_t Count = Reader.read<uint32_t>();
  Optional<uint64_t> PC;

  switch (Arch) {
  case model::Architecture::x86: {

    switch (Flavor) {
    case MachO::x86_THREAD_STATE32:
      revng_check(Count == MachO::x86_THREAD_STATE32_COUNT);
      PC = Reader.read<x86_thread_state32_t>().eip;
      break;

    case MachO::x86_THREAD_STATE:
      revng_check(Count == MachO::x86_THREAD_STATE_COUNT);
      PC = Reader.read<x86_thread_state_t>().uts.ts32.eip;
      break;

    default:
      revng_abort();
    }

    revng_check(Reader.eof());

  } break;

  case model::Architecture::x86_64: {

    switch (Flavor) {
    case MachO::x86_THREAD_STATE64:
      revng_check(Count == MachO::x86_THREAD_STATE64_COUNT);
      PC = Reader.read<x86_thread_state64_t>().rip;
      break;

    case MachO::x86_THREAD_STATE:
      revng_check(Count == MachO::x86_THREAD_STATE_COUNT);
      PC = Reader.read<x86_thread_state_t>().uts.ts64.rip;
      break;

    default:
      revng_abort();
    }

  } break;

  case model::Architecture::arm: {

    switch (Flavor) {
    case MachO::ARM_THREAD_STATE:
      revng_check(Count == MachO::ARM_THREAD_STATE_COUNT);
      PC = Reader.read<arm_thread_state_t>().uts.ts32.pc;
      break;

    default:
      revng_abort();
    }

  } break;

  case model::Architecture::aarch64: {

    switch (Flavor) {
    case MachO::ARM_THREAD_STATE64:
      revng_check(Count == MachO::ARM_THREAD_STATE64_COUNT);
      PC = Reader.read<arm_thread_state64_t>().pc;
      break;

    default:
      revng_abort();
    }

  } break;

  default:
    revng_abort("Unexpected architecture for Mach-O");
    break;
  }

  revng_check(Reader.eof());

  if (PC)
    return MetaAddress::fromPC(model::Architecture::toLLVMArchitecture(Arch), *PC);
  else
    return MetaAddress::invalid();
}

class MachOImporter : public Lol {
private:
  MyFile File;
  TupleTree<model::Binary> &Model;
  const object::ObjectFile &TheBinary;
  uint64_t PreferedBaseAddress;

public:
  MachOImporter(TupleTree<model::Binary> &Model,
                const object::ObjectFile &TheBinary,
                uint64_t PreferedBaseAddress) : File(*Model, {}),
                                                Model(Model),
                                                TheBinary(TheBinary),
                                                PreferedBaseAddress(PreferedBaseAddress) {}

  void import() {
    (void) File;
    (void) Model;
    (void) TheBinary;
    (void) PreferedBaseAddress;

    // WIP: set PreferedBaseAddress if PIC

    using namespace llvm::MachO;
    using namespace llvm::object;
    using LoadCommandInfo = MachOObjectFile::LoadCommandInfo;

    auto &MachO = cast<object::MachOObjectFile>(TheBinary);
    
    revng_assert(Model->Architecture != model::Architecture::Invalid);
    bool IsLittleEndian = model::Architecture::isLittleEndian(Model->Architecture);
    StringRef StringDataRef = TheBinary.getData();
    auto RawDataRef = ArrayRef<uint8_t>(StringDataRef.bytes_begin(),
                                        StringDataRef.size());
    bool MustSwap = IsLittleEndian != sys::IsLittleEndianHost;

    bool EntryPointFound = false;
    Optional<uint64_t> EntryPointOffset;
    for (const LoadCommandInfo &LCI : MachO.load_commands()) {
      switch (LCI.C.cmd) {

      case LC_SEGMENT:
        parseMachOSegment(RawDataRef, MachO.getSegmentLoadCommand(LCI));
        break;

      case LC_SEGMENT_64:
        parseMachOSegment(RawDataRef, MachO.getSegment64LoadCommand(LCI));
        break;

      case LC_UNIXTHREAD: {
        revng_check(not EntryPointFound);
        EntryPointFound = true;
        const uint8_t *Pointer = reinterpret_cast<const uint8_t *>(LCI.Ptr);
        ArrayRef<uint8_t> CommandBuffer(Pointer + sizeof(thread_command),
                                        LCI.C.cmdsize - sizeof(thread_command));
        // WIP: review all contains()
#if 0
        revng_check(contains(RawDataRef, CommandBuffer));
#endif

        Model->EntryPoint = getInitialPC(Model->Architecture, MustSwap, CommandBuffer);
      } break;

      case LC_MAIN:
        revng_check(not EntryPointFound);
        EntryPointFound = true;

        // This is an offset, delay translation to code for later
        EntryPointOffset = MachO.getEntryPointCommand(LCI).entryoff;
        break;

      case LC_FUNCTION_STARTS:
      case LC_DATA_IN_CODE:
      case LC_SYMTAB:
      case LC_DYSYMTAB:
        // TODO: very interesting
        break;
      }
    }

#if 0
    if (EntryPointOffset)
      Model->EntryPoint = virtualAddressFromOffset(*EntryPointOffset).toPC(Arch);

    const uint64_t PointerSize = TheArchitecture.pointerSize() / 8;

    Error TheError = Error::success();

    for (const MachOBindEntry &U : MachO.bindTable(TheError))
      registerBindEntry(&U, PointerSize);
    revng_check(not TheError);

    for (const MachOBindEntry &U : MachO.lazyBindTable(TheError))
      registerBindEntry(&U, PointerSize);
    revng_check(not TheError);

    // TODO: we should handle weak symbols
    for (const MachOBindEntry &U : MachO.weakBindTable(TheError))
      registerBindEntry(&U, PointerSize);
    revng_check(not TheError);
#endif
  }

  template<typename T>
  void parseMachOSegment(ArrayRef<uint8_t> RawDataRef,
                         const T &SegmentCommand);
  
};

template<typename T>
inline void MachOImporter::parseMachOSegment(ArrayRef<uint8_t> RawDataRef,
                                             const T &SegmentCommand) {
  using namespace nooverflow;
  using namespace llvm::MachO;

  MetaAddress Start = fromGeneric(SegmentCommand.vmaddr);
  MetaAddress End = Start + SegmentCommand.vmsize;
  model::Segment Segment({ Start, End });

  Segment.StartOffset = SegmentCommand.fileoff;
  // WIP: no assert on overflow
  Segment.EndOffset = *add(SegmentCommand.fileoff, SegmentCommand.filesize);

  Segment.IsReadable = SegmentCommand.initprot & VM_PROT_READ;
  Segment.IsWriteable = SegmentCommand.initprot & VM_PROT_WRITE;
  Segment.IsExecutable = SegmentCommand.initprot & VM_PROT_EXECUTE;

  Model->Segments.insert(std::move(Segment));
#if 0

  SegmentInfo Segment;
  Segment.StartVirtualAddress = fromGeneric(SegmentCommand.vmaddr);
  Segment.EndVirtualAddress = fromGeneric(SegmentCommand.vmaddr)
                              + SegmentCommand.vmsize;
  Segment.StartFileOffset = SegmentCommand.fileoff;
  Segment.EndFileOffset = *add(SegmentCommand.fileoff, SegmentCommand.filesize);
  Segment.IsExecutable = SegmentCommand.initprot & VM_PROT_EXECUTE;
  Segment.IsReadable = SegmentCommand.initprot & VM_PROT_READ;
  Segment.IsWriteable = SegmentCommand.initprot & VM_PROT_WRITE;
  Segment.Data = ArrayRef<uint8_t>(*add(RawDataRef.begin(),
                                        SegmentCommand.fileoff),
                                   SegmentCommand.filesize);
  revng_assert(contains(RawDataRef, Segment.Data));

  Segments.push_back(Segment);
#endif
}

void importMachO(TupleTree<model::Binary> &Model,
                 const object::ObjectFile &TheBinary,
                 uint64_t PreferedBaseAddress) {
  MachOImporter X(Model, TheBinary, PreferedBaseAddress);
  X.import();
}
