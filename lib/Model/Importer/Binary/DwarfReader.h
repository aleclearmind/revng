#pragma once

#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/LEB128.h"

// WIP: this file assumes using namespace llvm

// WIP: move to DwarfReader.h
//
// .eh_frame-related functions
//
template<typename E>
class DwarfReader {
public:
  DwarfReader(Triple::ArchType Architecture,
              ArrayRef<uint8_t> Buffer,
              MetaAddress Address) :
    Architecture(Architecture),
    Address(Address),
    Start(Buffer.data()),
    Cursor(Buffer.data()),
    End(Buffer.data() + Buffer.size()) {}

  uint8_t readNextU8() { return readNext<uint8_t>(); }
  uint16_t readNextU16() { return readNext<uint16_t>(); }
  uint32_t readNextU32() { return readNext<uint32_t>(); }
  uint64_t readNextU64() { return readNext<uint64_t>(); }
  uint64_t readNextU() {
    if (is64())
      return readNextU64();
    else
      return readNextU32();
  }

  uint64_t readULEB128() {
    unsigned Length;
    uint64_t Result = decodeULEB128(Cursor, &Length);
    Cursor += Length;
    revng_assert(Cursor <= End);
    return Result;
  }

  int64_t readSLEB128() {
    unsigned Length;
    int64_t Result = decodeSLEB128(Cursor, &Length);
    Cursor += Length;
    revng_assert(Cursor <= End);
    return Result;
  }

  int64_t readSignedValue(unsigned Encoding) {
    return static_cast<int64_t>(readValue(Encoding));
  }

  uint64_t readUnsignedValue(unsigned Encoding) {
    return static_cast<uint64_t>(readValue(Encoding));
  }

  Pointer
  readPointer(unsigned Encoding, MetaAddress Base = MetaAddress::invalid()) {
    revng_assert((Encoding & ~(0x70 | 0x0F | dwarf::DW_EH_PE_indirect)) == 0);

    // Handle PC-relative values
    revng_assert(Cursor >= Start);
    if ((Encoding & 0x70) == dwarf::DW_EH_PE_pcrel) {
      revng_assert(Base.isInvalid());
      Base = Address + (Cursor - Start);
    }

    if (isSigned(Encoding & 0x0F)) {
      return readPointerInternal(readSignedValue(Encoding), Encoding, Base);
    } else {
      return readPointerInternal(readUnsignedValue(Encoding), Encoding, Base);
    }
  }

  void moveTo(uint64_t Offset) {
    const uint8_t *NewCursor = Start + Offset;
    revng_assert(NewCursor >= Cursor && NewCursor <= End);
    Cursor = NewCursor;
  }

  bool eof() const { return Cursor >= End; }
  uint64_t offset() const { return Cursor - Start; }

private:
  template<typename T>
  std::conditional_t<std::numeric_limits<T>::is_signed, int64_t, uint64_t>
  readNext() {
    constexpr bool IsSigned = std::numeric_limits<T>::is_signed;
    using ReturnType = std::conditional_t<IsSigned, int64_t, uint64_t>;
    revng_assert(Cursor + sizeof(T) <= End);
    auto Result = static_cast<T>(Endianess<T, E>::read(Cursor));
    Cursor += sizeof(T);
    return static_cast<ReturnType>(Result);
  }

  static bool isSigned(unsigned Format) {
    switch (Format) {
    case dwarf::DW_EH_PE_sleb128:
    case dwarf::DW_EH_PE_signed:
    case dwarf::DW_EH_PE_sdata2:
    case dwarf::DW_EH_PE_sdata4:
    case dwarf::DW_EH_PE_sdata8:
      return true;
    case dwarf::DW_EH_PE_absptr:
    case dwarf::DW_EH_PE_uleb128:
    case dwarf::DW_EH_PE_udata2:
    case dwarf::DW_EH_PE_udata4:
    case dwarf::DW_EH_PE_udata8:
      return false;
    default:
      revng_abort("Unknown Encoding");
    }
  }

  uint64_t readValue(unsigned Encoding) {
    revng_assert((Encoding & ~(0x70 | 0x0F | dwarf::DW_EH_PE_indirect)) == 0);

    // Extract the format
    unsigned Format = Encoding & 0x0F;
    switch (Format) {
    case dwarf::DW_EH_PE_uleb128:
      return readULEB128();
    case dwarf::DW_EH_PE_sleb128:
      return readSLEB128();
    case dwarf::DW_EH_PE_absptr:
      if (is64())
        return readNext<uint64_t>();
      else
        return readNext<uint32_t>();
    case dwarf::DW_EH_PE_signed:
      if (is64())
        return readNext<int64_t>();
      else
        return readNext<int32_t>();
    case dwarf::DW_EH_PE_udata2:
      return readNext<uint16_t>();
    case dwarf::DW_EH_PE_sdata2:
      return readNext<int16_t>();
    case dwarf::DW_EH_PE_udata4:
      return readNext<uint32_t>();
    case dwarf::DW_EH_PE_sdata4:
      return readNext<int32_t>();
    case dwarf::DW_EH_PE_udata8:
      return readNext<uint64_t>();
    case dwarf::DW_EH_PE_sdata8:
      return readNext<int64_t>();
    default:
      revng_unreachable("Unknown Encoding");
    }
  }

  template<typename T>
  Pointer readPointerInternal(T Value, unsigned Encoding, MetaAddress Base) {
    bool IsIndirect = Encoding & dwarf::DW_EH_PE_indirect;

    if (Base.isInvalid()) {
      return Pointer(IsIndirect, MetaAddress::fromGeneric(Architecture, Value));
    } else {
      unsigned EncodingRelative = Encoding & 0x70;
      revng_assert(EncodingRelative == 0 || EncodingRelative == 0x10);
      return Pointer(IsIndirect, Base + Value);
    }
  }

  bool is64() const;

private:
  Triple::ArchType Architecture;
  MetaAddress Address;
  const uint8_t *Start;
  const uint8_t *Cursor;
  const uint8_t *End;
};

template<>
bool DwarfReader<object::ELF32BE>::is64() const {
  return false;
}
template<>
bool DwarfReader<object::ELF32LE>::is64() const {
  return false;
}
template<>
bool DwarfReader<object::ELF64BE>::is64() const {
  return true;
}
template<>
bool DwarfReader<object::ELF64LE>::is64() const {
  return true;
}
