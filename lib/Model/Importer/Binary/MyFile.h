#pragma once

// WIP: header
// WIP: rename/split file

// WIP: rename and don't inherit?
class Lol {
protected:
  model::Architecture::Values Architecture = model::Architecture::Invalid;
  uint64_t BaseAddress = 0;

public:
  MetaAddress relocate(MetaAddress Address) const {
    if (BaseAddress) {
      // WIP: overflow
      return Address + BaseAddress;
    } else {
      return Address;
    }
  }

  MetaAddress relocate(uint64_t Address) const {
    return relocate(fromGeneric(Address));
  }

  MetaAddress fromPC(uint64_t PC) const {
    return MetaAddress::fromPC(model::Architecture::toLLVMArchitecture(
                                 Architecture),
                               PC);
  }

  MetaAddress fromGeneric(uint64_t Address) const {
    return MetaAddress::fromGeneric(model::Architecture::toLLVMArchitecture(
                                      Architecture),
                                    Address);
  }
};

inline uint64_t u64(uint64_t Value) {
  return Value;
}

namespace nooverflow {

template<typename T, typename U>
auto add(T LHS, U RHS) -> std::optional<decltype(LHS + RHS)> {
  using V = decltype(LHS + RHS);
  V Result = LHS + RHS;

  if (Result < LHS)
    return {};

  return Result;
}

} // namespace nooverflow

class MyFile {
private:
  const model::Binary &Binary;
  llvm::ArrayRef<uint8_t> Data;

public:
  MyFile(const model::Binary &Binary, llvm::ArrayRef<uint8_t> Data) :
    Binary(Binary), Data(Data) {}

public:
  uint64_t size() { return Data.size(); }

  llvm::ArrayRef<uint8_t> getByOffset(uint64_t Offset, uint64_t Size) const {
    using namespace nooverflow;
    auto MaybeSum = add(Offset, Size);
    if (not MaybeSum or *MaybeSum >= Data.size())
      return {};
    return Data.slice(Offset, Size);
  }

  llvm::ArrayRef<uint8_t>
  getByAddress(MetaAddress Address, uint64_t Size) const {
    auto MaybeOffset = addressToOffset(Address);
    if (not MaybeOffset)
      return {};

    return getByOffset(*MaybeOffset, Size);
  }

  llvm::ArrayRef<uint8_t> getFromAddressOn(MetaAddress Address) const {
    auto [Segment, OffsetInSegment] = findOffsetInSegment(Address, 0);
    if (Segment == nullptr)
      return {};

    // WIP: overflow
    // WIP: within bounds?
    auto StartOffset = Segment->StartOffset + OffsetInSegment;
    return Data.slice(StartOffset, Segment->EndOffset - StartOffset);
  }

  /// \note This function ignores the underlying data, it just performs address
  ///       translation.
  std::optional<uint64_t>
  addressToOffset(MetaAddress Address, uint64_t Size = 0) const {
    auto [Segment, OffsetInSegment] = findOffsetInSegment(Address, Size);
    if (Segment == nullptr) {
      return {};
    } else {
      // WIP: overflow
      return Segment->StartOffset + OffsetInSegment;
    }
  }

private:
  std::pair<const model::Segment *, uint64_t>
  findOffsetInSegment(MetaAddress Address, uint64_t Size) const {
    const model::Segment *Match = nullptr;
    for (const model::Segment &Segment : Binary.Segments) {
      if (Segment.contains(Address, Size)) {

        if (Match != nullptr) {
          // We have more than one match!
          return { nullptr, 0 };
        }

        Match = &Segment;
      }
    }

    if (Match != nullptr) {
      // WIP: overflow
      uint64_t Offset = Address - Match->StartAddress;
      return { Match, Offset };
    } else {
      // No match
      return { nullptr, 0 };
    }
  }
};
