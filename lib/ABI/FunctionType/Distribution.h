#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/ADT/SmallVector.h"

namespace abi::FunctionType {

using RegisterVector = llvm::SmallVector<model::Register::Values, 2>;

/// The internal representation of the argument shared between both
/// the to-raw conversion and the layout.
struct DistributedArgument {
  /// A list of registers the argument uses.
  RegisterVector Registers = {};

  /// The total size of the argument (including padding if necessary) in bytes.
  uint64_t Size = 0;

  /// The size of the piece of the argument placed on the stack.
  /// \note: has to be equal to `0` or `this->Size` for any ABI for which
  ///        `abi::Definition::ArgumentsCanBeSplitBetweenRegistersAndStack()`
  ///        returns `false`. Otherwise, it has to be an integer value, such
  ///        that `(0 <= SizeOnStack <= this->Size)` is true.
  uint64_t SizeOnStack = 0;

  /// Mark this argument as a padding argument, which means an unused location
  /// (either a register or a piece of the stack) which needs to be seen as
  /// a separate argument to be able to place all the following arguments
  /// in the correct positions.
  ///
  /// The "padding" arguments are emitted as normal arguments in RawFunctionType
  /// but are omitted in `Layout`.
  bool RepresentsPadding = false;
};
using DistributedArguments = llvm::SmallVector<DistributedArgument, 8>;

using RegisterSpan = std::span<const model::Register::Values>;
using ArgumentSet = TrackingSortedVector<model::Argument>;

class ArgumentDistributor {
private:
  const abi::Definition &ABI;

public:
  uint64_t UsedGeneralPurposeRegisterCount = 0;
  uint64_t UsedVectorRegisterCount = 0;
  uint64_t UsedStackOffset = 0;
  uint64_t ArgumentIndex = 0;

public:
  explicit ArgumentDistributor(const abi::Definition &ABI) : ABI(ABI) {
    revng_assert(ABI.verify());
  }

  DistributedArguments next(const model::QualifiedType &ArgumentType);

private:
  DistributedArguments positionBased(const model::QualifiedType &ArgumentType);
  DistributedArguments
  nonPositionBased(const model::QualifiedType &ArgumentType);

public:
  /// \returns true if adding the \ref CurrentType to an CABI-FT as is would
  ///          make sense.
  /// \note this calls `next` internally, so it's a bad idea to call both.
  bool canBeNext(model::QualifiedType CurrentType,
                 uint64_t CurrentOffset,
                 uint64_t NextOffset,
                 uint64_t NextAlignment);

private:
  // A helper used for finding padding holes.
  bool verifyAlignment(uint64_t CurrentOffset,
                       uint64_t CurrentSize,
                       uint64_t NextOffset,
                       uint64_t NextAlignment) const;
};

} // namespace abi::FunctionType
