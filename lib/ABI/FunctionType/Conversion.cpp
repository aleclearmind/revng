/// \file Conversion.cpp

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/ADT/SmallSet.h"

#include "revng/ABI/Definition.h"
#include "revng/ABI/FunctionType/Conversion.h"
#include "revng/ABI/FunctionType/Layout.h"
#include "revng/ABI/FunctionType/Support.h"
#include "revng/ABI/FunctionType/TypeBucket.h"
#include "revng/ADT/STLExtras.h"
#include "revng/Model/Binary.h"
#include "revng/Model/Helpers.h"
#include "revng/Support/OverflowSafeInt.h"

#include "Distribution.h"

static Logger Log("function-type-conversion-to-cabi");

namespace abi::FunctionType {

class ToCABIConverter {
private:
  // Use a simple SortedVector, no reason to use the Tracking one.
  using ArgumentRegisters = TrackingSortedVector<model::NamedTypedRegister>;
  using ReturnValueRegisters = TrackingSortedVector<model::TypedRegister>;

public:
  struct Converted {
    llvm::SmallVector<model::Argument, 8> RegisterArguments;
    llvm::SmallVector<model::Argument, 8> StackArguments;
    model::QualifiedType ReturnValueType;
  };

private:
  const abi::Definition &ABI;
  TypeBucket Bucket;
  const bool UseSoftRegisterStateDeductions = false;

public:
  ToCABIConverter(const abi::Definition &ABI,
                  model::Binary &Binary,
                  const bool UseSoftRegisterStateDeductions) :
    ABI(ABI),
    Bucket(Binary),
    UseSoftRegisterStateDeductions(UseSoftRegisterStateDeductions) {}

  // Why public?
  [[nodiscard]] std::optional<Converted>
  tryConvert(const model::RawFunctionType &FunctionType) {
    // Register arguments first.
    auto Arguments = tryConvertingRegisterArguments(FunctionType.Arguments());
    if (!Arguments.has_value()) {
      revng_log(Log, "Unable to convert register arguments.");
      Bucket.drop();
      return std::nullopt;
    }

    // Count used registers.
    ArgumentDistributor Distributor(ABI);
    Distributor.ArgumentIndex = Arguments->size();
    for (const auto &NTRegister : FunctionType.Arguments()) {
      auto Kind = model::Register::primitiveKind(NTRegister.Location());
      if (Kind == model::PrimitiveTypeKind::Values::PointerOrNumber)
        ++Distributor.UsedGeneralPurposeRegisterCount;
      else if (Kind == model::PrimitiveTypeKind::Float)
        ++Distributor.UsedVectorRegisterCount;
      else
        revng_abort(("Register ("
                     + model::Register::getName(NTRegister.Location()).str()
                     + ") is not supported as an argument.")
                      .c_str());
    }

    // Then stack ones.
    auto Stack = tryConvertingStackArguments(FunctionType.StackArgumentsType(),
                                             Distributor);
    if (!Stack.has_value()) {
      revng_log(Log, "Unable to convert stack arguments.");
      Bucket.drop();
      return std::nullopt;
    }

    // And the return value.
    auto ReturnType = tryConvertingReturnValue(FunctionType.ReturnValues());
    if (!ReturnType.has_value()) {
      revng_log(Log, "Unable to convert return value.");
      Bucket.drop();
      return std::nullopt;
    }

    // Conversion successful, return the result.
    Bucket.commit();
    return Converted{ std::move(*Arguments),
                      std::move(*Stack),
                      std::move(*ReturnType) };
  }

private:
  /// Helper used for converting register arguments to the c-style
  /// representation
  ///
  /// \param Registers a set of registers confirmed to be in use by
  ///        the function in question.
  ///
  /// \return a list of arguments if the conversion was successful,
  ///         `std::nullopt` otherwise.
  std::optional<llvm::SmallVector<model::Argument, 8>>
  tryConvertingRegisterArguments(const ArgumentRegisters &Registers);

  /// Helper used for converting stack argument struct into
  /// the c-style representation
  ///
  /// \param StackArgumentTypes The qualified type of the relevant part of
  ///        the stack.
  /// \param IndexOffset The index of the first argument (should be set to
  ///        the number of register arguments).
  ///
  /// \return An ordered list of arguments.
  std::optional<llvm::SmallVector<model::Argument, 8>>
  tryConvertingStackArguments(model::QualifiedType StackArgumentTypes,
                              ArgumentDistributor DistributionHelper);

  /// Helper used for converting return values to the c-style representation
  ///
  /// \param Registers a set of registers confirmed to be in use by
  ///        the function in question.
  /// \param ReturnValueLocation an optional register used for pointing to
  ///        return values in memory on some architectures. It is set to
  ///        `nullptr` if it's not applicable.
  ///
  /// \return a qualified type if conversion is possible, `std::nullopt`
  ///         otherwise.
  std::optional<model::QualifiedType>
  tryConvertingReturnValue(const ReturnValueRegisters &Registers);
};

// Docs
std::optional<model::TypePath>
tryConvertToCABI(const model::RawFunctionType &FunctionType,
                 TupleTree<model::Binary> &Binary,
                 std::optional<model::ABI::Values> MaybeABI,
                 bool UseSoftRegisterStateDeductions) {
  if (!MaybeABI.has_value())
    MaybeABI = Binary->DefaultABI();

  revng_log(Log, "Converting a `RawFunctionType` to `CABIFunctionType`.");
  revng_log(Log, "ABI: " << model::ABI::getName(MaybeABI.value()).str());
  revng_log(Log, "Original Type:\n" << serializeToString(FunctionType));
  LoggerIndent Indentation(Log);

  const abi::Definition &ABI = abi::Definition::get(*MaybeABI);
  // It checking this strictly necessary or are we going to fail anyway later on
  // if we don't do this?
  if (ABI.isIncompatibleWith(FunctionType)) {
    revng_log(Log,
              "FAIL: the function is not compatible with `"
                << model::ABI::getName(ABI.ABI()) << "`.");
    return std::nullopt;
  }

  ToCABIConverter Converter(ABI, *Binary, UseSoftRegisterStateDeductions);
  std::optional Converted = Converter.tryConvert(FunctionType);
  if (!Converted.has_value())
    return std::nullopt;

  // The conversion was successful, a new `CABIFunctionType` can now be created,
  auto [NewType, NewTypePath] = Binary->makeType<model::CABIFunctionType>();
  revng_assert(NewType.ID() != 0);
  model::copyMetadata(NewType, FunctionType);
  NewType.ABI() = ABI.ABI();

  // And filled in with the argument information.
  auto Arguments = llvm::concat<model::Argument>(Converted->RegisterArguments,
                                                 Converted->StackArguments);
  for (auto &Argument : Arguments)
    NewType.Arguments().insert(std::move(Argument));
  NewType.ReturnType() = Converted->ReturnValueType;

  revng_log(Log, "Conversion successful:\n" << serializeToString(NewType));

  // Since CABI-FT only have one field for return value comments - we have no
  // choice but to resort to concatenation in order to preserve as much
  // information as possible.
  for (model::TypedRegister ReturnValue : FunctionType.ReturnValues()) {
    if (!ReturnValue.Comment().empty()) {
      if (!NewType.ReturnValueComment().empty())
        NewType.ReturnValueComment() += '\n';
      model::Register::Values RVLoc = ReturnValue.Location();
      NewType.ReturnValueComment() += model::Register::getRegisterName(RVLoc);
      NewType.ReturnValueComment() += ": ";
      NewType.ReturnValueComment() += ReturnValue.Comment();
    }
  }

  // To finish up the conversion, remove all the references to the old type by
  // carefully replacing them with references to the new one.
  replaceAllUsesWith(FunctionType.key(), NewTypePath, Binary);

  // And don't forget to remove the old type.
  Binary->Types().erase(FunctionType.key());

  return NewTypePath;
}

using TCC = ToCABIConverter;
std::optional<llvm::SmallVector<model::Argument, 8>>
TCC::tryConvertingRegisterArguments(const ArgumentRegisters &Registers) {
  // Rely onto the register state deduction to make sure no "holes" are
  // present in-between the argument registers.
  abi::RegisterState::Map Map(model::ABI::getArchitecture(ABI.ABI()));
  for (const model::NamedTypedRegister &Reg : Registers)
    Map[Reg.Location()].IsUsedForPassingArguments = abi::RegisterState::Yes;

  abi::RegisterState::Map DeductionResults = Map;
  if (UseSoftRegisterStateDeductions) {
    std::optional MaybeDeductionResults = ABI.tryDeducingRegisterState(Map);
    if (MaybeDeductionResults.has_value())
      DeductionResults = std::move(MaybeDeductionResults.value());
    else
      return std::nullopt;
  } else {
    DeductionResults = ABI.enforceRegisterState(Map);
  }

  llvm::SmallVector<model::Register::Values, 8> ArgumentRegisters;
  for (auto [Register, Pair] : DeductionResults)
    if (abi::RegisterState::shouldEmit(Pair.IsUsedForPassingArguments))
      ArgumentRegisters.emplace_back(Register);

  // But just knowing which registers we need is not sufficient, we also have to
  // order them properly.
  auto Ordered = ABI.sortArguments(ArgumentRegisters);

  llvm::SmallVector<model::Argument, 8> Result;
  for (model::Register::Values Register : Ordered) {
    auto CurrentRegisterIterator = Registers.find(Register);
    if (CurrentRegisterIterator != Registers.end()) {
      // If the current register is confirmed to be in use, convert it into
      // an argument while preserving its type and metadata.
      model::Argument &Current = Result.emplace_back();
      Current.CustomName() = CurrentRegisterIterator->CustomName();

      if (CurrentRegisterIterator->Type().UnqualifiedType().get() != nullptr)
        Current.Type() = CurrentRegisterIterator->Type();
      else
        Current.Type() = { Bucket.defaultRegisterType(Register), {} };
    } else {
      // This register is unused but we still have to create an argument
      // for it. Otherwise the resulting function will be semantically different
      // from the input one.
      //
      // Also, if the indicator for this "hole" is not preserved, there will be
      // no way to recreate it at any point in the future, when it's being
      // converted back to the representation similar to original (e.g. when
      // producing the `Layout` for this function).
      Result.emplace_back().Type() = { Bucket.genericRegisterType(Register),
                                       {} };
    }
  }

  // Rewrite all the indices to make sure they are consistent.
  for (auto Pair : llvm::enumerate(Result))
    Pair.value().Index() = Pair.index();

  return Result;
}

bool ArgumentDistributor::verifyAlignment(uint64_t CurrentOffset,
                                          uint64_t CurrentSize,
                                          uint64_t NextOffset,
                                          uint64_t NextAlignment) const {
  uint64_t PaddedSize = abi::FunctionType::paddedSizeOnStack(CurrentSize,
                                                             NextAlignment);
  revng_log(Log,
            "Attempting to verify alignment for an argument with size "
              << CurrentSize << "(" << PaddedSize << " when padded) at offset "
              << CurrentOffset << ". The next argument is at offset "
              << NextOffset << " and is aligned at " << NextAlignment << ".");

  OverflowSafeInt Offset = CurrentOffset;
  Offset += PaddedSize;
  if (!Offset) {
    // Abandon if offset overflows.
    revng_log(Log, "Error: Integer overflow when calculating field offsets.");
    return false;
  }

  if (*Offset == NextOffset) {
    revng_log(Log, "Argument slots in perfectly.");
    return true;
  } else if (*Offset < NextOffset) {
    // Offsets are different, there's most likely padding between the arguments.
    revng_assert(NextOffset % NextAlignment == 0,
                 "Type's alignment doesn't make sense.");
    uint64_t AdjustedAlignment = NextAlignment;

    // Round all the alignment up to the register size - to avoid sub-word
    // offsets on the stack.
    if (AdjustedAlignment < ABI.getPointerSize())
      AdjustedAlignment = ABI.getPointerSize();
    revng_assert(NextOffset % AdjustedAlignment == 0,
                 "Adjusted alignment doesn't make sense.");

    // Check whether the next argument's position makes sense.
    uint64_t Delta = AdjustedAlignment - *Offset % AdjustedAlignment;
    if (Delta != AdjustedAlignment && *Offset + Delta == NextOffset) {
      revng_log(Log,
                "Argument slots in after accounting for the alignment of the "
                "next field adjusted to "
                  << AdjustedAlignment << " with the resulting difference of "
                  << Delta << ".");
      return true;
    } else {
      revng_log(Log,
                "Error: The natural alignment of a type would make it "
                "impossible to represent as CABI: there would have to be "
                "a hole between two arguments. Abandon the conversion.");
      // TODO: we probably want to preprocess such functions and manually
      //       "fill" the holes in before attempting the conversion.
      return false;
    }
  } else {
    revng_log(Log,
              "Error: The natural alignment of a type would make it "
              "impossible to represent as CABI: the arguments (including "
              "the padding) would have to overlap. Abandon the conversion.");
    return false;
  }
}

// It might make more sense to move the body of this method into
// ToCABIConverter, since its only used by tryConvertingStackArguments.
// Same for verifyAlignment, I think.
// My goal would be to better isolate `ArgumentDistributor` (and
// `DistributeOne`) from the rest of the code by:
//
// 1. Moving this method (along with `verifyAlignment`) closer to its only user.
// 2. Have `ToRawConverter::distributeReturnValue` use a regular
//    `ArgumentDistributor` instead of using `distributeOne` directly.
bool ArgumentDistributor::canBeNext(model::QualifiedType CurrentType,
                                    uint64_t CurrentOffset,
                                    uint64_t NextOffset,
                                    uint64_t NextAlignment) {
  revng_log(Log,
            "Checking whether the argument #"
              << ArgumentIndex << " can be slotted right in:\n"
              << serializeToString(CurrentType) << "Currently "
              << UsedGeneralPurposeRegisterCount << " general purpose and "
              << UsedVectorRegisterCount << " vector registers are in use.");

  std::optional<uint64_t> MaybeSize = CurrentType.size();
  revng_assert(MaybeSize.has_value() && MaybeSize.value() != 0);

  if (!verifyAlignment(CurrentOffset, *MaybeSize, NextOffset, NextAlignment))
    return false;

  for (const auto &Distributed : next(CurrentType)) {
    if (Distributed.RepresentsPadding)
      continue; // Skip padding.

    if (!Distributed.Registers.empty()) {
      revng_log(Log,
                "Error: Because there are still available registers, "
                "an argument cannot just be added as is - resulting CFT would "
                "not be compatible with the original function.");
      return false;
    }

    if (Distributed.SizeOnStack == 0) {
      revng_log(Log,
                "Something went very wrong: an argument uses neither registers "
                "nor stack.");
      return false;
    }

    // Compute the next stack offset
    uint64_t NextStackOffset = ABI.alignedOffset(UsedStackOffset, CurrentType);
    NextStackOffset += ABI.paddedSizeOnStack(*MaybeSize);
    if (Distributed.SizeOnStack != NextStackOffset - UsedStackOffset) {
      revng_log(Log,
                "Because the stack position wouldn't match due to holes in the "
                "stack, an argument cannot just be added as is - resulting CFT "
                "would not be compatible.");
      return false;
    }
  }

  return true;
}

// Rename `DistributionHelper` to just `Distributor`
std::optional<llvm::SmallVector<model::Argument, 8>>
TCC::tryConvertingStackArguments(model::QualifiedType StackArgumentTypes,
                                 ArgumentDistributor DistributionHelper) {
  uint64_t InitialIndexOffset = DistributionHelper.ArgumentIndex;

  if (StackArgumentTypes.UnqualifiedType().empty()) {
    // If there is no type, it means that the importer responsible for this
    // function didn't detect any stack arguments and avoided creating
    // a new empty type.
    return llvm::SmallVector<model::Argument, 8>{};
  }

  // Qualifiers here are not allowed since this must point to a simple struct.
  revng_assert(StackArgumentTypes.Qualifiers().empty());
  auto *Unqualified = StackArgumentTypes.UnqualifiedType().get();
  model::StructType &Stack = *llvm::cast<model::StructType>(Unqualified);

  // Is there a "non-full" or "partial" alignment?
  // Compute the full alignment.
  uint64_t FullAlignment = *ABI.alignment(StackArgumentTypes);
  if (!llvm::isPowerOf2_64(FullAlignment)) {
    // Assert? Same below.
    revng_log(Log,
              "The natural alignment of a type is not a power of two:\n"
                << serializeToString(Stack));
    return std::nullopt;
  }

  // If the struct is empty, it indicates that there are no stack arguments.
  if (Stack.size() == 0) {
    revng_assert(Stack.Fields().empty());
    return llvm::SmallVector<model::Argument, 8>{};
  }

  // Verify the alignment of the first argument.
  if (Stack.Fields().empty()) {
    revng_log(Log, "Stack struct has no fields.");
  } else {
    uint64_t FirstAlignment = *ABI.alignment(Stack.Fields().begin()->Type());
    if (!llvm::isPowerOf2_64(FirstAlignment)) {
      revng_log(Log,
                "The natural alignment of a type is not a power of two:\n"
                  << serializeToString(Stack.Fields().begin()->Type()));
      return std::nullopt;
    }
  }

  // Go over all the `[CurrentArgument, TheNextOne]` field pairs.
  // `CurrentRange` is used to keep track of the "remaining" ones.
  // This allows us to keep its value when the loop to be aborted (which
  // happens when we meet an argument we cannot just convert "as is").
  uint64_t CurrentStackOffset = 0;
  llvm::SmallVector<model::Argument, 8> Result;
  // I think this can be rewritten this using some form of zip and make much
  // clearer.
  // Also I'd add a `bool Successful` instead of relying on
  // `CurrentRange.size() == 1`
  auto CurrentRange = std::ranges::subrange(Stack.Fields().begin(),
                                            Stack.Fields().end());
  while (CurrentRange.size() > 1) {
    auto [CurrentArgument, TheNextOne] = takeAsTuple<2>(CurrentRange);

    uint64_t NextAlignment = *ABI.alignment(TheNextOne.Type());
    if (!llvm::isPowerOf2_64(NextAlignment)) {
      revng_log(Log,
                "The natural alignment of a type is not a power of two:\n"
                  << serializeToString(TheNextOne.Type()));
      return std::nullopt;
    }

    if (!DistributionHelper.canBeNext(CurrentArgument.Type(),
                                      CurrentArgument.Offset(),
                                      TheNextOne.Offset(),
                                      NextAlignment)) {
      // We met a argument we cannot just "add".
      break;
    }

    // Create the argument from this field.
    model::Argument &New = Result.emplace_back();
    model::copyMetadata(New, CurrentArgument);
    New.Index() = DistributionHelper.ArgumentIndex - 1;
    New.Type() = CurrentArgument.Type();

    CurrentRange = std::ranges::subrange(std::next(CurrentRange.begin()),
                                         Stack.Fields().end());
  }

  // The main loop is over. Which means that we either converted everything but
  // the very last argument correctly, OR that we aborted half-way through.
  if (CurrentRange.size() == 1) {
    // Having only one element in the "remaining" range means that only the last
    // field is left - add it too after checking.
    const model::StructField &LastArgument = CurrentRange.front();

    // This is a workaround for the cases where expected alignment is missing
    // at the very end of the RFT-style stack argument struct:
    uint64_t FullSize = abi::FunctionType::paddedSizeOnStack(Stack.Size(),
                                                             FullAlignment);

    std::optional<uint64_t> LastSize = LastArgument.Type().size();
    revng_assert(LastSize.has_value() && LastSize.value() != 0);
    if (DistributionHelper.canBeNext(LastArgument.Type(),
                                     LastArgument.Offset(),
                                     FullSize,
                                     FullAlignment)) {
      model::Argument &New = Result.emplace_back();
      model::copyMetadata(New, LastArgument);
      New.Type() = LastArgument.Type();
      New.Index() = DistributionHelper.ArgumentIndex - 1;
      return Result;
    }
  }

  // I'm not asking to do this, but could we, in theory, create a struct
  // argument to bundle non-conforming fields and then go back to emit regular
  // stack arguments?
  // Getting to this point (past the return statement in the last element
  // section) means that there is at least one argument we cannot just "add".
  revng_log(Log,
            "Naive conversion failed. Try to fall back on using structs "
            "instead.");
  if (CurrentRange.size() != Stack.Fields().size()) {
    // This condition being true means that we did succeed in converting some
    // of the arguments, but failed on some others. Let's try to wrap
    // the remainder into a struct and see if bundled together they make more
    // sense in c-like representation.
    revng_log(Log,
              "Some fields were converted successfully, try to slot in the "
              "rest as a struct.");
    const model::StructField &LastSuccessful = *std::prev(CurrentRange.begin());
    std::uint64_t Offset = LastSuccessful.Offset();
    Offset += ABI.paddedSizeOnStack(*LastSuccessful.Type().size());

    model::StructType ResultType;
    ResultType.Size() = Stack.Size() - Offset;
    for (const auto &Field : CurrentRange) {
      model::StructField Copy = Field;
      Copy.Offset() -= Offset;
      ResultType.Fields().emplace(std::move(Copy));
    }

    auto [QT, _] = temporaryQualifiedType<model::StructType>(ResultType);
    if (DistributionHelper.canBeNext(QT, Offset, Stack.Size(), FullAlignment)) {
      revng_log(Log, "Struct for the remaining argument worked.");
      model::Argument &New = Result.emplace_back();
      New.Index() = Stack.Fields().size() - CurrentRange.size();
      New.Index() += InitialIndexOffset;

      auto [_, P] = Bucket.makeType<model::StructType>(std::move(ResultType));
      New.Type() = model::QualifiedType{ P, {} };
      return Result;
    }
  }

  // Reaching this far means that we either aborted on the very first argument
  // OR that the partial conversion didn't work well either - let's try and see
  // if it would make sense to add the whole "stack" struct as one argument.
  if (!DistributionHelper.canBeNext(StackArgumentTypes,
                                    0,
                                    Stack.Size(),
                                    FullAlignment)) {
    // Nope, stack struct didn't work either. There's nothing else we can do.
    // Just report that this function cannot be converted.
    return std::nullopt;
  }

  revng_log(Log,
            "Adding the whole stack as a single argument is the best we can "
            "do.");
  model::Argument &New = Result.emplace_back();
  New.Type() = StackArgumentTypes;
  New.Index() = InitialIndexOffset;
  return Result;
}

std::optional<model::QualifiedType>
TCC::tryConvertingReturnValue(const ReturnValueRegisters &Registers) {
  if (Registers.size() == 0) {
    // The function doesn't return anything.
    return model::QualifiedType{
      Bucket.getPrimitiveType(model::PrimitiveTypeKind::Void, 0), {}
    };
  }

  // We only convert register-based return values: those that are returned using
  // a pointer to memory readied by the callee are technically fine without any
  // intervention (they are just `void` functions that modify some object passed
  // into them with a pointer.
  //
  // We might want to handle this in a different way under some architectures
  // (i.e. ARM64 because it uses a separate `PointerToCopyLocation` register),
  // but for now the dumb approach should suffice.

  abi::RegisterState::Map Map(model::ABI::getArchitecture(ABI.ABI()));
  for (const model::TypedRegister &Register : Registers)
    Map[Register.Location()].IsUsedForReturningValues = abi::RegisterState::Yes;
  abi::RegisterState::Map DeductionResults = Map;
  if (UseSoftRegisterStateDeductions) {
    std::optional MaybeDeductionResults = ABI.tryDeducingRegisterState(Map);
    if (MaybeDeductionResults.has_value())
      DeductionResults = std::move(MaybeDeductionResults.value());
    else
      return std::nullopt;
  } else {
    DeductionResults = ABI.enforceRegisterState(Map);
  }

  llvm::SmallSet<model::Register::Values, 8> ReturnValueRegisters;
  for (auto [Register, Pair] : DeductionResults)
    if (abi::RegisterState::shouldEmit(Pair.IsUsedForReturningValues))
      ReturnValueRegisters.insert(Register);
  auto Ordered = ABI.sortReturnValues(ReturnValueRegisters);

  if (Ordered.size() == 1) {
    if (auto Iter = Registers.find(*Ordered.begin()); Iter != Registers.end()) {
      // Only one register is used, just return its type.
      return Iter->Type();
    } else {
      // One register is used but its type cannot be obtained.
      // Create a register sized return type instead.
      return model::QualifiedType(Bucket.defaultRegisterType(*Ordered.begin()),
                                  {});
    }
  } else {
    // Multiple registers, it's either a struct or a big scalar.

    // First, check if it can possibly be a struct.
    if (ABI.MaximumGPRsPerAggregateReturnValue() < Ordered.size()) {
      // It cannot be a struct, it's bigger than allowed.
      if (ABI.MaximumGPRsPerScalarReturnValue() < Ordered.size()) {
        // It cannot be a scalar either.
        revng_log(Log,
                  "No known return value type supports that many registers ("
                    << Ordered.size() << ") under "
                    << model::ABI::getName(ABI.ABI()) << ":\n");
        return std::nullopt;
      }

      // It's probably a scalar, replace its type with a fake one making sure
      // at least the size adds up.
      //
      // TODO: sadly this discards type information from the registers, look
      //       into preserving it at least partially.
      uint64_t PointerSize = model::ABI::getPointerSize(ABI.ABI());
      return model::QualifiedType{
        Bucket.getPrimitiveType(model::PrimitiveTypeKind::Values::Generic,
                                PointerSize * Ordered.size()),
        {}
      };
    } else {
      // It could be either a struct or a scalar, go the conservative route
      // and make a struct for it.
      auto [ReturnType, ReturnTypePath] = Bucket.makeType<model::StructType>();
      for (model::Register::Values Register : Ordered) {
        // Make a separate field for each register.
        model::StructField Field;
        Field.Offset() = ReturnType.Size();

        // TODO: ensure that the type is in fact naturally aligned
        if (auto It = Registers.find(*Ordered.begin()); It != Registers.end())
          Field.Type() = It->Type();
        else
          Field.Type() = { Bucket.genericRegisterType(*Ordered.begin()), {} };

        std::optional<uint64_t> FieldSize = Field.Type().size();
        revng_assert(FieldSize.has_value() && FieldSize.value() != 0);

        // Round the next offset based on the natural alignment.
        ReturnType.Size() = ABI.alignedOffset(ReturnType.Size(), Field.Type());

        // Insert the field
        ReturnType.Fields().insert(std::move(Field));

        // Update the total struct size: insert some padding if necessary.
        uint64_t RegisterSize = model::ABI::getPointerSize(ABI.ABI());
        ReturnType.Size() += paddedSizeOnStack(FieldSize.value(), RegisterSize);
      }

      revng_assert(ReturnType.Size() != 0 && !ReturnType.Fields().empty());
      return model::QualifiedType(ReturnTypePath, {});
    }
  }
}

} // namespace abi::FunctionType
