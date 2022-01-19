/// \file ApplyRegisterStateDeductions.cpp
/// \brief

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <span>

#include "llvm/ADT/ArrayRef.h"

#include "revng/ABI/ApplyRegisterStateDeductions.h"
#include "revng/ABI/CallingConventionTrait.h"
#include "revng/Model/Types.h"

static Logger Log("ABI-RegisterStateDeduction");

using NRS = detail::NullableRegisterState;
using AccessorType = NRS &(detail::State &);
using CAccessorType = const NRS &(const detail::State &);

static NRS &accessArgument(detail::State &Input) {
  return Input.IsUsedForPassingArguments;
}
static NRS &accessReturnValue(detail::State &Input) {
  return Input.IsUsedForReturningValues;
}

static const NRS &accessCArgument(const detail::State &Input) {
  return Input.IsUsedForPassingArguments;
}
static const NRS &accessCReturnValue(const detail::State &Input) {
  return Input.IsUsedForReturningValues;
}

using CRegister = const model::Register::Values;

/// Finds last relevant (`Yes` or `Dead`) register out of the provided list.
///
/// Returns `0` if no registers from the list were mentioned.
template<CAccessorType Accessor, model::Architecture::Values Arch>
size_t findLastUsedIndex(llvm::ArrayRef<model::Register::Values> Registers,
                         const detail::RegisterStateMap<Arch> &State) {
  for (size_t Index = Registers.size(); Index != 0; --Index)
    if (const auto &MaybeState = Accessor(State.at(Registers[Index - 1])))
      if (model::RegisterState::isYesOrDead(MaybeState.value()))
        return Index;

  return 0;
}

template<model::ABI::Values ABI, bool EnforceABIConformance>
struct DeductionImpl {
  static constexpr auto Arch = model::ABI::getArchitecture(ABI);
  using CallingConvention = CallingConventionTrait<ABI>;
  using InternalState = detail::RegisterStateMap<Arch>;

  static std::optional<RegisterStateMap>
  run(const RegisterStateMap &InputState) {
    const InternalState &State = unpackState(InputState);

    std::optional<InternalState> Result;
    if constexpr (CallingConvention::ArgumentsArePositionBased)
      Result = runForPositionBasedABIs(State);
    else
      Result = runForNonPositionBasedABIs(State);

    if (Result.has_value())
      return RegisterStateMap(Result.value());
    else
      return std::nullopt;
  }

private:
  static std::optional<InternalState>
  runForPositionBasedABIs(const InternalState &State) {
    // Copy the state to serve as a return value.
    InternalState Result = State;

    if (!runForPositionBasedArguments(Result))
      return std::nullopt;

    if (!runForPositionBasedReturnValues(Result))
      return std::nullopt;

    // Detect possible state problems and set all the registers still
    // remaining as "unknown" to `No`.
    for (auto [Register, RegisterState] : Result) {
      if (auto &A = accessArgument(RegisterState))
        revng_assert(A != model::RegisterState::Invalid);
      else
        A = model::RegisterState::No;

      if (auto &RV = accessReturnValue(RegisterState))
        revng_assert(RV != model::RegisterState::Invalid);
      else
        RV = model::RegisterState::No;
    }

    if (!checkPositionBasedABIConformance(Result))
      return std::nullopt;

    return Result;
  }

  static bool checkPositionBasedABIConformance(InternalState &State) {
    llvm::ArrayRef GPAR = CallingConvention::GeneralPurposeArgumentRegisters;
    llvm::ArrayRef VAR = CallingConvention::VectorArgumentRegisters;
    llvm::ArrayRef GPRV = CallingConvention::GeneralPurposeReturnValueRegisters;
    llvm::ArrayRef VRVR = CallingConvention::VectorReturnValueRegisters;

    auto AllowedArgumentRegisters = llvm::concat<CRegister>(GPAR, VAR);
    auto AllowedReturnValueRegisters = llvm::concat<CRegister>(GPRV, VRVR);
    for (auto [Register, RegisterState] : State) {
      auto &[UsedForPassingArguments, UsedForReturningValues] = RegisterState;

      revng_assert(UsedForPassingArguments.has_value());
      if (model::RegisterState::isYesOrDead(*UsedForPassingArguments)) {
        if (!llvm::is_contained(AllowedArgumentRegisters, Register)) {
          if constexpr (EnforceABIConformance == true) {
            revng_log(Log,
                      "Enforcing `model::Register::"
                        << model::Register::getName(Register).data()
                        << "` to `No` as `"
                        << model::ABI::getName(CallingConvention::ABI).data()
                        << "` ABI doesn't allow it to be used.");
            UsedForPassingArguments = model::RegisterState::No;
          } else {
            revng_log(Log,
                      "Aborting, `model::Register::"
                        << model::Register::getName(Register).data()
                        << "` register is used despite not being allowed by `"
                        << model::ABI::getName(CallingConvention::ABI).data()
                        << "` ABI.");
            return false;
          }
        }
      }

      revng_assert(UsedForReturningValues.has_value());
      if (model::RegisterState::isYesOrDead(*UsedForReturningValues)) {
        if (!llvm::is_contained(AllowedReturnValueRegisters, Register)) {
          if constexpr (EnforceABIConformance == true) {
            revng_log(Log,
                      "Enforcing `model::Register::"
                        << model::Register::getName(Register).data()
                        << "` to `No` as `"
                        << model::ABI::getName(CallingConvention::ABI).data()
                        << "` ABI doesn't allow it to be used.");
            UsedForReturningValues = model::RegisterState::No;
          } else {
            revng_log(Log,
                      "Aborting, `model::Register::"
                        << model::Register::getName(Register).data()
                        << "` register is used despite not being allowed by `"
                        << model::ABI::getName(CallingConvention::ABI).data()
                        << "` ABI.");
            return false;
          }
        }
      }
    }

    return true;
  }

  template<AccessorType Accessor>
  static void offsetPositionBasedArgument(model::Register::Values Register,
                                          bool &IsRequired,
                                          InternalState &State) {
    auto &Accessed = Accessor(State[Register]);
    if (!Accessed.has_value())
      Accessed = model::RegisterState::Maybe;

    if (IsRequired == true) {
      if (model::RegisterState::isYesOrDead(Accessed.value())) {
        // Nothing to do, it's already set.
      } else {
        // Set it.
        Accessed = model::RegisterState::YesOrDead;
      }
    } else {
      if (model::RegisterState::isYesOrDead(Accessed.value())) {
        // Make all the subsequent registers required.
        IsRequired = true;
      } else {
        // Nothing to do, it doesn't have to be set.
      }
    }
  }

  static bool runForPositionBasedArguments(InternalState &State) {
    llvm::ArrayRef GPAR = CallingConvention::GeneralPurposeArgumentRegisters;
    llvm::ArrayRef VAR = CallingConvention::VectorArgumentRegisters;

    bool IsRequired = false;
    if (GPAR.size() > VAR.size()) {
      for (auto Regist : llvm::reverse(GPAR.drop_front(VAR.size())))
        offsetPositionBasedArgument<accessArgument>(Regist, IsRequired, State);
      GPAR = GPAR.take_front(VAR.size());
    } else if (VAR.size() > GPAR.size()) {
      for (auto Regist : llvm::reverse(VAR.drop_front(GPAR.size())))
        offsetPositionBasedArgument<accessArgument>(Regist, IsRequired, State);
      VAR = VAR.take_front(GPAR.size());
    }

    auto ArgumentRange = llvm::zip(llvm::reverse(GPAR), llvm::reverse(VAR));
    for (auto [GPRegister, VRegister] : ArgumentRange) {
      if (auto D = singlePositionBasedDeduction<accessCArgument>(GPRegister,
                                                                 VRegister,
                                                                 IsRequired,
                                                                 State)) {
        accessArgument(State[GPRegister]) = D->GPR;
        accessArgument(State[VRegister]) = D->VR;
      } else {
        return false;
      }
    }

    return true;
  }

  static bool runForPositionBasedReturnValues(InternalState &State) {
    llvm::ArrayRef GPRV = CallingConvention::GeneralPurposeReturnValueRegisters;
    llvm::ArrayRef VRVR = CallingConvention::VectorReturnValueRegisters;

    size_t GPRVCount = findLastUsedIndex<accessCReturnValue>(GPRV, State);
    auto UsedGPRV = llvm::ArrayRef(GPRV).take_front(GPRVCount);
    size_t VRVRCount = findLastUsedIndex<accessCReturnValue>(VRVR, State);
    auto UsedVRVR = llvm::ArrayRef(VRVR).take_front(VRVRCount);

    auto AllowedRetValRegisters = llvm::concat<CRegister>(GPRV, VRVR);
    auto UsedRetValRegisters = llvm::concat<CRegister>(UsedGPRV, UsedVRVR);

    // Even for position based ABIs, return values have behaviour patterns
    // similar to non-position based ones, so the initial deduction step is
    // to run the non-position based deduction.
    for (auto [Register, RegisterState] : State) {
      auto &RVState = accessReturnValue(RegisterState);
      bool IsRVAllowed = llvm::is_contained(AllowedRetValRegisters, Register);
      bool IsRVRequired = llvm::is_contained(UsedRetValRegisters, Register);
      if (auto RV = singleNonPositionBasedDeduction(RVState,
                                                    IsRVAllowed,
                                                    IsRVRequired,
                                                    Register))
        RVState = RV.value();
      else
        return false;
    }

    // Then we make sure that only either GPRs or VRs are used, never both.
    bool Dummy = false;
    if (auto D = singlePositionBasedDeduction<accessCReturnValue>(GPRV.front(),
                                                                  VRVR.front(),
                                                                  Dummy,
                                                                  State)) {
      if (model::RegisterState::isYesOrDead(D->GPR)) {
        if (model::RegisterState::isYesOrDead(D->VR)) {
          revng_log(Log,
                    "Impossible to differenciate whether the return value is "
                    "passed in a general purpose register or in vector ones. "
                    "The ABI is "
                      << model::ABI::getName(ABI).data() << ".");
          return false;
        } else {
          // The return value is in GPRs, mark all the vector register as `No`.
          for (auto Register : VRVR)
            accessReturnValue(State[Register]) = model::RegisterState::No;
        }
      } else {
        if (model::RegisterState::isYesOrDead(D->VR)) {
          // The return value is in vector registers, mark all the GPRs as `No`.
          for (auto Register : GPRV)
            accessReturnValue(State[Register]) = model::RegisterState::No;
        } else {
          // No return value (???)
          // Since there's no way to confirm, do nothing.
        }
      }
    } else {
      return false;
    }

    return true;
  }

  struct StatePair {
    model::RegisterState::Values GPR;
    model::RegisterState::Values VR;
  };
  template<CAccessorType Accessor>
  static std::optional<StatePair>
  singlePositionBasedDeduction(model::Register::Values GPRegister,
                               model::Register::Values VRegister,
                               bool &IsRequired,
                               const InternalState &State) {
    const auto &ArgumentState = Accessor(State[GPRegister]);
    const auto &ReturnValueState = Accessor(State[VRegister]);

    StatePair Result;
    Result.GPR = ArgumentState.has_value() ? ArgumentState.value() :
                                             model::RegisterState::Maybe;
    Result.VR = ReturnValueState.has_value() ? ReturnValueState.value() :
                                               model::RegisterState::Maybe;

    // If only one register of the pair is specified, consider it to be
    // the dominant one.
    if (ArgumentState.has_value() != ReturnValueState.has_value()) {
      if (ArgumentState.has_value()) {
        if (IsRequired) {
          if (!model::RegisterState::isYesOrDead(Result.GPR))
            Result.GPR = model::RegisterState::YesOrDead;
          Result.VR = model::RegisterState::No;
        }
      } else if (ReturnValueState.has_value()) {
        if (IsRequired) {
          if (!model::RegisterState::isYesOrDead(Result.VR))
            Result.VR = model::RegisterState::YesOrDead;
          Result.GPR = model::RegisterState::No;
        }
      }
    }

    // Do the deduction
    if (model::RegisterState::isYesOrDead(Result.GPR)) {
      if (model::RegisterState::isYesOrDead(Result.VR)) {
        // Both are set - there's no way to tell which one is actually used.
        revng_log(Log,
                  "Impossible to differenciate which one of the two registers "
                  "(`model::Register::"
                    << model::Register::getName(GPRegister).data()
                    << "` and `model::Register::"
                    << model::Register::getName(VRegister).data()
                    << "`) should be used: both are `YesOrDead`. The ABI is "
                    << model::ABI::getName(ABI).data() << ".");
        return std::nullopt;
      } else {
        // Only GPR is set, ensure VR is marked as `No`.
        Result.VR = model::RegisterState::No;
        IsRequired = true;
      }
    } else {
      if (model::RegisterState::isYesOrDead(Result.VR)) {
        // Only VR is set, ensure GPR is marked as `No`.
        Result.GPR = model::RegisterState::No;
        IsRequired = true;
      } else {
        // Neither one is used. An error if one of the pair was required.
        if (IsRequired) {
          revng_log(Log,
                    "Impossible to differenciate which one of the two "
                    "registers "
                    "(`model::Register::"
                      << model::Register::getName(GPRegister).data()
                      << "` and `model::Register::"
                      << model::Register::getName(VRegister).data()
                      << "`) should be used: neither is `YesOrDead`. The ABI "
                         "is "
                      << model::ABI::getName(ABI).data() << ".");
          return std::nullopt;
        }
      }
    }

    return Result;
  }

  static std::optional<InternalState>
  runForNonPositionBasedABIs(const InternalState &State) {
    using CC = CallingConvention;
    if constexpr (CC::OnlyStartDoubleArgumentsFromAnEvenRegister) {
      // Only partial deduction is possible. It can be implemented if necessary.
      return std::nullopt;
    }

    // Separate all the registers before the "last used one" into separate
    // sub-ranges.

    auto &GPAR = CallingConvention::GeneralPurposeArgumentRegisters;
    size_t GPARCount = findLastUsedIndex<accessCArgument>(GPAR, State);
    auto UsedGPAR = llvm::ArrayRef(GPAR).take_front(GPARCount);

    auto &VAR = CallingConvention::VectorArgumentRegisters;
    size_t VARCount = findLastUsedIndex<accessCArgument>(VAR, State);
    auto UsedVAR = llvm::ArrayRef(VAR).take_front(VARCount);

    auto &GPRVR = CallingConvention::GeneralPurposeReturnValueRegisters;
    size_t GPRVRCount = findLastUsedIndex<accessCReturnValue>(GPRVR, State);
    auto UsedGPRVR = llvm::ArrayRef(GPRVR).take_front(GPRVRCount);

    auto &VRVR = CallingConvention::VectorReturnValueRegisters;
    size_t VRVRCount = findLastUsedIndex<accessCReturnValue>(VRVR, State);
    auto UsedVRVR = llvm::ArrayRef(VRVR).take_front(VRVRCount);

    // Merge sub-ranges together to get the final register sets.
    auto AllowedArgRegisters = llvm::concat<CRegister>(GPAR, VAR);
    auto AllowedRetValRegisters = llvm::concat<CRegister>(GPRVR, VRVR);
    auto UsedArgRegisters = llvm::concat<CRegister>(UsedGPAR, UsedVAR);
    auto UsedRetValRegisters = llvm::concat<CRegister>(UsedGPRVR, UsedVRVR);

    // Copy the state to serve as a return value.
    InternalState Result = State;

    // Try and apply deductions for each register. Abort if any of them fails.
    for (auto [Register, RegisterState] : Result) {
      auto &ArgumentState = accessArgument(RegisterState);
      bool IsArgAllowed = llvm::is_contained(AllowedArgRegisters, Register);
      bool IsArgRequired = llvm::is_contained(UsedArgRegisters, Register);
      if (auto A = singleNonPositionBasedDeduction(ArgumentState,
                                                   IsArgAllowed,
                                                   IsArgRequired,
                                                   Register))
        ArgumentState = A.value();
      else
        return std::nullopt;

      auto &RVState = accessReturnValue(RegisterState);
      bool IsRVAllowed = llvm::is_contained(AllowedRetValRegisters, Register);
      bool IsRVRequired = llvm::is_contained(UsedRetValRegisters, Register);
      if (auto RV = singleNonPositionBasedDeduction(RVState,
                                                    IsRVAllowed,
                                                    IsRVRequired,
                                                    Register))
        RVState = RV.value();
      else
        return std::nullopt;
    }

    return Result;
  }

  static detail::NullableRegisterState
  singleNonPositionBasedDeduction(const detail::NullableRegisterState &Input,
                                  bool IsAllowed,
                                  bool IsRequired,
                                  model::Register::Values Register) {
    auto Result = Input; // Copy the input to serve as a return value.

    // Normalize the state.
    if (Result == std::nullopt) {
      Result = model::RegisterState::Maybe;
    } else {
      // Ensure there are no `Invalid` states.
      revng_assert(Input != model::RegisterState::Invalid);
    }

    if (model::RegisterState::isYesOrDead(Result.value())) {
      if (!IsAllowed) {
        // If register is marked as "Yes" or "Dead" but is not usable, either
        // set it to `No` if `EnforceABIConformance == true` or abort.
        if constexpr (EnforceABIConformance == true) {
          revng_log(Log,
                    "Enforcing `model::Register::"
                      << model::Register::getName(Register).data()
                      << "` to `No` as `"
                      << model::ABI::getName(CallingConvention::ABI).data()
                      << "` ABI doesn't allow it to be used.");
          return model::RegisterState::No;
        } else {
          revng_log(Log,
                    "Aborting, `model::Register::"
                      << model::Register::getName(Register).data()
                      << "` register is used despite not being allowed by `"
                      << model::ABI::getName(CallingConvention::ABI).data()
                      << "` ABI.");
          return std::nullopt;
        }
      } else {
        // If a usable register is set, it's required by definition.
        revng_assert(IsRequired,
                     "Encountered an impossible state: the data structure is "
                     "probably corrupted.");
      }
    } else {
      // The input state is `No` or `Maybe` or equivalent,
      if (!IsAllowed) {
        // Set it to `No` if the ABI does not allow that register.
        return model::RegisterState::No;
      } else if (IsRequired) {
        // Set it to `YesOrDead` if it's required.
        return model::RegisterState::YesOrDead;
      }
    }

    return Result;
  }

  static const InternalState &unpackState(const RegisterStateMap &InputState) {
    if (std::holds_alternative<InternalState>(InputState.Internal))
      return std::get<InternalState>(InputState.Internal);
    else
      revng_abort("Architecture mismatch!");
  }
};

std::optional<RegisterStateMap>
applyRegisterStateDeductions(const RegisterStateMap &State,
                             model::ABI::Values ABI,
                             bool EnforceABIConformance) {
  revng_assert(ABI != model::ABI::Invalid);
  return skippingEnumSwitch<1>(ABI, [&]<model::ABI::Values A>() {
    std::optional<detail::Map> InternalResult;
    if (EnforceABIConformance)
      return DeductionImpl<A, true>::run(State);
    else
      return DeductionImpl<A, false>::run(State);
  });
}
