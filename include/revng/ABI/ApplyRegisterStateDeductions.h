#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <array>
#include <iterator>
#include <optional>
#include <span>
#include <variant>

#include "revng/ADT/STLExtras.h"
#include "revng/Model/ABI.h"
#include "revng/Model/Architecture.h"
#include "revng/Model/Register.h"
#include "revng/Model/RegisterState.h"
#include "revng/Support/EnumSwitch.h"

namespace detail {

using NullableRegisterState = std::optional<model::RegisterState::Values>;

struct State {
  NullableRegisterState IsUsedForPassingArguments = std::nullopt;
  NullableRegisterState IsUsedForReturningValues = std::nullopt;
};

template<model::Architecture::Values Architecture>
class RegisterStateMap {
public:
  State &at(model::Register::Values Requested) {
    static constexpr auto First = model::Register::getFirst<Architecture>();
    static constexpr auto Last = model::Register::getLast<Architecture>();
    revng_assert(Requested >= First && Requested <= Last);
    return Internal.at(Requested - First);
  }
  const State &at(model::Register::Values Requested) const {
    static constexpr auto First = model::Register::getFirst<Architecture>();
    static constexpr auto Last = model::Register::getLast<Architecture>();
    revng_assert(Requested >= First && Requested <= Last);
    return Internal.at(Requested - First);
  }

  State &operator[](model::Register::Values V) { return at(V); }
  const State &operator[](model::Register::Values V) const { return at(V); }

  size_t size() const { return Internal.size(); }

private:
  std::pair<model::Register::Values, State &> mapIt(State &Value) {
    static constexpr auto First = model::Register::getFirst<Architecture>();
    auto ResultIndex = First + std::distance(Internal.data(), &Value);
    return { model::Register::Values(ResultIndex), Value };
  }
  std::pair<model::Register::Values, const State &>
  mapIt(const State &Value) const {
    static constexpr auto First = model::Register::getFirst<Architecture>();
    auto ResultIndex = First + std::distance(Internal.data(), &Value);
    return { model::Register::Values(ResultIndex), Value };
  }

public:
  auto mapped_range() {
    auto L = [this](State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(Internal.begin(), L),
                            revng::map_iterator(Internal.end(), L));
  }
  auto reverse_mapped_range() {
    auto L = [this](State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(Internal.rbegin(), L),
                            revng::map_iterator(Internal.rend(), L));
  }
  auto const_mapped_range() const {
    auto L = [this](const State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(Internal.begin(), L),
                            revng::map_iterator(Internal.end(), L));
  }
  auto const_reverse_mapped_range() const {
    auto L = [this](const State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(Internal.rbegin(), L),
                            revng::map_iterator(Internal.rend(), L));
  }

public:
  auto begin() { return mapped_range().begin(); }
  auto rbegin() { return reverse_mapped_range().begin(); }
  auto cbegin() const { return const_mapped_range().begin(); }
  auto crbegin() const { return const_reverse_mapped_range().begin(); }

  auto end() { return mapped_range().end(); }
  auto rend() { return reverse_mapped_range().end(); }
  auto cend() const { return const_mapped_range().end(); }
  auto crend() const { return const_reverse_mapped_range().end(); }

  auto begin() const { return cbegin(); }
  auto end() const { return cend(); }

  std::span<State, model::Register::getCount<Architecture>()> staticView() {
    return Internal;
  }
  std::span<const State, model::Register::getCount<Architecture>()>
  staticView() const {
    return Internal;
  }

  std::span<State> dynamicView() { return Internal; }
  std::span<const State> dynamicView() const { return Internal; }

  static constexpr size_t getOffset() {
    return model::Register::getFirst<Architecture>();
  }

private:
  std::array<State, model::Register::getCount<Architecture>()> Internal;
};

template<typename InternalType,
         template<InternalType>
         typename ResultType,
         std::size_t Offset,
         std::size_t... Indices>
auto makeVariantImpl(std::integer_sequence<std::size_t, Indices...>) {
  return std::variant<ResultType<InternalType(Offset + Indices)>...>{};
}

inline constexpr std::integer_sequence
  Indices = std::make_index_sequence<model::Architecture::Count - 1>();
using Map = decltype(detail::makeVariantImpl<model::Architecture::Values,
                                             RegisterStateMap,
                                             1>(detail::Indices));

} // namespace detail

/// The map is used to represent the possibility of any register corresponding
/// to an architecture being used as either an argument or an return value.
class RegisterStateMap {
  template<model::ABI::Values ABI, bool EnforceABIConformance>
  friend struct DeductionImpl;

public:
  RegisterStateMap() = default;
  explicit RegisterStateMap(const detail::Map &Value) : Internal(Value) {}
  explicit RegisterStateMap(detail::Map &&Value) : Internal(std::move(Value)) {}
  RegisterStateMap &operator=(const detail::Map &NewValue) {
    Internal = NewValue;
    return *this;
  }
  RegisterStateMap &operator=(detail::Map &&NewValue) {
    Internal = std::move(NewValue);
    return *this;
  }

  detail::State &at(model::Register::Values Requested) {
    auto Visitor = [&Requested](auto &Visited) -> auto & {
      return Visited.at(Requested);
    };
    return std::visit(Visitor, Internal);
  }
  const detail::State &at(model::Register::Values Requested) const {
    auto Visitor = [&Requested](const auto &Visited) -> const auto & {
      return Visited.at(Requested);
    };
    return std::visit(Visitor, Internal);
  }

  detail::State &operator[](model::Register::Values Requested) {
    return at(Requested);
  }
  const detail::State &operator[](model::Register::Values Requested) const {
    return at(Requested);
  }

  size_t size() const {
    auto Visitor = [](const auto &Visited) { return Visited.size(); };
    return std::visit(Visitor, Internal);
  }

  std::span<detail::State> view() {
    auto Visitor = [](auto &Visited) { return Visited.dynamicView(); };
    return std::visit(Visitor, Internal);
  }
  std::span<const detail::State> view() const {
    auto Visitor = [](const auto &Visited) { return Visited.dynamicView(); };
    return std::visit(Visitor, Internal);
  }

private:
  std::pair<model::Register::Values, detail::State &>
  mapIt(detail::State &Value) {
    auto ResultIndex = getOffset() + std::distance(&view().front(), &Value);
    return { model::Register::Values(ResultIndex), Value };
  }
  std::pair<model::Register::Values, const detail::State &>
  mapIt(const detail::State &Value) const {
    auto ResultIndex = getOffset() + std::distance(&view().front(), &Value);
    return { model::Register::Values(ResultIndex), Value };
  }

  size_t getOffset() const {
    auto Visitor = [](const auto &Visited) { return Visited.getOffset(); };
    return std::visit(Visitor, Internal);
  }

  auto mapped_range() {
    auto L = [this](detail::State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(view().begin(), L),
                            revng::map_iterator(view().end(), L));
  }
  auto reverse_mapped_range() {
    auto L = [this](detail::State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(view().rbegin(), L),
                            revng::map_iterator(view().rend(), L));
  }
  auto const_mapped_range() const {
    auto L = [this](const detail::State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(view().begin(), L),
                            revng::map_iterator(view().end(), L));
  }
  auto const_reverse_mapped_range() const {
    auto L = [this](const detail::State &Value) { return this->mapIt(Value); };
    return llvm::make_range(revng::map_iterator(view().rbegin(), L),
                            revng::map_iterator(view().rend(), L));
  }

public:
  auto begin() { return mapped_range().begin(); }
  auto rbegin() { return reverse_mapped_range().begin(); }
  auto cbegin() const { return const_mapped_range().begin(); }
  auto crbegin() const { return const_reverse_mapped_range().begin(); }

  auto end() { return mapped_range().end(); }
  auto rend() { return reverse_mapped_range().end(); }
  auto cend() const { return const_mapped_range().end(); }
  auto crend() const { return const_reverse_mapped_range().end(); }

  auto begin() const { return cbegin(); }
  auto end() const { return cend(); }

private:
  detail::Map Internal;
};

constexpr inline RegisterStateMap
makeRegisterStateMap(model::Architecture::Values Arch) {
  RegisterStateMap Result;
  skippingEnumSwitch<1>(Arch, [&]<model::Architecture::Values A>() {
    Result = detail::Map(detail::RegisterStateMap<A>{});
  });
  return Result;
}

constexpr inline RegisterStateMap makeRegisterStateMap(model::ABI::Values ABI) {
  auto Arch = model::ABI::getArchitecture(ABI);
  revng_assert(Arch != model::Architecture::Invalid);

  return makeRegisterStateMap(Arch);
}

std::optional<RegisterStateMap>
applyRegisterStateDeductions(const RegisterStateMap &State,
                             model::ABI::Values ABI,
                             bool EnforceABIConformance = false);
