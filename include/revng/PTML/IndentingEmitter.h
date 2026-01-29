#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <ranges>

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"

#include "revng/Support/Assert.h"

namespace ptml {
namespace detail {

template<typename Derived>
concept IndentingEmitterDerived = requires(Derived &D) {
  D.emitLiteral(llvm::StringRef());
  D.emitIndentation(static_cast<unsigned>(0));
};

} // namespace detail

template<typename Derived>
class IndentingEmitter {
  Derived &XXX;
  unsigned Indentation = 0;
  bool IsAtBeginningOfLine = true;

public:
  IndentingEmitter(Derived &XXX) : XXX(XXX) {}

  void indent(int Offset) {
    revng_assert(Offset >= 0 or static_cast<unsigned>(-Offset) <= Indentation,
                 "Offset would result in negative indentation.");

    Indentation += static_cast<unsigned>(Offset);
  }

  [[nodiscard]] unsigned indentation() const { return Indentation; }

  [[nodiscard]] bool isAtBeginningOfLine() const { return IsAtBeginningOfLine; }

  void emit(llvm::StringRef String) {
    if (not String.empty()) {
      for (auto [I, R] : llvm::enumerate(std::views::split(String, '\n'))) {
        llvm::StringRef Line = std::string_view(R.begin(), R.end());

        if (I != 0)
          emitNewline();

        if (not Line.empty())
          emitLiteralImpl(Line);
      }

      IsAtBeginningOfLine = String.back() == '\n';
    }
  }

  void emitLiteral(llvm::StringRef String) {
    if (not String.empty()) {
      revng_assert(not String.contains('\n'));
      emitLiteralImpl(String);
    }
  }

  void emitNewline() {
    derived()->emitLiteral(llvm::StringRef("\n"));
    IsAtBeginningOfLine = true;
  }

public:
  IndentingEmitter() {
    static_assert(detail::IndentingEmitterDerived<Derived>);
  }

  Derived *derived() { return &XXX; }

  void emitIndentationIfNeeded() {
    if (IsAtBeginningOfLine) {
      IsAtBeginningOfLine = false;
      derived()->emitIndentation(Indentation);
    }
  }

private:
  void emitLiteralImpl(llvm::StringRef String) {
    revng_assert(not String.empty());
    emitIndentationIfNeeded();
    derived()->emitLiteral(String);
  }
};

} // namespace ptml
