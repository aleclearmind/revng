#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"

#include "revng/PTML/IndentingEmitter.h"

namespace ptml {

enum class Tagging : bool {
  Disabled,
  Enabled,
};

class TagEmitter;

struct EmitterLow {
  llvm::raw_ostream &OS;
  bool EmitTags = false;
  const TagEmitter *CurrentOpenTagEmitter = nullptr;

  void emitLiteral(llvm::StringRef String);
  void emitIndentation(unsigned Indentation);
};

/// \brief Provides a streaming interface for emitting PTML tags and content.
///
/// Underlying byte-IO is done via the provided llvm::raw_ostream reference.
///
/// PTML tag emission is performed using Emitter::TagEmitter, which is an RAII
/// object guaranteeing emission of well-formed PTML. Tag content is emitted
/// using the Emitter interface. See the documentation of Emitter::TagEmitter
/// for more information.
///
/// PTML tag emission can be toggled using the ptml::Tagging parameter. Note
/// that valid usage of the PTML tag emission interface is checked regardless
/// of whether PTML tag emission is enabled.
class Emitter : public EmitterLow {
private:
  public:
  IndentingEmitter<EmitterLow> Indenter;

public:
  explicit Emitter(llvm::raw_ostream &OS, Tagging Tags) :
    EmitterLow(OS, Tags == Tagging::Enabled), Indenter(*this) {}


  [[nodiscard]] bool isTagged() const { return EmitTags; }

  // Emit the specified content literally. The string shall not contain newlines
  // or characters requiring HTML escape sequences (<, >, &).
  void emitLiteralContent(llvm::StringRef String);

  void emitContentNewline() {
    revng_assert(CurrentOpenTagEmitter == nullptr,
                 "Content shall not emitted while an opening tag is "
                 "unfinalized.");
    return Indenter.emitNewline();
  }

  void emitContent(llvm::StringRef String);

  auto indent(int Offset) { return Indenter.indent(Offset); }
  auto indentation() { return Indenter.indentation(); }
  auto isAtBeginningOfLine() { return Indenter.isAtBeginningOfLine(); }

  [[nodiscard]] TagEmitter initializeOpenTag(llvm::StringRef Tag);

private:
  template<bool EscapeQuotes = false>
  void emitEscapedContent(llvm::StringRef String);

public:
  void emitAttributeValue(llvm::StringRef String);

};

/// \brief RAII object used for emitting PTML tags.
///
/// TagEmitter has three states:
/// 1. Uninitialized:
///    The TagEmitter has been default-constructed or initialised and
///    subsequently closed.
///
/// 2. Initialized:
///    Emission of the opening tag has been started, but not yet completed. It
///    is only in this state that emission of attributes is possible, while at
///    the same time emission of content via the associated ptml::Emitter is
///    disallowed.
///
///    This state can be entered using the initializeOpenTag member functions
///    of either Emitter or TagEmitter, or by constructing a TagEmitter using
///    its non-default constructor.
///
///    Emitter::initializeOpenTag is a convenience function which returns a
///    TagEmitter in the initialized state.
///
/// 3. Finalized:
///    Emission of the opening tag has been completed, but the closing tag has
///    not yet been emitted. In this state tag content can be emitted via the
///    associated ptml::Emitter.
///
///    This state is entered using the finalizeOpenTag member function.
///
/// The closing tag is emitted by explicitly calling the close member function,
/// or implicitly by the destructor, which also takes care of finalizing the
/// open tag if necessary.
///
/// At any given time, the emitter may be associated with multiple TagEmitters
/// but only the innermost can have an unfinalized open tag.
class TagEmitter {
  Emitter *ParentEmitter;
  llvm::StringRef Tag;
  bool IsOpenTagFinalized = false;

public:
  TagEmitter() : ParentEmitter(nullptr) {}

  explicit TagEmitter(Emitter &ParentEmitter, llvm::StringRef Tag) {
    revng_assert(ParentEmitter.CurrentOpenTagEmitter == nullptr,
                 "The parent Emitter is already associated with an "
                 "unfinalized TagEmitter.");

    initializeOpenTagImpl(ParentEmitter, Tag);
  }

  TagEmitter(const TagEmitter &) = delete;
  TagEmitter &operator=(const TagEmitter &) = delete;

  ~TagEmitter() { closeImpl(); }

  TagEmitter &initializeOpenTag(Emitter &ParentEmitter, llvm::StringRef Tag) & {
    revng_assert(this->ParentEmitter == nullptr,
                 "This TagEmitter was already initialized.");
    revng_assert(ParentEmitter.CurrentOpenTagEmitter == nullptr,
                 "The parent Emitter is already associated with an "
                 "unfinalized TagEmitter.");


    initializeOpenTagImpl(ParentEmitter, Tag);
    return *this;
  }

  TagEmitter &emitAttribute(llvm::StringRef Name, llvm::StringRef Value) & {
    revng_assert(ParentEmitter != nullptr,
                 "This TagEmitter has not been initialized.");
    revng_assert(not IsOpenTagFinalized,
                 "This TagEmitter has already been finalized.");

    emitAttributeImpl(Name, Value);
    return *this;
  }

  TagEmitter &emitListAttribute(llvm::StringRef Name,
                                llvm::ArrayRef<llvm::StringRef> Values) & {
    revng_assert(ParentEmitter != nullptr,
                 "This TagEmitter has not been initialized.");
    revng_assert(not IsOpenTagFinalized,
                 "This TagEmitter has already been finalized.");

    emitListAttributeImpl(Name, Values);
    return *this;
  }

  [[nodiscard]] bool isOpenTagFinalized() const { return IsOpenTagFinalized; }

  void finalizeOpenTag() {
    revng_assert(ParentEmitter != nullptr,
                 "This TagEmitter has not been initialized.");
    revng_assert(not IsOpenTagFinalized,
                 "This TagEmitter has already been finalized.");
    finalizeOpenTagImpl();
  }

  [[nodiscard]] bool isOpen() const { return ParentEmitter != nullptr; }

  void close() {
    revng_assert(ParentEmitter != nullptr,
                 "This TagEmitter has not been initialized.");
    closeImpl();
  }

private:
  void initializeOpenTagImpl(Emitter &ParentEmitter, llvm::StringRef Tag);

  void emitAttributeImpl(llvm::StringRef Name, llvm::StringRef Value);
  void emitListAttributeImpl(llvm::StringRef Name,
                             llvm::ArrayRef<llvm::StringRef> Values);

  void finalizeOpenTagImpl();
  void closeImpl();
};

inline TagEmitter Emitter::initializeOpenTag(llvm::StringRef Tag) {
  return TagEmitter(*this, Tag);
}


} // namespace ptml
