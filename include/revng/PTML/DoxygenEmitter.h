#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/PTML/CommentEmitter.h"
#include "revng/PTML/Constants.h"

namespace ptml {

struct DoxygenCommentConfiguration {
  char KeywordSignifier = '\\';
  std::optional<llvm::StringRef> CommentHeader;
  std::optional<llvm::StringRef> CommentFooter;
  llvm::StringRef LinePrefix;
};

template<CommentEmitter CommentEmitterT>
struct DoxygenCommentEmitterLow {
  CommentEmitterT &Emitter;
  DoxygenCommentConfiguration Configuration;
  static constexpr llvm::StringRef IndentString = "  ";

  void emitLiteral(llvm::StringRef String) { Emitter.emitContent(String); }

  void emitIndentation(unsigned Indentation) {
    Emitter.emitContent(Configuration.LinePrefix);
    for (unsigned I = 0, C = Indentation; I < C; ++I)
      Emitter.emitContent(IndentString);
  }

};

template<CommentEmitter CommentEmitterT>
class DoxygenCommentEmitter {


  static constexpr llvm::StringRef IndentString = "  ";

  CommentEmitterT Emitter;
  DoxygenCommentEmitterLow<CommentEmitterT> Low;
  IndentingEmitter<DoxygenCommentEmitterLow<CommentEmitterT>> Indenter;
  DoxygenCommentConfiguration Configuration;

public:

  template<typename... ArgsT>
    requires std::constructible_from<CommentEmitterT, ArgsT...>
  explicit DoxygenCommentEmitter(DoxygenCommentConfiguration Configuration,
                                 ArgsT &&...Args) :
    Emitter(std::forward<ArgsT>(Args)...), Low(Emitter, Configuration), Indenter(Low), Configuration(Configuration) {
    if (Configuration.CommentHeader) {
      Emitter.emitContent(*Configuration.CommentHeader);
      Indenter.emitNewline();
    }
  }

  void emitKeyword(llvm::StringRef Keyword) {
    auto Tag = Emitter.initializeOpenTag(ptml::tags::Span);
    Tag.emitAttribute(ptml::attributes::Token, ptml::doxygen::tokens::Keyword);
    Tag.finalizeOpenTag();

    llvm::StringRef Signifier(&Configuration.KeywordSignifier, 1);
    Indenter.emit(Signifier);
    Indenter.emit(Keyword);
  }

  DoxygenCommentEmitter(const DoxygenCommentEmitter &) = delete;
  DoxygenCommentEmitter &operator=(const DoxygenCommentEmitter &) = delete;

  ~DoxygenCommentEmitter() {
    if (Configuration.CommentFooter) {
      if (not Indenter.isAtBeginningOfLine())
        Indenter.emitNewline();
      Emitter.emitContent(*Configuration.CommentFooter);
    }
  }

  void emitContent(llvm::StringRef Content) {
    Indenter.emit(Content);
  }

  void emitContentNewline() {
    Indenter.emitNewline();
  }

};

} // namespace ptml
