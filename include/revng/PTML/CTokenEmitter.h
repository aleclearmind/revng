#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"

#include "revng/PTML/Emitter.h"
#include "revng/Support/CTarget.h"

class CTokenEmitter {
public:
  class CommentEmitter;

private:
  // It is very important to hide the PTML emitter and not to expose any direct
  // access to it in the public interface of this class. This design prevents
  // the emission of lexically invalid C.
  ptml::Emitter PTML;

  // Used to ensure that only one comment emitter may exist at any given time.
  bool IsEmittingComment = false;

public:
  explicit CTokenEmitter(llvm::raw_ostream &OS, ptml::Tagging Tags) :
    PTML(OS, Tags) {}

  void emitSpace() { PTML.emitLiteralContent(" "); }

  void emitNewline() { PTML.emitContentNewline(); }

  enum class Keyword {
    Auto,
    Bool,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    False,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    True,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
  };

  void emitKeyword(Keyword K);

  enum class Punctuator {
    Colon,
    Comma,
    Dot,
    Equals,
    LeftBrace,
    LeftBracket,
    LeftParenthesis,
    RightBrace,
    RightBracket,
    RightParenthesis,
    Semicolon,
    Star,
  };

  void emitPunctuator(Punctuator P);

  enum class Operator {
    Ampersand,
    AmpersandAmpersand,
    AmpersandEquals,
    Arrow,
    Caret,
    CaretEquals,
    Colon,
    Comma,
    Dot,
    Equals,
    EqualsEquals,
    Exclaim,
    ExclaimEquals,
    Greater,
    GreaterEquals,
    GreaterGreater,
    GreaterGreaterEquals,
    LeftBracket,
    LeftParenthesis,
    Less,
    LessEquals,
    LessLess,
    LessLessEquals,
    Minus,
    MinusEquals,
    MinusMinus,
    Percent,
    PercentEquals,
    Pipe,
    PipeEquals,
    PipePipe,
    Plus,
    PlusEquals,
    PlusPlus,
    Question,
    RightBracket,
    RightParenthesis,
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    Tilde,
  };

  void emitOperator(Operator O);

  enum class EntityKind {
    Primitive,
    Typedef,

    Enum,
    Enumerator,

    Struct,
    Union,
    Field,

    GlobalVariable,
    LocalVariable,

    Function,
    FunctionParameter,

    Label,

    Attribute,
    AttributeArgument,
  };

  enum class IdentifierKind : bool {
    Reference,
    Definition,
  };

  /// \pre \param Identifier matches `[_a-zA-Z][_a-zA-Z0-9]*`.
  void emitIdentifier(llvm::StringRef Identifier,
                      llvm::StringRef Location,
                      EntityKind Kind,
                      IdentifierKind IsDefinition);

  /// \pre \param Identifier matches `[_a-zA-Z][_a-zA-Z0-9]*`.
  void emitLiteralIdentifier(llvm::StringRef Identifier);

  // TODO: There is currently no API for emitting character literals, because
  //       there are no Clift users of such an API. Whenever support for
  //       emitting character literals is needed, another function should be
  //       added for that purpose.

  /// \pre \param Radix must be one of 2, 8, 10 or 16.
  void
  emitIntegerLiteral(llvm::APSInt Value, CIntegerKind Type, unsigned Radix);

  void emitStringLiteral(llvm::StringRef Content);

  enum class CommentKind : bool {
    Line,
    Block,
  };

  class CommentEmitter {
    CTokenEmitter &Emitter;
    CommentKind Kind;
    ptml::TagEmitter Tag;
    bool IsAtBeginningOfLine = false;

  public:
    explicit CommentEmitter(CTokenEmitter &Emitter, CommentKind Kind);

    CommentEmitter(const CommentEmitter &) = delete;
    CommentEmitter &operator=(const CommentEmitter &) = delete;

    ~CommentEmitter();

    [[nodiscard]] ptml::TagEmitter initializeOpenTag(llvm::StringRef Tag) {
      return ptml::TagEmitter(Emitter.PTML, Tag);
    }

    void emitContent(llvm::StringRef Content);

  private:
    void emitLinePrefix();
    void emitEscaped(llvm::StringRef Content);
  };

  [[nodiscard]] CommentEmitter emitComment(CommentKind Kind) {
    return CommentEmitter(*this, Kind);
  }

  void emitComment(llvm::StringRef Content, CommentKind Kind);

  enum class IncludeMode : bool {
    Quote,
    Angle,
  };

  void emitIncludeDirective(llvm::StringRef Content,
                            llvm::StringRef Location,
                            IncludeMode Mode);

  enum class ScopeKind : uint8_t {
    None,
    EnumDefinition,
    StructDefinition,
    UnionDefinition,
    FunctionDeclaration,
    FunctionDefinition,
    BlockStatement,
  };

  enum class Delimiter : uint8_t {
    None,
    Braces,
  };

  class Scope {
  public:
    explicit Scope(CTokenEmitter &Emitter,
                   ScopeKind Kind,
                   Delimiter Delimiter,
                   int Indent) :
      Emitter(Emitter), Delimiter(Delimiter), Indent(Indent) {
      Emitter.enterScopeImpl(Tag, Delimiter, Indent, Kind);
    }

    Scope(const Scope &) = delete;
    Scope &operator=(const Scope &) = delete;

    ~Scope() { Emitter.leaveScopeImpl(Tag, Delimiter, Indent); }

  private:
    CTokenEmitter &Emitter;

    Delimiter Delimiter;
    int Indent;

    ptml::TagEmitter Tag;
  };

  [[nodiscard]] Scope
  enterScope(ScopeKind Kind, Delimiter Delimiter, int Indent = 1) {
    return Scope(*this, Kind, Delimiter, Indent);
  }

  enum class RegionKind : uint8_t {
    Expression,
  };

  class Region {
    ptml::TagEmitter Tag;

  public:
    explicit Region(CTokenEmitter &Emitter,
                    RegionKind Kind,
                    llvm::StringRef Location) {
      Emitter.enterRegionImpl(Tag, Kind, Location);
    }

    Region(const Region &) = delete;
    Region &operator=(const Region &) = delete;
  };

  [[nodiscard]] Region enterRegion(RegionKind Kind, llvm::StringRef Location) {
    return Region(*this, Kind, Location);
  }

private:
  void enterScopeImpl(ptml::TagEmitter &Tag,
                      Delimiter Delimiter,
                      int Indent,
                      ScopeKind Kind);

  void leaveScopeImpl(ptml::TagEmitter &Tag, Delimiter Delimiter, int Indent);

  void enterRegionImpl(ptml::TagEmitter &Tag,
                       RegionKind Kind,
                       llvm::StringRef Location);
};
