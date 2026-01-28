#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/PTML/Emitter.h"

namespace ptml {

template<typename CommentEmitterT>
concept CommentEmitter = requires(CommentEmitterT &E, llvm::StringRef S) {
  E.emitContent(S);
  { E.initializeOpenTag(S) } -> std::same_as<TagEmitter>;
};

} // namespace ptml
