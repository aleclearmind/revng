#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <optional>

#include "revng/Model/ABI.h"
#include "revng/Model/Binary.h"

/// Creates the "default" prototype for a function.
///
/// Such a prototype considers all the possible ways to pass
/// an argument in or an return value out as used.
///
/// If `ABI` parameter was not provided, `BinaryToRecordTheTypeAt.DefaultABI`
/// is used instead.
model::TypePath
defaultFunctionPrototype(model::Binary &BinaryToRecordTheTypeAt,
                         std::optional<model::ABI::Values> ABI = std::nullopt);
