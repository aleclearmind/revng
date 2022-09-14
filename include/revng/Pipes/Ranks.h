#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/Model/DynamicFunction.h"
#include "revng/Model/Function.h"
#include "revng/Model/Segment.h"
#include "revng/Model/Type.h"
#include "revng/Pipeline/Rank.h"
#include "revng/Support/MetaAddress.h"
#include "revng/Support/YAMLTraits.h"

namespace revng::ranks {

inline auto Binary = pipeline::defineRootRank<"binary">("The root rank "
                                                        "representing the "
                                                        "whole binary");

using pipeline::defineRank;
inline auto Function = defineRank<"function", model::Function::Key>("An "
                                                                    "individual"
                                                                    " function "
                                                                    "in the "
                                                                    "binary, "
                                                                    "identified"
                                                                    " by its "
                                                                    "entry "
                                                                    "address.",
                                                                    Binary);
inline auto BasicBlock = defineRank<"basic-block", MetaAddress>("A basic block "
                                                                "within a "
                                                                "specific "
                                                                "function, "
                                                                "identified by "
                                                                "its start "
                                                                "address.",
                                                                Function);
inline auto Instruction = defineRank<"instruction", MetaAddress>("An "
                                                                 "instruction "
                                                                 "within a "
                                                                 "specific "
                                                                 "basic block, "
                                                                 "within a "
                                                                 "specific "
                                                                 "function, "
                                                                 "identified "
                                                                 "by its "
                                                                 "address.",
                                                                 BasicBlock);

inline auto Type = defineRank<"type", model::Type::Key>("A type, identified by "
                                                        "its kind and unique "
                                                        "identifier.",
                                                        Binary);
// WIP: we should split these
inline auto TypeField = defineRank<"type-field", std::tuple<uint64_t>>("A "
                                                                       "field "
                                                                       "in a "
                                                                       "struct "
                                                                       "or a "
                                                                       "union. "
                                                                       "In the "
                                                                       "former "
                                                                       "case "
                                                                       "the "
                                                                       "identif"
                                                                       "ier is "
                                                                       "the "
                                                                       "offset "
                                                                       "of the "
                                                                       "field, "
                                                                       "in the "
                                                                       "latter "
                                                                       "the "
                                                                       "identif"
                                                                       "ier is "
                                                                       "the "
                                                                       "index "
                                                                       "of the "
                                                                       "entry "
                                                                       "of the "
                                                                       "union.",
                                                                       Type);

inline auto RawByte = defineRank<"raw-byte", MetaAddress>("A pointer to raw "
                                                          "data in the binary, "
                                                          "identified by its "
                                                          "address.",
                                                          Binary);
// WIP: key should be uint64_t
inline auto RawByteRange = defineRank<"raw-byte-range", MetaAddress>("A "
                                                                     "pointer "
                                                                     "to a "
                                                                     "range of "
                                                                     "raw data "
                                                                     "in the "
                                                                     "binary, "
                                                                     "identifie"
                                                                     "d by its "
                                                                     "start "
                                                                     "and end "
                                                                     "address.",
                                                                     RawByte);

inline auto Segment = defineRank<"segment", model::Segment::Key>("A segment, "
                                                                 "identified "
                                                                 "by its "
                                                                 "`StartAddress"
                                                                 "` and "
                                                                 "`VirtualSize`"
                                                                 ".",
                                                                 Binary);

inline auto DynamicFunction = defineRank<"dynamic-function",
                                         model::DynamicFunction::Key>("A "
                                                                      "dynamic "
                                                                      "function"
                                                                      ", "
                                                                      "identifi"
                                                                      "ed by "
                                                                      "its "
                                                                      "`Origina"
                                                                      "lName`.",
                                                                      Binary);

} // namespace revng::ranks
