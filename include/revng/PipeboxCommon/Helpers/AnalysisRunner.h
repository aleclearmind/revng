#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <utility>

#include "llvm/Support/Error.h"

#include "revng/Dunno.h"
#include "revng/PipeboxCommon/Common.h"
#include "revng/PipeboxCommon/Concepts.h"
#include "revng/PipeboxCommon/Model.h"

namespace revng::pypeline::helpers {

/// Helper function that allows running an analysis, deals with unpacking the
/// container list to multiple parameters that will be passed to the run
/// function to of the Analysis.
template<IsAnalysis T, typename ListType, typename... ContainersT>
inline llvm::Error runAnalysis(T &Analysis,
                               llvm::Error (T::*RunMethod)(Model &,
                                                           const Request &,
                                                           llvm::StringRef,
                                                           ContainersT...),
                               Model &TheModel,
                               const Request &Incoming,
                               llvm::StringRef Configuration,
                               ListType &Containers) {
  revng_assert(Incoming.size() == sizeof...(ContainersT));

  auto Runner = ([&]<size_t... ContainerIndexes>(const index_sequence<
                                                 ContainerIndexes...> &) {
    return (Analysis.*RunMethod)(TheModel,
                                 Incoming,
                                 Configuration,
                                 ExtractContainerFromList<
                                   std::remove_reference_t<ContainersT>,
                                   ContainerIndexes,
                                   ListType &>::get(Containers)...);
  });
  return Runner(std::make_integer_sequence<size_t, sizeof...(ContainersT)>());
}

} // namespace revng::pypeline::helpers
