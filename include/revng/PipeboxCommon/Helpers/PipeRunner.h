#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <utility>

#include "revng/Dunno.h"
#include "revng/PipeboxCommon/Common.h"
#include "revng/PipeboxCommon/Concepts.h"
#include "revng/PipeboxCommon/Model.h"

namespace revng::pypeline::helpers {

/// Helper function that allows running a pipe, deals with unpacking the
/// container list to multiple parameters that will be passed to the run
/// function to of the Pipe.
template<IsPipe T, typename ListType>
inline ObjectDependencies runPipe(T &Pipe,
                                  const Model &TheModel,
                                  const Request &Incoming,
                                  const Request &Outgoing,
                                  llvm::StringRef Configuration,
                                  ListType &Containers) {
  using Traits = PipeRunTraits<T>;
  revng_assert(Incoming.size() == Traits::ContainerCount);
  revng_assert(Outgoing.size() == Traits::ContainerCount);

  auto Runner = ([&]<size_t... ContainerIndexes>(const std::index_sequence<
                                                 ContainerIndexes...> &) {
    return Pipe.run(TheModel,
                    Incoming,
                    Outgoing,
                    Configuration,
                    ExtractContainerFromList<
                      std::tuple_element_t<ContainerIndexes,
                                           typename Traits::ContainerTypes>,
                      ContainerIndexes,
                      ListType &>::get(Containers)...);
  });
  return Runner(std::make_index_sequence<Traits::ContainerCount>());
}

} // namespace revng::pypeline::helpers
