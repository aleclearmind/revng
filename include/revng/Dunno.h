#pragma once

#include <cstdlib>

namespace revng::pypeline::helpers {
template<typename C, size_t I, typename ListType>
struct ExtractContainerFromList {
  static C &get(ListType Containers);
};
} // namespace revng::pypeline::helpers
