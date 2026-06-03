///

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/Support/ExplicitSpecializations.h"

using IntVector = std::vector<unsigned int>;

#if 0
template void
IntVector::__push_back_slow_path<const unsigned int &>(const unsigned int &);
#endif
