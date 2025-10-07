//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/PipeboxCommon/Helpers/Native/Helpers.h"
#include "revng/PipeboxCommon/Helpers/Python/Helpers.h"
#include "revng/PipeboxCommon/Helpers/Registrars.h"

#include "MockPipeboxImpl.h"

static RegisterContainer<StringContainer> X;
static RegisterPipe<AppendFooPipe> Y;
static RegisterAnalysis<AppendFooLibAnalysis> Z;
