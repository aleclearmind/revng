#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/Model/Architecture.h"

/* TUPLE-TREE-YAML
name: RelocationType
doc: Enum for identifying different kind of model types
type: enum
members:
  - name: WriteAbsoluteAddress32
  - name: WriteAbsoluteAddress64
  - name: AddAbsoluteAddress32
  - name: AddAbsoluteAddress64
  - name: WriteRelativeAddress32
  - name: WriteRelativeAddress64
  - name: AddRelativeAddress32
  - name: AddRelativeAddress64
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/RelocationType.h"

const unsigned char R_MIPS_IMPLICIT_RELATIVE = 255;

namespace model::RelocationType {

Values fromELFRelocation(model::Architecture::Values Architecture,
                         unsigned char ELFRelocation);

} // namespace model::RelocationType

#include "revng/Model/Generated/Late/RelocationType.h"
