#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

/* TUPLE-TREE-YAML
name: DefinedType
doc: // WIP
type: struct
inherits: Type2
fields:
  - name: Definition
    reference:
      pointeeType: Type
      rootType: Binary
    optional: true
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/DefinedType.h"

class model::DefinedType : public model::generated::DefinedType {
public:
  using generated::DefinedType::DefinedType;

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  RecursiveCoroutine<bool> verify(VerifyHelper &VH) const;

  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/DefinedType.h"
