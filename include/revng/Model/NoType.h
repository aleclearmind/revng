#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

/* TUPLE-TREE-YAML
name: NoType
doc: // WIP
type: struct
inherits: Type2
fields: []
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/NoType.h"

class model::NoType : public model::generated::NoType {
public:
  using generated::NoType::NoType;

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  RecursiveCoroutine<bool> verify(VerifyHelper &VH) const;

  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/NoType.h"
