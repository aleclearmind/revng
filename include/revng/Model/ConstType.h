#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

/* TUPLE-TREE-YAML
name: ConstType
doc: // WIP
type: struct
inherits: Type2
fields:
  - name: UnderlyingType
    type: Type2
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/ConstType.h"

class model::ConstType : public model::generated::ConstType {
public:
  using generated::ConstType::ConstType;

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  RecursiveCoroutine<bool> verify(VerifyHelper &VH) const;

  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/ConstType.h"
