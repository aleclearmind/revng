#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

/* TUPLE-TREE-YAML
name: ArrayType
doc: // WIP
type: struct
inherits: Type2
fields:
  - name: UnderlyingType
    type: Type2
  - name: ElementsCount
    type: uint64_t
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/ArrayType.h"

class model::ArrayType : public model::generated::ArrayType {
public:
  using generated::ArrayType::ArrayType;

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  RecursiveCoroutine<bool> verify(VerifyHelper &VH) const;

  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/ArrayType.h"
