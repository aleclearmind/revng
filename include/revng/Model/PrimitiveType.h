#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

/* TUPLE-TREE-YAML
name: PrimitiveType
doc: // WIP
type: struct
inherits: Type2
fields:
  - name: PrimitiveKind
    type: PrimitiveTypeKind
  - name: Size
    type: uint64_t
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/PrimitiveType.h"

class model::PrimitiveType : public model::generated::PrimitiveType {
public:
  using generated::PrimitiveType::PrimitiveType;

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  RecursiveCoroutine<bool> verify(VerifyHelper &VH) const;

  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/PrimitiveType.h"
