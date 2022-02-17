#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/Model/VerifyHelper.h"
#include "revng/Support/MetaAddress.h"
#include "revng/Support/MetaAddress/YAMLTraits.h"

// WIP: ContainsCode == true implies it's in an exectuable segment
// WIP: it must lie within the segment

/* TUPLE-TREE-YAML
name: Section
type: struct
fields:
  - name: StartAddress
    type: MetaAddress
  - name: Size
    type: uint64_t
  - name: Name
    type: std::string
  - name: ContainsCode
    type: bool

key:
  - StartAddress
  - Size
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/Section.h"

class model::Section : public model::generated::Section {
public:
  using generated::Section::Section;

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  bool verify(VerifyHelper &VH) const;
  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/Section.h"
