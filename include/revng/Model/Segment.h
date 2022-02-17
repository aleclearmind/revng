#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include "revng/ADT/SortedVector.h"
#include "revng/Model/Identifier.h"
#include "revng/Model/Section.h"
#include "revng/Model/VerifyHelper.h"
#include "revng/Support/MetaAddress.h"
#include "revng/Support/MetaAddress/YAMLTraits.h"

// WIP: s/EndAddress/Size/
/* TUPLE-TREE-YAML
name: Segment
type: struct
fields:
  - name: StartAddress
    type: MetaAddress
  - name: EndAddress
    type: MetaAddress
  - name: StartOffset
    type: uint64_t
  - name: EndOffset
    type: uint64_t
  - name: IsReadable
    type: bool
  - name: IsWriteable
    type: bool
  - name: IsExecutable
    type: bool
  - name: Name
    type: std::string
    optional: true
  - name: Relocations
    optional: true
    sequence:
      type: SortedVector
      elementType: model::Relocation
  - name: Sections
    optional: true
    sequence:
      type: SortedVector
      elementType: model::Section

key:
  - StartAddress
  - EndAddress
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/Segment.h"

class model::Segment : public model::generated::Segment {
public:
  using generated::Segment::Segment;

public:
  Identifier name() const;

public:
  bool contains(MetaAddress Address) const {
    return (StartAddress.addressLowerThanOrEqual(Address)
            and Address.addressLowerThan(EndAddress));
  }

  bool contains(MetaAddress Start, uint64_t Size) const {
    // WIP: check for overflow
    return contains(Start) and (Size <= 1 or contains(Start + Size - 1));
  }

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  bool verify(VerifyHelper &VH) const;
  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/Segment.h"
