#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

/* TUPLE-TREE-YAML
name: Type2
doc: // WIP
type: struct
fields:
  - name: Kind
    type: Type2Kind
  - name: IsConst
    type: bool
    optional: true
key: [Kind]
abstract: true
TUPLE-TREE-YAML */

#include "revng/Model/Generated/Early/Type2.h"

class model::Type2 : public model::generated::Type2 {
public:
  using generated::Type2::Type2;

public:
  static bool classof(const Type2 *T) { return classof(T->key()); }
  static bool classof(const Key &K) { return true; }

public:
#if 0
  std::optional<uint64_t> size() const debug_function;
  RecursiveCoroutine<std::optional<uint64_t>> size(VerifyHelper &VH) const;

  std::optional<uint64_t> trySize() const debug_function;
  RecursiveCoroutine<std::optional<uint64_t>> trySize(VerifyHelper &VH) const;

public:
  /// Checks if is a scalar type, unwrapping typedefs
  bool isScalar() const;
  /// Checks if is a primitive type, unwrapping typedefs
  bool isPrimitive() const;
  /// Checks if is a primitive type of a specific kind, unwrapping typedefs
  bool isPrimitive(model::PrimitiveTypeKind::Values V) const;
  /// Checks if is float, unwrapping typedefs
  bool isFloat() const { return isPrimitive(model::PrimitiveTypeKind::Float); }
  /// Checks if is void, unwrapping typedefs
  bool isVoid() const { return isPrimitive(model::PrimitiveTypeKind::Void); }
  /// Checks if is an array type, unwrapping typedefs
  bool isArray() const;
  /// Checks if is a pointer type, unwrapping typedefs
  bool isPointer() const;
  /// Checks if is a const type, unwrapping typedefs
  bool isConst() const;
  /// Checks if is of a given TypeKind, unwrapping typedefs
  bool is(model::TypeKind::Values K) const;
#endif

public:
  bool verify() const debug_function;
  bool verify(bool Assert) const debug_function;
  RecursiveCoroutine<bool> verify(VerifyHelper &VH) const;

  /// Returns a pseudo C representation of the type
  ///
  /// \note This is for debug purposes only. It's not a valid C type.
  std::string pseudoC() const debug_function;

  void dump() const debug_function;
};

#include "revng/Model/Generated/Late/Type2.h"
