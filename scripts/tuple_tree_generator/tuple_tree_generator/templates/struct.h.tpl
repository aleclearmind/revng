#pragma once

/*#-
This template file is distributed under the MIT License. See LICENSE.md for details.
The notice below applies to the generated files.
#*/
//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

// This file is autogenerated! Do not edit it directly

#include <compare>

/**- for header in struct.includes **/
#include "/*= generator.user_include_path =*//*= header =*/"
/**- endfor **/

/*= struct.doc | docstring -=*/
struct /*= struct.fullname =*/
  /**- if struct.inherits **/ : public /*= struct.inherits.user_fullname =*/ /** endif -**/
{
  /*#- --- Member list --- #*/
  /**- for field in struct.fields **/
  /*= field.doc | docstring =*/
  /**- if field.const **/const /** endif -**/
  /*= field.type =*/ /*= field.name =*/ = /*= field.type =*/{};
  /**- endfor **/

  /*# --- Default constructor --- #*/
  /// Default constructor
  /*= struct.name =*/() :
    /**- if struct.inherits **//*= struct.inherits.name =*/()/** endif **/
    /**- for field in struct.fields **/
    /**- if not loop.first or struct.inherits **/, /** endif **//*= field.name =*/()
    /**- endfor **/ {}

  /*# --- Key constructor --- #*/
  /**- if struct.key_fields **/
  /// Key constructor
  /*= struct.name =*/(
    /**- for field in struct.key_fields **/
    /*=- field.type =*/ /*= field.name =*//** if not loop.last **/, /** endif **/
    /**- endfor **/
  ) :
    /**- if struct.inherits **/
    /*=- struct.inherits.name =*/(
      /**- for field in struct.key_fields **/
      /*=- field.name =*//** if not loop.last **/, /** endif **/
      /**- endfor **/
    )
    /**- else **/
    /**- for field in struct.key_fields **/
    /*=- field.name =*/(/*= field.name =*/)/** if not loop.last **/, /** endif **/
    /**- endfor **/
    /**- endif **/ {}
  /** endif **/

  /*# --- Full constructor --- #*/
  /** if struct.emit_full_constructor **/
  /// Full constructor
  /*= struct.name =*/(
    /*#- Inherited fields #*/
    /**- for field in struct.inherits.fields **/
    /*=- field.type =*/ /*= field.name =*/,
    /**- endfor **/

    /*#- Own fields #*/
    /**- for field in struct.fields **/
    /*=- field.type =*/ /*= field.name =*//** if not loop.last **/, /** endif **/
    /**- endfor **/
  ) :
    /*#- Invoke base class constructor #*/
    /**- if struct.inherits **/
    /*= struct.inherits.name =*/(
      /**- for field in struct.inherits.fields **/
      /*= field.name =*//** if not loop.last **/, /** endif **/
      /**- endfor **/
    ), /** endif **/

    /*#- Initialize own fields #*/
    /**- for field in struct.fields **/
    /*=- field.name =*/(/*= field.name =*/)/** if not loop.last **/, /** endif **/
    /**- endfor **/ {}
  /** endif **/

  /*# --- Key definition for KeyedObjectTraits --- #*/
  /** if struct._key **/
  using Key = std::tuple<
    /**- for key_field in struct.key_fields -**/
    /*= key_field.type =*//** if not loop.last **/, /** endif **/
    /**- endfor -**/
  >;
  /** endif **/

  /*# --- Comparison operator --- #*/
  /** if struct.key_fields **/
  Key key() const {
    return Key {
      /**- for key_field in struct.key_fields -**/
      /*= key_field.name =*//** if not loop.last **/, /** endif **/
      /**- endfor -**/
    };
  }
  bool operator==(const /*= struct.name =*/ &Other) const { return key() == Other.key(); }
  bool operator<(const /*= struct.name =*/ &Other) const { return key() < Other.key(); }
  bool operator>(const /*= struct.name =*/ &Other) const { return key() > Other.key(); }

  /** else **/
  bool operator==(const /*= struct.name =*/ &Other) const = default;
  /** endif **/

  bool localCompare(const /*= struct.name =*/ &Other) const;

  static constexpr const char *Tag = "!/*= struct.tag =*/";
};

/*# --- UpcastablePointer stuff --- #*/
/** if upcastable **/
/*# Emit both const and non-const specialization of concrete_types_traits #*/
/** for const_qualifier in ["", "const"] **/
template<>
struct concrete_types_traits</*= const_qualifier =*/ /*= struct.user_fullname =*/> {
  using type = std::tuple<
    /**- for child_type in upcastable|sort(attribute="user_fullname") **/
    /*=- const_qualifier =*/ /*= child_type.user_fullname =*//** if not loop.last **/, /** endif **/
    /**- endfor **/
    /**- if not struct.abstract **/, /*=- const_qualifier =*/ /*= struct.user_fullname =*//** endif **/>;
};
/** endfor **/
/** endif **//*# End UpcastablePointer stuff #*/


inline bool /*= struct.fullname =*/::localCompare(const /*= struct.fullname =*/ &Other) const {
  /** for field in struct.fields if field.__class__.__name__ != "ReferenceStructField" **/

  /**- if field.__class__.__name__ == "SimpleStructField" **/

  /**- if generator.get_definition_for(field.type).__class__.__name__ == "StructDefinition" -**/
  if (not this->/*= field.name =*/.localCompare(Other./*= field.name =*/)) return false;
  /**- else -**/
  if (not (this->/*= field.name =*/ == Other./*= field.name =*/)) return false;
  /**- endif -**/

  /**- elif field.__class__.__name__ == "SequenceStructField" -**/
  /**- if generator.get_definition_for(field.element_type).__class__.__name__ == "StructDefinition" -**/
  // Lamdas cannot capture structured bindings
  for (const auto &LRTuple : llvm::zip(this->/*= field.name =*/, Other./*= field.name =*/)) {
    const auto &L = std::get<0>(LRTuple);
    const auto &R = std::get<1>(LRTuple);

    /** if field.upcastable **/
    bool CompareFails = false;
    upcast(L, [&R, &CompareFails](const auto &UpcastedL){
      upcast(R, [&UpcastedL, &CompareFails](const auto &UpcastedR){
        if constexpr (not std::is_same_v<decltype(UpcastedL), decltype(UpcastedR)>) {
          CompareFails = true;
          return;
        } else if (not UpcastedL.localCompare(UpcastedR)) CompareFails = true;
      });
    });
    if (CompareFails) return false;

    /** else **/
    if (not L.localCompare(R)) return false;
    /** endif **/

  }
  /**- else -**/
  if (not (this->/*= field.name =*/ == Other./*= field.name =*/)) return false;
  /**- endif -**/

  /** else **//*= ERROR("unexpected field type") =*//** endif **/

  /** endfor **/

  return true;
}

