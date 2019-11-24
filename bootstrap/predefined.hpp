/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "str.hpp"

namespace herschel {
// predefined names and constants

//--------------------------------------------------------------------------
// definitions of the reserved keywords
#define MID_FUNCTIONid "Function"
#define MID_add "add"
#define MID_append "append"
#define MID_bitand "bitand"
#define MID_bitor "bitor"
#define MID_bitxor "bitxor"
#define MID_byid "by"
#define MID_castto "cast-to"
#define MID_compare "compare"
#define MID_concat "concat"
#define MID_defid "def"
#define MID_divide "divide"
#define MID_elseid "else"
#define MID_eofid "eof"
#define MID_equalq "equal?"
#define MID_exponent "exponent"
#define MID_exportid "export"
#define MID_externid "extern"
#define MID_fold "fold"
#define MID_forid "for"
#define MID_functionid "function"
#define MID_greaterequalq "greater-equal?"
#define MID_greaterq "greater?"
#define MID_ifid "if"
#define MID_importid "import"
#define MID_in "in"
#define MID_includeid "include"
#define MID_isaQ "isa?"
#define MID_lessequalq "less-equal?"
#define MID_lessq "less?"
#define MID_letid "let"
#define MID_libraryid "library"
#define MID_logand "logand"
#define MID_logor "logor"
#define MID_mapto "map-to"
#define MID_matchid "match"
#define MID_mod "mod"
#define MID_moduleid "module"
#define MID_multiply "multiply"
#define MID_namespaceid "namespace"
#define MID_nilid "nil"
#define MID_notid "not"
#define MID_nsid "ns"
#define MID_rem "rem"
#define MID_selectid "select"
#define MID_shiftleft "shift-left"
#define MID_shiftright "shift-right"
#define MID_subtract "subtract"
#define MID_thenid "then"
#define MID_unequalq "unequal?"
#define MID_whenid "when"
#define MID_whereid "where"
#define MID_whileid "while"
#define MID_withid "with"


//--------------------------------------------------------------------------
// names of known keywords
#define MID_exitKeyword "exit"
#define MID_signalKeyword "signal"


//--------------------------------------------------------------------------
// names of predefined types
#define MID_core_ns ".lang"

#define MID_clang_ns ".clang"

#define MID_AnyTypeName \
  MID_core_ns "."       \
              "Any"

#define MID_BoolTypeName \
  MID_core_ns "."        \
              "Bool"
#define MID_CharTypeName \
  MID_core_ns "."        \
              "Char"

#define MID_ObjectTypeName \
  MID_core_ns "."          \
              "Object"
#define MID_ClassTypeName \
  MID_core_ns "."         \
              "Type"

#define MID_NumberTypeName \
  MID_core_ns "."          \
              "Number"
#define MID_ComplexTypeName \
  MID_core_ns "."           \
              "Complex"
#define MID_RationalTypeName \
  MID_core_ns "."            \
              "Rational"
#define MID_IntegerTypeName \
  MID_core_ns "."           \
              "Integer"

#define MID_Float32TypeName \
  MID_core_ns "."           \
              "Float32"
#define MID_Float64TypeName \
  MID_core_ns "."           \
              "Float64"
#define MID_Float128TypeName \
  MID_core_ns "."            \
              "Float128"

#define MID_EofTypeName \
  MID_core_ns "."       \
              "Eof"
#define MID_NilTypeName \
  MID_core_ns "."       \
              "Nil"
#define MID_UnspecifiedTypeName \
  MID_core_ns "."               \
              "Unspecified"

#define MID_KeywordTypeName \
  MID_core_ns "."           \
              "Keyword"
#define MID_StringTypeName \
  MID_core_ns "."          \
              "String"

#define MID_Int8TypeName \
  MID_core_ns "."        \
              "Int8"
#define MID_UInt8TypeName \
  MID_core_ns "."         \
              "UInt8"
#define MID_Int16TypeName \
  MID_core_ns "."         \
              "Int16"
#define MID_UInt16TypeName \
  MID_core_ns "."          \
              "UInt16"
#define MID_Int32TypeName \
  MID_core_ns "."         \
              "Int32"
#define MID_UInt32TypeName \
  MID_core_ns "."          \
              "UInt32"
#define MID_Int64TypeName \
  MID_core_ns "."         \
              "Int64"
#define MID_UInt64TypeName \
  MID_core_ns "."          \
              "UInt64"

#define MID_SliceableTypeName \
  MID_core_ns "."             \
              "Sliceable"
#define MID_SliceableXTypeName \
  MID_core_ns "."              \
              "Sliceable!"

#define MID_OrderedSliceableTypeName \
  MID_core_ns "."                    \
              "OrderedSliceable"
#define MID_OrderedSliceableXTypeName \
  MID_core_ns "."                     \
              "OrderedSliceable!"

#define MID_RangeTypeName \
  MID_core_ns "."         \
              "Range"
#define MID_CollectionTypeName \
  MID_core_ns "."              \
              "Collection"
#define MID_OrderedCollectionTypeName \
  MID_core_ns "."                     \
              "OrderedCollection"
#define MID_SequenceTypeName \
  MID_core_ns "."            \
              "Sequence"
#define MID_VectorTypeName \
  MID_core_ns "."          \
              "Vector"
#define MID_AssocCollectionTypeName \
  MID_core_ns "."                   \
              "AssocCollection"
#define MID_AssocTypeName \
  MID_core_ns "."         \
              "Assoc"
#define MID_MapTypeName \
  MID_core_ns "."       \
              "Map"

#define MID_clang_IntTypeName \
  MID_clang_ns "."            \
               "int"
#define MID_clang_CharTypeName \
  MID_clang_ns "."             \
               "char"


//--------------------------------------------------------------------------
// names of predefined functions
#define MID_lang_return \
  MID_core_ns "."       \
              "return"
#define MID_lang_slice \
  MID_core_ns "."      \
              "slice"
#define MID_lang_sliceX \
  MID_core_ns "."       \
              "slice!"
#define MID_lang_unspecified \
  MID_core_ns "."            \
              "unspecified"
#define MID_lang_allocate \
  MID_core_ns "."         \
              "allocate"
#define MID_lang_allocate_array \
  MID_core_ns "."               \
              "allocate*"
#define MID_lang_nil \
  MID_core_ns "."    \
              "nil"

#define MID_lang_equal_q MID_core_ns "." MID_equalq
#define MID_lang_greater_q MID_core_ns "." MID_greaterq
#define MID_lang_less_q MID_core_ns "." MID_lessq
#define MID_lang_less_equal_q MID_core_ns "." MID_lessequalq
#define MID_lang_greater_equal_q MID_core_ns "." MID_greaterequalq
#define MID_lang_unequal_q MID_core_ns "." MID_unequalq
#define MID_lang_compare MID_core_ns "." MID_compare
#define MID_lang_concat MID_core_ns "." MID_concat

#define MID_app_main ".app.main"


class Names {
public:
  // predefined type names
  const static String kAnyTypeName;

  const static String kBoolTypeName;
  const static String kCharTypeName;

  const static String kObjectTypeName;
  const static String kClassTypeName;

  const static String kNumberTypeName;
  const static String kComplexTypeName;
  const static String kRationalTypeName;
  const static String kIntegerTypeName;

  const static String kFloat32TypeName;
  const static String kFloat64TypeName;
  const static String kFloat128TypeName;

  const static String kEofTypeName;
  const static String kNilTypeName;
  const static String kUnspecifiedTypeName;

  const static String kKeywordTypeName;
  const static String kStringTypeName;

  const static String kInt8TypeName;
  const static String kUInt8TypeName;
  const static String kInt16TypeName;
  const static String kUInt16TypeName;
  const static String kInt32TypeName;
  const static String kUInt32TypeName;
  const static String kInt64TypeName;
  const static String kUInt64TypeName;

  const static String kSliceableTypeName;
  const static String kSliceableXTypeName;
  const static String kOrderedSliceableTypeName;
  const static String kOrderedSliceableXTypeName;

  const static String kRangeTypeName;
  const static String kCollectionTypeName;
  const static String kOrderedCollectionTypeName;
  const static String kSequenceTypeName;
  const static String kVectorTypeName;
  const static String kAssocTypeName;
  const static String kAssocCollectionTypeName;
  const static String kMapTypeName;

  // predefined functions
  const static String kLangReturn;
  const static String kLangSlice;
  const static String kLangSliceX;
  const static String kLangUnspecified;
  const static String kLangAllocate;
  const static String kLangAllocateArray;
  const static String kLangNil;
  const static String kLangInitFunctor;
  const static String kLangEndp;
  const static String kLangNext;
  const static String kLangNumItems;

  const static String kLangEqualQ;
  const static String kLangUnequalQ;
  const static String kLangLessQ;
  const static String kLangLessEqualQ;
  const static String kLangGreaterQ;
  const static String kLangGreaterEqualQ;
  const static String kLangCompare;
  const static String kLangConcat;

  const static String kLangIsaQ;
  const static String kLangToChar;

  const static String kLangAdd;
  const static String kLangSubtract;
  const static String kLangMultiply;
  const static String kLangDivide;

  const static String kAppMain;


  const static String kClangIntTypeName;
  const static String kClangCharTypeName;
  const static String kClangAtomTypeName;

  // known keywords
  const static String kExitKeyword;
  const static String kSignalKeyword;

  // predefined function, variable, and keyarg names
  const static String kInitFuncName;
  const static String kValueKeyargName;
  const static String kNullValueFuncName;
  const static String kOnInitFuncName;
  const static String kOnCopyFuncName;
};

}  // namespace herschel
