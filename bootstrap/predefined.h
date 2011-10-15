/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_predefined_h
#define bootstrap_predefined_h

#include "str.h"

namespace herschel
{
  // predefined names and constants

  //--------------------------------------------------------------------------
  // definitions of the reserved keywords
#define MID_ById        "by"
#define MID_DefId       "def"
#define MID_ElseId      "else"
#define MID_EofId       "eof"
#define MID_ExportId    "export"
#define MID_ExtendId    "extend"
#define MID_ExternId    "extern"
#define MID_ForId       "for"
#define MID_FUNCTIONId  "Function"
#define MID_FunctionId  "function"
#define MID_IfId        "if"
#define MID_ImportId    "import"
#define MID_LetId       "let"
#define MID_MatchId     "match"
#define MID_ModuleId    "module"
#define MID_NilId       "nil"
#define MID_NotId       "not"
#define MID_OnId        "on"
#define MID_ReifyId     "reify"
#define MID_SelectId    "select"
#define MID_ThenId      "then"
#define MID_WhenId      "when"
#define MID_WhereId     "where"
#define MID_WhileId     "while"

#define MID_EqualQ      "equal?"
#define MID_GreaterQ    "greater?"
#define MID_LessQ       "less?"
#define MID_LessEqualQ  "less-equal?"
#define MID_GreaterEqualQ "greater-equal?"
#define MID_UnequalQ    "unequal?"

#define MID_add         "add"
#define MID_append      "append"
#define MID_bitand      "bitand"
#define MID_bitor       "bitor"
#define MID_bitxor      "bitxor"
#define MID_compare     "compare"
#define MID_divide      "divide"
#define MID_exponent    "exponent"
#define MID_fold        "fold"
#define MID_in          "in"
#define MID_isaQ        "isa?"
#define MID_logand      "logand"
#define MID_logor       "logor"
#define MID_mod         "mod"
#define MID_multiply    "multiply"
#define MID_rem         "rem"
#define MID_shiftLeft   "shift-left"
#define MID_shiftRight  "shift-right"
#define MID_subtract    "subtract"
#define MID_CastTo      "cast-to"
#define MID_MapTo       "map-to"


  //--------------------------------------------------------------------------
  // names of known keywords
#define MID_exitKeyword "exit"
#define MID_signalKeyword "signal"


  //--------------------------------------------------------------------------
  // names of predefined types
#define MID_core_ns             "lang"

#define MID_clang_ns            "clang"

#define MID_AnyTypeName         MID_core_ns "|" "Any"

#define MID_BoolTypeName        MID_core_ns "|" "Bool"
#define MID_CharTypeName        MID_core_ns "|" "Char"

#define MID_ObjectTypeName      MID_core_ns "|" "Object"
#define MID_ClassTypeName       MID_core_ns "|" "Class"

#define MID_NumberTypeName      MID_core_ns "|" "Number"
#define MID_ComplexTypeName     MID_core_ns "|" "Complex"
#define MID_RationalTypeName    MID_core_ns "|" "Rational"

#define MID_Float32TypeName     MID_core_ns "|" "Float32"
#define MID_Float64TypeName     MID_core_ns "|" "Float64"
#define MID_Float128TypeName    MID_core_ns "|" "Float128"

#define MID_EofTypeName         MID_core_ns "|" "Eof"
#define MID_NilTypeName         MID_core_ns "|" "Nil"
#define MID_UnspecifiedTypeName MID_core_ns "|" "Unspecified"

#define MID_KeywordTypeName     MID_core_ns "|" "Keyword"
#define MID_StringTypeName      MID_core_ns "|" "String"

#define MID_Int8TypeName        MID_core_ns "|" "Int8"
#define MID_UInt8TypeName       MID_core_ns "|" "UInt8"
#define MID_Int16TypeName       MID_core_ns "|" "Int16"
#define MID_UInt16TypeName      MID_core_ns "|" "UInt16"
#define MID_Int32TypeName       MID_core_ns "|" "Int32"
#define MID_UInt32TypeName      MID_core_ns "|" "UInt32"
#define MID_Int64TypeName       MID_core_ns "|" "Int64"
#define MID_UInt64TypeName      MID_core_ns "|" "UInt64"

#define MID_SliceableTypeName   MID_core_ns "|" "Sliceable"
#define MID_SliceableXTypeName  MID_core_ns "|" "Sliceable!"

#define MID_RangeTypeName       MID_core_ns "|" "Range"
#define MID_CollectionTypeName  MID_core_ns "|" "Collection"
#define MID_OrderedCollectionTypeName MID_core_ns "|" "OrderedCollection"
#define MID_SequenceTypeName    MID_core_ns "|" "Sequence"
#define MID_VectorTypeName      MID_core_ns "|" "Vector"
#define MID_AssocCollectionTypeName MID_core_ns "|" "AssocCollection"
#define MID_AssocTypeName       MID_core_ns "|" "Assoc"
#define MID_MapTypeName         MID_core_ns "|" "Map"

#define MID_clang_IntTypeName   MID_clang_ns "|" "int"
#define MID_clang_CharTypeName  MID_clang_ns "|" "char"


  //--------------------------------------------------------------------------
  // names of predefined functions
#define MID_lang_return         MID_core_ns "|" "return"
#define MID_lang_slice          MID_core_ns "|" "slice"
#define MID_lang_sliceX         MID_core_ns "|" "slice!"
#define MID_lang_unspecified    MID_core_ns "|" "unspecified"
#define MID_lang_allocate       MID_core_ns "|" "allocate"
#define MID_lang_allocate_array MID_core_ns "|" "allocate*"
#define MID_lang_nil            MID_core_ns "|" "nil"

#define MID_lang_equal_q         MID_core_ns "|" MID_EqualQ
#define MID_lang_greater_q       MID_core_ns "|" MID_GreaterQ
#define MID_lang_less_q          MID_core_ns "|" MID_LessQ
#define MID_lang_less_equal_q    MID_core_ns "|" MID_LessEqualQ
#define MID_lang_greater_equal_q MID_core_ns "|" MID_GreaterEqualQ
#define MID_lang_unequal_q       MID_core_ns "|" MID_UnequalQ


#define MID_app_main            "app|main"


  class Names
  {
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
    const static String kLangUnequalq;
    const static String kLangLessQ;
    const static String kLangLessEqualQ;
    const static String kLangGreaterQ;
    const static String kLangGreaterEqualQ;

    const static String kLangAdd;
    const static String kLangSubtract;

    const static String kAppMain;



    const static String kClangIntTypeName;
    const static String kClangCharTypeName;

    // known keywords
    const static String kExitKeyword;
    const static String kSignalKeyword;

    // predefined function, variable, and keyarg names
    const static String kInitFuncName;
    const static String kValueKeyargName;
  };
};                              // namespace

#endif                          // bootstrap_predefined_h
