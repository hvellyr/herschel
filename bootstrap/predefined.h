/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_predefined_h
#define bootstrap_predefined_h

#include "str.h"

namespace heather
{
  // predefined names and constants

  //--------------------------------------------------------------------------
  // definitions of the reserved keywords
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


  //--------------------------------------------------------------------------
  // names of known keywords
#define MID_exitKeyword "exit"
#define MID_signalKeyword "signal"


  //--------------------------------------------------------------------------
  // names of predefined types

#define MID_AnyTypeName         "lang|Any"

#define MID_BoolTypeName        "lang|Bool"
#define MID_CharTypeName        "lang|Char"

#define MID_ObjectTypeName      "lang|Object"

#define MID_NumberTypeName      "lang|Number"
#define MID_ComplexTypeName     "lang|Complex"
#define MID_IntTypeName         "lang|Int"
#define MID_RationalTypeName    "lang|Rational"
#define MID_RealTypeName        "lang|Real"

#define MID_Float32TypeName     "lang|Float32"
#define MID_Float64TypeName     "lang|Float64"
#define MID_Float128TypeName    "lang|Float128"

#define MID_EofTypeName         "lang|Eof"
#define MID_NilTypeName         "lang|Nil"
#define MID_UnspecifiedTypeName "lang|Unspecified"

#define MID_KeywordTypeName     "lang|Keyword"
#define MID_StringTypeName      "lang|String"

#define MID_Int8TypeName        "lang|Int8"
#define MID_UInt8TypeName       "lang|UInt8"
#define MID_Int16TypeName       "lang|Int16"
#define MID_UInt16TypeName      "lang|UInt16"
#define MID_Int32TypeName       "lang|Int32"
#define MID_UInt32TypeName      "lang|UInt32"
#define MID_Int64TypeName       "lang|Int64"
#define MID_UInt64TypeName      "lang|UInt64"

#define MID_SliceableTypeName   "lang|Sliceable"
#define MID_SliceableXTypeName  "lang|Sliceable!"

#define MID_RangeTypeName       "lang|Range"


  //--------------------------------------------------------------------------
  // names of predefined functions
#define MID_lang_return         "lang|return"
#define MID_lang_slice          "lang|slice"
#define MID_lang_sliceX         "lang|slice!"

  class Names
  {
  public:
    // predefined type names
    const static String kAnyTypeName;

    const static String kBoolTypeName;
    const static String kCharTypeName;

    const static String kObjectTypeName;

    const static String kNumberTypeName;
    const static String kComplexTypeName;
    const static String kIntTypeName;
    const static String kRationalTypeName;
    const static String kRealTypeName;

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


    // predefined functions
    const static String kLangReturn;
    const static String kLangSlice;
    const static String kLangSliceX;


    // known keywords
    const static String kExitKeyword;
    const static String kSignalKeyword;
  };
};                              // namespace

#endif                          // bootstrap_predefined_h
