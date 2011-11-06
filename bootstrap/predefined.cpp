/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "predefined.h"


//----------------------------------------------------------------------------

using namespace herschel;


const String herschel::Names::kAnyTypeName         = String(MID_AnyTypeName);

const String herschel::Names::kBoolTypeName        = String(MID_BoolTypeName);
const String herschel::Names::kCharTypeName        = String(MID_CharTypeName);

const String herschel::Names::kObjectTypeName      = String(MID_ObjectTypeName);
const String herschel::Names::kClassTypeName       = String(MID_ClassTypeName);

const String herschel::Names::kNumberTypeName      = String(MID_NumberTypeName);
const String herschel::Names::kComplexTypeName     = String(MID_ComplexTypeName);
const String herschel::Names::kRationalTypeName    = String(MID_RationalTypeName);
const String herschel::Names::kIntegerTypeName     = String(MID_IntegerTypeName);

const String herschel::Names::kFloat32TypeName     = String(MID_Float32TypeName);
const String herschel::Names::kFloat64TypeName     = String(MID_Float64TypeName);
const String herschel::Names::kFloat128TypeName    = String(MID_Float128TypeName);

const String herschel::Names::kEofTypeName         = String(MID_EofTypeName);
const String herschel::Names::kNilTypeName         = String(MID_NilTypeName);
const String herschel::Names::kUnspecifiedTypeName = String(MID_UnspecifiedTypeName);

const String herschel::Names::kKeywordTypeName     = String(MID_KeywordTypeName);
const String herschel::Names::kStringTypeName      = String(MID_StringTypeName);

const String herschel::Names::kInt8TypeName        = String(MID_Int8TypeName);
const String herschel::Names::kUInt8TypeName       = String(MID_UInt8TypeName);
const String herschel::Names::kInt16TypeName       = String(MID_Int16TypeName);
const String herschel::Names::kUInt16TypeName      = String(MID_UInt16TypeName);
const String herschel::Names::kInt32TypeName       = String(MID_Int32TypeName);
const String herschel::Names::kUInt32TypeName      = String(MID_UInt32TypeName);
const String herschel::Names::kInt64TypeName       = String(MID_Int64TypeName);
const String herschel::Names::kUInt64TypeName      = String(MID_UInt64TypeName);

const String herschel::Names::kSliceableTypeName   = String(MID_SliceableTypeName);
const String herschel::Names::kSliceableXTypeName  = String(MID_SliceableXTypeName);
const String herschel::Names::kRangeTypeName       = String(MID_RangeTypeName);
const String herschel::Names::kCollectionTypeName  = String(MID_CollectionTypeName);
const String herschel::Names::kOrderedCollectionTypeName = String(MID_OrderedCollectionTypeName);
const String herschel::Names::kSequenceTypeName    = String(MID_SequenceTypeName);
const String herschel::Names::kVectorTypeName      = String(MID_VectorTypeName);
const String herschel::Names::kAssocTypeName       = String(MID_AssocTypeName);
const String herschel::Names::kAssocCollectionTypeName = String(MID_AssocCollectionTypeName);
const String herschel::Names::kMapTypeName         = String(MID_MapTypeName);

const String herschel::Names::kLangReturn          = String(MID_lang_return);
const String herschel::Names::kLangSlice           = String(MID_lang_slice);
const String herschel::Names::kLangSliceX          = String(MID_lang_sliceX);
const String herschel::Names::kLangUnspecified     = String(MID_lang_unspecified);
const String herschel::Names::kLangAllocate        = String(MID_lang_allocate);
const String herschel::Names::kLangAllocateArray   = String(MID_lang_allocate_array);
const String herschel::Names::kLangNil             = String(MID_lang_nil);
const String herschel::Names::kLangInitFunctor     = String(MID_core_ns "|" "init-functor");
const String herschel::Names::kLangEndp            = String(MID_core_ns "|" "end?");
const String herschel::Names::kLangNext            = String(MID_core_ns "|" "next");
const String herschel::Names::kLangNumItems        = String(MID_core_ns "|" "num-items");

const String herschel::Names::kLangEqualQ          = String(MID_lang_equal_q);
const String herschel::Names::kLangUnequalq        = String(MID_lang_unequal_q);
const String herschel::Names::kLangLessQ           = String(MID_lang_less_q);
const String herschel::Names::kLangLessEqualQ      = String(MID_lang_equal_q);
const String herschel::Names::kLangGreaterQ        = String(MID_lang_greater_q);
const String herschel::Names::kLangGreaterEqualQ   = String(MID_lang_greater_equal_q);

const String herschel::Names::kLangIsaQ            = String(MID_core_ns "|" MID_isaQ);

const String herschel::Names::kLangAdd             = String(MID_core_ns "|" MID_add);
const String herschel::Names::kLangSubtract        = String(MID_core_ns "|" MID_subtract);
const String herschel::Names::kLangMultiply        = String(MID_core_ns "|" MID_multiply);
const String herschel::Names::kLangDivide          = String(MID_core_ns "|" MID_divide);

const String herschel::Names::kAppMain             = String(MID_app_main);

const String herschel::Names::kClangIntTypeName    = String(MID_clang_IntTypeName);
const String herschel::Names::kClangCharTypeName   = String(MID_clang_CharTypeName);
const String herschel::Names::kClangAtomTypeName   = String(MID_clang_ns "|ATOM");

const String herschel::Names::kExitKeyword         = String(MID_exitKeyword);
const String herschel::Names::kSignalKeyword       = String(MID_signalKeyword);


const String herschel::Names::kInitFuncName        = String("init");
const String herschel::Names::kValueKeyargName     = String("value");


