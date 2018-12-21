/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "predefined.hpp"

#include "str.hpp"


namespace herschel {


const String Names::kAnyTypeName = String(MID_AnyTypeName);

const String Names::kBoolTypeName = String(MID_BoolTypeName);
const String Names::kCharTypeName = String(MID_CharTypeName);

const String Names::kObjectTypeName = String(MID_ObjectTypeName);
const String Names::kClassTypeName = String(MID_ClassTypeName);

const String Names::kNumberTypeName = String(MID_NumberTypeName);
const String Names::kComplexTypeName = String(MID_ComplexTypeName);
const String Names::kRationalTypeName = String(MID_RationalTypeName);
const String Names::kIntegerTypeName = String(MID_IntegerTypeName);

const String Names::kFloat32TypeName = String(MID_Float32TypeName);
const String Names::kFloat64TypeName = String(MID_Float64TypeName);
const String Names::kFloat128TypeName = String(MID_Float128TypeName);

const String Names::kEofTypeName = String(MID_EofTypeName);
const String Names::kNilTypeName = String(MID_NilTypeName);
const String Names::kUnspecifiedTypeName = String(MID_UnspecifiedTypeName);

const String Names::kKeywordTypeName = String(MID_KeywordTypeName);
const String Names::kStringTypeName = String(MID_StringTypeName);

const String Names::kInt8TypeName = String(MID_Int8TypeName);
const String Names::kUInt8TypeName = String(MID_UInt8TypeName);
const String Names::kInt16TypeName = String(MID_Int16TypeName);
const String Names::kUInt16TypeName = String(MID_UInt16TypeName);
const String Names::kInt32TypeName = String(MID_Int32TypeName);
const String Names::kUInt32TypeName = String(MID_UInt32TypeName);
const String Names::kInt64TypeName = String(MID_Int64TypeName);
const String Names::kUInt64TypeName = String(MID_UInt64TypeName);

const String Names::kSliceableTypeName = String(MID_SliceableTypeName);
const String Names::kSliceableXTypeName = String(MID_SliceableXTypeName);
const String Names::kRangeTypeName = String(MID_RangeTypeName);
const String Names::kCollectionTypeName = String(MID_CollectionTypeName);
const String Names::kOrderedCollectionTypeName = String(MID_OrderedCollectionTypeName);
const String Names::kSequenceTypeName = String(MID_SequenceTypeName);
const String Names::kVectorTypeName = String(MID_VectorTypeName);
const String Names::kAssocTypeName = String(MID_AssocTypeName);
const String Names::kAssocCollectionTypeName = String(MID_AssocCollectionTypeName);
const String Names::kMapTypeName = String(MID_MapTypeName);

const String Names::kLangReturn = String(MID_lang_return);
const String Names::kLangSlice = String(MID_lang_slice);
const String Names::kLangSliceX = String(MID_lang_sliceX);
const String Names::kLangUnspecified = String(MID_lang_unspecified);
const String Names::kLangAllocate = String(MID_lang_allocate);
const String Names::kLangAllocateArray = String(MID_lang_allocate_array);
const String Names::kLangNil = String(MID_lang_nil);
const String Names::kLangInitFunctor = String(MID_core_ns "."
                                                          "init-functor");
const String Names::kLangEndp = String(MID_core_ns "."
                                                   "end?");
const String Names::kLangNext = String(MID_core_ns "."
                                                   "next");
const String Names::kLangNumItems = String(MID_core_ns "."
                                                       "num-items");

const String Names::kLangEqualQ = String(MID_lang_equal_q);
const String Names::kLangUnequalQ = String(MID_lang_unequal_q);
const String Names::kLangLessQ = String(MID_lang_less_q);
const String Names::kLangLessEqualQ = String(MID_lang_equal_q);
const String Names::kLangGreaterQ = String(MID_lang_greater_q);
const String Names::kLangGreaterEqualQ = String(MID_lang_greater_equal_q);
const String Names::kLangCompare = String(MID_lang_compare);
const String Names::kLangConcat = String(MID_lang_concat);

const String Names::kLangIsaQ = String(MID_core_ns "." MID_isaQ);
const String Names::kLangToChar = String(MID_core_ns ".to-char");

const String Names::kLangAdd = String(MID_core_ns "." MID_add);
const String Names::kLangSubtract = String(MID_core_ns "." MID_subtract);
const String Names::kLangMultiply = String(MID_core_ns "." MID_multiply);
const String Names::kLangDivide = String(MID_core_ns "." MID_divide);

const String Names::kAppMain = String(MID_app_main);

const String Names::kClangIntTypeName = String(MID_clang_IntTypeName);
const String Names::kClangCharTypeName = String(MID_clang_CharTypeName);
const String Names::kClangAtomTypeName = String(MID_clang_ns ".ATOM");

const String Names::kExitKeyword = String(MID_exitKeyword);
const String Names::kSignalKeyword = String(MID_signalKeyword);


const String Names::kInitFuncName = String("init");
const String Names::kValueKeyargName = String("value");

const String Names::kOnAllocFuncName = String("on-alloc");

}  // namespace herschel
