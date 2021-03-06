/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

namespace herschel
{
  //! Don't change the numeric values without good reason; you have to update
  //! the test descriptions otherwise.
  enum ErrCodes {
    // general punctuation error codes
    E_ParamMissParanClose = 0x0001,
    E_MissingBraceClose   = 0x0002,
    E_UnexpectedToken     = 0x0003,
    E_MissingParanOpen    = 0x0004,
    E_BadParameterList    = 0x0005,
    E_MissingParanClose   = 0x0006,
    E_MissingBracketClose = 0x0007,
    E_MissingGenericClose = 0x0008,
    E_GenericTypeList     = 0x0009,
    E_UnexpectedEOF       = 0x000a,
    E_MissingBraceOpen    = 0x000b,
    E_AssignExpected      = 0x000c,
    E_MapToExpected       = 0x000d,
    E_MissingApos         = 0x000e,
    E_UnexpectedQuote     = 0x000f,

    // notation errors
    E_UnexpectedChar      = 0x0100,
    E_BadHashNotation     = 0x0101,
    E_UnknownCharName     = 0x0102,
    E_ExpectedCharName    = 0x0103,
    E_UnterminatedChar    = 0x0104,
    E_UnterminatedString  = 0x0105,
    E_BadCharNotation     = 0x0106,
    E_BadNumberNotation   = 0x0107,
    E_UnitExpected        = 0x0108,
    E_UnknownSymbolDomain = 0x0109,
    E_UndefinedUnit       = 0x010a,
    E_OrphanedMultiValue  = 0x010b,
    E_OrphanedRestInd     = 0x010c,
    E_MisplacedThenWhile  = 0x010d,
    E_TypeHasNoBraces     = 0x010e,

    E_SymbolExpected      = 0x1001,
    E_StringExpected      = 0x1002,
    E_UnexpectedTopExpr   = 0x1003,
    E_ConstExprExpected   = 0x1004,
    E_UnexpectedDefExpr   = 0x1005,
    E_OnExprInType        = 0x1006,

    E_AmbiguousSym        = 0x2000,
    E_SymbolRedefined     = 0x2001,

    E_UnknownLinkage      = 0x2100,

    // def parsing
    E_Redefinition        = 0x4000,
    E_DefInitUnexpToken   = 0x4001,
    E_DefNoInitValue      = 0x4002,
    E_LocalGenericFunc    = 0x4010,
    E_BadCharValue        = 0x4011,
    E_GlobalLet           = 0x4012,

    // typedef parsing
    E_MissingType         = 0x4013,
    E_LocalTypeDef        = 0x4021,
    E_CtorNotInTypes      = 0x4022,
    E_NestedTypeDef       = 0x4023,
    E_SlotNotInClassDef   = 0x4024,
    E_UnexpDefInClass     = 0x4025,
    E_MissingUnitTag      = 0x4026,
    E_MissingBaseType     = 0x4027,
    E_MissingUnitSign     = 0x4028,
    E_UnknownSlotFlag     = 0x4029,
    E_SuperGenericType    = 0x4030,
    E_QualifiedLocalSym   = 0x4031,
    E_QualifiedParamKey   = 0x4032,
    E_QualifiedSlot       = 0x4033,
    E_QualifiedEnumDefSym = 0x4034,
    E_EnumNotBaseType     = 0x4035,
    E_EnumInitTypeMismatch= 0x4036,
    E_InvalidArraySize    = 0x4037,
    E_RefToFunc           = 0x4038,
    k_DoubleRefType       = 0x4039,
    E_EmptySeqType        = 0x403a,
    E_MixedValueType      = 0x403b,
    E_InheritsRefType     = 0x403c,
    E_InvariantType       = 0x403d,
    E_MultiDimenArray     = 0x403e,
    E_BadGenericType      = 0x403f,
    E_BadClassOnAlloc     = 0x4040,
    E_UnknownType         = 0x4041,
    E_SlotRefToNonClass   = 0x4042,
    E_UnknownSlot         = 0x4043,

    // enums
    E_BadEnumItemList     = 0x4050,

    // macro
    E_BadMacroPattern     = 0x4080,
    E_BadMacroReplcment   = 0x4081,
    E_MacroInconsistency  = 0x4082,
    E_InvalidPatternType  = 0x4083,
    E_PatternNameMismatch = 0x4084,
    E_MacroParamType      = 0x4085,
    E_MacroParamMismatch  = 0x4086,
    E_UnknownMacroParam   = 0x4087,
    E_OrphanedSangHash    = 0x4088,

    // functions
    E_MissingBody         = 0x4100,
    E_InvalidRestParam    = 0x4101,
    E_ParamOrder          = 0x4102,
    E_SpecNamedParam      = 0x4103,
    E_AbstractMethod      = 0x4104,
    E_GenericNoSpecPrm    = 0x4105,
    E_NoGenericFunction   = 0x4106,
    E_BadGenericReferrer  = 0x4107,
    E_BadFunctionArity    = 0x4108,
    E_CtorArgNameConflict = 0x4109,
    E_ArrayReqDefaultCtor = 0x410a,

    // loops
    E_BadForLoopClause    = 0x4200,
    BadExplicitForClause  = 0x4201,

    // literals
    E_InconsistentArgs    = 0x4250,

    // on
    E_UnknownOnKey        = 0x4301,
    E_UnreachableCode     = 0x4302,
    E_OrphanedOnExit      = 0x4303,

    // expressions
    E_MissingRHExpr       = 0x4400,
    E_BadLHExpr           = 0x4401,

    // select, match
    E_RedefinedPattern    = 0x4402,
    E_ColonExpected       = 0x4403,
    E_ExpectedPipe        = 0x4404,
    E_ElseNotLastPattern  = 0x4405,
    E_BadPatternList      = 0x4406,
    E_MissingExpr         = 0x4407,
    E_UnexpectedMapTo     = 0x4408,

    E_UndefinedVar        = 0x4410,
    E_BadType             = 0x4411,
    E_UndefinedType       = 0x4412,
    E_TypeMismatch        = 0x4413,

    E_BadArgNumber        = 0x4414,
    E_BoolTypeExpected    = 0x4415,
    E_IfConsqTypeMismatch = 0x4416,
    E_IfAltTypeMismatch   = 0x4417,
    E_GenericsMismatch    = 0x4418,
    E_RangeTypeMismatch   = 0x4419,
    E_WhileTypeMismatch   = 0x441a,
    E_BinaryTypeMismatch  = 0x441b,
    E_BadArgKind          = 0x441c,
    E_MatchAmbiguousType  = 0x441d,

    // module parsing
    E_MissingModName      = 0x4500,
    E_MissingDefName      = 0x4501,

    // import, export
    E_ExportVisibility    = 0x4540,
    E_EmptyExportList     = 0x4541,
    E_UnknownInputFile    = 0x4542,
    E_UnknownVisibility   = 0x4543,
    E_MissingRequiredFile = 0x4544,

    // external C syntax
    E_BadCSyntax          = 0xe000,
    E_UnexpLinkage        = 0xe001,
  };

} // namespace
