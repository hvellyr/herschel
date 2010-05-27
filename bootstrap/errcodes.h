/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_errcodes_h
#define bootstrap_errcodes_h

namespace heather
{
  // don't change the numeric values without good reason; you have to
  // update the test descriptions otherwise
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

    E_SymbolExpected      = 0x1001,
    E_StringExpected      = 0x1002,
    E_UnexpectedTopExpr   = 0x1003,
    E_ConstExprExpected   = 0x1004,

    // notation errors
    E_UnexpectedChar      = 0x0100,
    E_BadHashNotation     = 0x0101,
    E_UnknownCharName     = 0x0102,
    E_ExpectedCharName    = 0x0103,
    E_UnterminatedChar    = 0x0104,
    E_UnterminatedString  = 0x0105,
    E_BadCharNotation     = 0x0106,
    E_BadNumberNotation   = 0x0107,

    // def parsing
    E_DefInitValueUnexpectedToken = 0x4001,
    E_DefNoInitValue      = 0x4002,
    E_LocalGenericFunc    = 0x4010,
    E_BadCharValue        = 0x4011,
    E_GlobalLet           = 0x4012,

    // typedef parsing
    E_MissingType         = 0x4013,
    E_LocalAliasDef       = 0x4020,
    E_LocalTypeDef        = 0x4021,
    E_CtorNotInTypes      = 0x4022,

    // functions
    E_MissingBody         = 0x4100,
    E_InvalidRestParam    = 0x4101,
    E_ParamOrder          = 0x4102,

    // expressions
    E_MissingRHExpr       = 0x6001,

    // module parsing
    E_MissingModName      = 0xa000,
    E_MissingDefName      = 0xa001,

    // import
    E_MapToExpected       = 0xa101,

    // literals
    E_InconsistentArgs    = 0xa201,

    // on
    E_UnknownOnKey        = 0xa301,

    // select, match
    E_BadPatternList      = 0xa401,
    E_RedefinedPattern    = 0xa402,
    E_ColonExpected       = 0xa403,

    // loops
    E_BadForLoopClause    = 0xa501,
    BadExplicitForClause  = 0xa502,
  };
};

#endif  // bootstrap_errcodes_h
