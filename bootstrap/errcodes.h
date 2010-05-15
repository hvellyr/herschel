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
    E_ParamMissBraceClose = 0x0002,
    E_UnexpectedToken     = 0x0003,

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

    // module parsing
    E_MissingModName      = 0xa000,
    E_MissingDefName      = 0xa001,
  };
};

#endif  // bootstrap_errcodes_h
