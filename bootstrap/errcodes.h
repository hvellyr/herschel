/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_errcodes_h
#define bootstrap_errcodes_h

namespace heather
{
  enum ErrCodes {
    // general punctuation error codes
    E_ParamMissParanClose = 0x0001,
    E_ParamMissBraceClose = 0x0002,
    E_UnexpectedToken     = 0x0003,

    // notation errors
    E_BadHashNotation     = 0x0101,

    // def parsing
    E_DefInitValueUnexpectedToken = 0x4001,

    // module parsing
    E_MissingModName      = 0xa000,
    E_MissingDefName      = 0xa001,
  };
};

#endif  // bootstrap_errcodes_h
