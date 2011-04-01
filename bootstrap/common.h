/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_common_h
#define bootstrap_common_h

#include "config-local.h"
#include "version.h"
#include "sysconf.h"
#include "require.h"

//! Type abstraction for a byte.
typedef unsigned char Octet;

//! Type abstraction for a single unicode character.  Note that this type
//! requires UTF-16 encoding for strings.
typedef unsigned short Char;

//! Macro which evaluates _condA IMP _condB
#define implies(_condA, _condB)  (!(_condA) || ((_condA) && (_condB)))


#define K(_arg) true


enum CompileOutFormat
{
  kLLVM_IR,
  kLLVM_BC
};

#endif
