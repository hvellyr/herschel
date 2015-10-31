/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_common_h
#define bootstrap_common_h

#include "config-local.h"
#include "version.h"
#include "sysconf.h"
#include "require.h"

//! Type abstraction for a byte.
using Octet = unsigned char;

//! Type abstraction for a single unicode character.  Note that this type
//! requires UTF-16 encoding for strings.
using Char = unsigned short;

//! Macro which evaluates _condA IMP _condB
#define implies(_condA, _condB)  (!(_condA) || ((_condA) && (_condB)))

//! Macro for annotation boolean values

//! Instead of writing <tt>a.setValue(true)</tt> use
//! <tt>a.setValue(K(isReference))</tt> to document what is meant with true.
#define K(_arg) true


//! Enumeration of possible output formats for the compiler
enum CompileOutFormat
{
  kLLVM_IR,                     //!< Use LLVM textual intermediate representation
  kLLVM_BC                      //!< Use LLVM binary intermediate representation
};

#endif
