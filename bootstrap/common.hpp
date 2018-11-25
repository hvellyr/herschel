/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "config-local.h"
#include "sysconf.hpp"
#include "version.h"

//! Type abstraction for a byte.
using Octet = unsigned char;

//! Type abstraction for a single unicode character.  Note that this type
//! requires UTF-16 encoding for strings.
using Char = unsigned short;

//! Type abstraction for a utf8 encoded character string.
using zstring = const char*;

//! Macro which evaluates _condA IMP _condB
#define implies(_condA, _condB) (!(_condA) || ((_condA) && (_condB)))

//! Macro for annotation boolean values

//! Instead of writing <tt>a.setValue(true)</tt> use
//! <tt>a.setValue(K(isReference))</tt> to document what is meant with true.
#define K(_arg) true


//! Enumeration of possible output formats for the compiler
enum CompileOutFormat {
  kLLVM_IR,  //!< Use LLVM textual intermediate representation
  kLLVM_BC   //!< Use LLVM binary intermediate representation
};

#include "require.hpp"
