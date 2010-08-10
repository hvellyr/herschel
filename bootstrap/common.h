/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_common_h
#define bootstrap_common_h

#include "config-local.h"
#include "version.h"
#include "sysconf.h"

typedef unsigned char Octet;
typedef unsigned short Char;


#define heaImplies(_condA, _condB)  (!(_condA) || ((_condA) && (_condB)))

#endif
