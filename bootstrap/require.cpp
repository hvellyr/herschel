/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


#include "require.hpp"

#include "log.hpp"

#include <stdlib.h>


using namespace herschel;


#if defined(IS_DEBUG)

static bool areRequiresFatal = true;


void setRequiresAreFatal(bool value)
{
  areRequiresFatal = value;
}


void herschel::requireHandler(zstring file, int line, zstring title, zstring msg,
                              zstring expr)
{
  logf(kError, "%s:%d: %s: %s%s%s\n", file, line, title, msg, expr ? ": " : "",
       expr ? expr : "");

  if (areRequiresFatal)
    abort();
}

#endif  // if defined(IS_DEBUG)
