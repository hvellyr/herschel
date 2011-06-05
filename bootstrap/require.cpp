/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


//----------------------------------------------------------------------------

#include "common.h"
#include "require.h"
#include "log.h"

#include <stdlib.h>

using namespace herschel;


#if defined(IS_DEBUG)

static bool areRequiresFatal = true;


void
setRequiresAreFatal(bool value)
{
  areRequiresFatal = value;
}


void
herschel::requireHandler(const char* file, int line,
                         const char* title, const char* msg,
                         const char* expr)
{
  logf(kError, "%s:%d: %s: %s%s%s\n",
       file, line, title, msg,
       expr ? ": " : "", expr ? expr : "");

  if (areRequiresFatal)
    abort();
}

#endif
