/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


#include "require.hpp"

#include "log.hpp"

#include <stdlib.h>


#if defined(IS_DEBUG)

namespace herschel {


static bool areRequiresFatal = true;


void setRequiresAreFatal(bool value)
{
  areRequiresFatal = value;
}


void requireHandler(zstring file, int line, zstring title, zstring msg, zstring expr)
{
  HR_LOG(kError) << file << ":" << line << ": " << title << ": " << msg
                 << (expr ? ": " : "") << (expr ? expr : "");

  if (areRequiresFatal)
    abort();
}

}  // namespace herschel

#endif  // if defined(IS_DEBUG)
