/*
   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#if !defined(build_sysconf_h)
#  define build_sysconf_h

/* ----------------------------------------------------------------------
   setup endianess flags
   ---------------------------------------------------------------------- */
#  if defined(ARCH_i386)

#    if !defined(LITTLE_ENDIAN)
#      define LITTLE_ENDIAN 1
#    endif
#    undef BIG_ENDIAN

#  elif defined(ARCH_x86_64)

#    if !defined(LITTLE_ENDIAN)
#      define LITTLE_ENDIAN 1
#    endif
#    undef BIG_ENDIAN

#  elif defined(ARCH_ppc)

#    if !defined(BIG_ENDIAN)
#      define BIG_ENDIAN 1
#    endif
#    undef LITTLE_ENDIAN

#  endif

#  if defined(OS_mac)

#    include "stdint.h"

#  elif defined(OS_linux)

#  elif defined(OS_win)

#    include <inttypes.h>

// typedef long long Int64;
// typedef unsigned __int64 UInt64;

#  else
#    error Unsupported OS
#  endif

#  undef __CHAR_UNSIGNED__


/* ----------------------------------------------------------------------
   define base types and type sizes.

   Once we support 64bit we need to extend this conditionally.
   ---------------------------------------------------------------------- */

#  if defined(OS_win)
#    ifdef _MSC_VER
#      pragma warning(push)
#      pragma warning(disable : 4018 4244 4047 4996 4333)
#    endif
#  endif

#endif
