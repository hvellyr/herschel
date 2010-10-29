/*
   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#if !defined(build_sysconf_h)
#define build_sysconf_h

/* ----------------------------------------------------------------------
   setup endianess flags
   ---------------------------------------------------------------------- */
#if defined(ARCH_i386)

#  if !defined(LITTLE_ENDIAN)
#    define LITTLE_ENDIAN 1
#  endif
#  undef BIG_ENDIAN

typedef long Int32;
typedef unsigned long UInt32;

#elif defined(ARCH_ppc)

#  if !defined(BIG_ENDIAN)
#    define BIG_ENDIAN 1
#  endif
#  undef LITTLE_ENDIAN

typedef long Int32;
typedef unsigned long UInt32;

#endif

#if defined(OS_mac)

#include "stdint.h"

typedef int64_t  Int64;
typedef uint64_t UInt64;

#elif defined(OS_linux)

typedef long long Int64;
typedef unsigned long long UInt64;

#elif defined(OS_win)

#include <inttypes.h>

// typedef long long Int64;
// typedef unsigned __int64 UInt64;

#else
#  error Unsupported OS
#endif

#undef __CHAR_UNSIGNED__


/* ----------------------------------------------------------------------
   define base types and type sizes.

   Once we support 64bit we need to extend this conditionally.
   ---------------------------------------------------------------------- */
#define SIZEOF_CHAR         1
#define SIZEOF_SHORT        2
#define SIZEOF_INT          4
#define SIZEOF_LONG         4
#define SIZEOF_LONG_LONG    8
#define SIZEOF_FLOAT        4
#define SIZEOF_DOUBLE       8
#define SIZEOF_LONG_DOUBLE  16
#define SIZEOF_VOID_P       4

#define HEA_INT_MIN (-2147483647 - 1)
#define HEA_INT_MAX 2147483647

#if defined(USE_LLONG)
#define HEA_LLONG_MIN (-0x7fffffffffffffffLL-1)
#define HEA_LLONG_MAX 0x7fffffffffffffffLL
#endif


#if defined(OS_linux)
#  define HAVE_STRTOLL        1
#  define HAVE_ISATTY         1
#  define HAVE_LIMITS_H       1
#  define HAVE_UNISTD_H       1
#  define HAVE_LIMITS_H       1
#  define HAVE_DIRENT_DTYPE   1
#  define HAVE_STAT_ISSOCK    1
#  define HAVE_TYPES_H        1
#endif


#if defined(OS_mac)
#  define HAVE_STRTOLL        1
#  define HAVE_ISATTY         1
#  define HAVE_READLINE       1
#  define HAVE_LIMITS_H       1
#  define HAVE_UNISTD_H       1
#  define HAVE_DIRENT_DTYPE   1
#  define HAVE_STAT_ISSOCK    1
#  define HAVE_TYPES_H        1
#  define HAVE_ICONV          1
#  define ICONV_CONST
#  define HAVE_MEMORY_H       1
#  define HAVE_MEMSET         1
#  define HAVE_LSTAT          1
#  define HAVE_STDBOOL_H      1
#  define HAVE_STDINT_H       1
#  define HAVE_STDLIB_H       1
#  define HAVE_STRCHR         1
#  define HAVE_STRERROR       1
#  define HAVE_STRERROR_R     1
#  define HAVE_STRINGS_H      1
#  define HAVE_STRING_H       1
#  define HAVE_STRTOL         1
#  define HAVE_MMAP           1
#  define HAVE_MUNMAP         1
#  define HAVE_SYS_STAT_H     1
#  define HAVE_SYS_TYPES_H    1
#  define HAVE_TIMEGM         1
#  define HAVE_UNISTD_H       1
#  define HAVE_SYS_MMAN_H     1
#  define HAVE__BOOL          1
#  define HAVE_GXXCLASSVISIBILITY 1
#  define STDC_HEADERS        1
#endif


#if defined(OS_win)
#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning(disable: 4018 4244 4047 4996 4333)
#endif
#  undef HAVE_UNISTD_H
#  undef HAVE_STD_WSTRING
#endif

#endif
