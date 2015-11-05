/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

namespace herschel
{
//--------------------------------------------------------------------------

#if !defined(IS_DEBUG)

#define hr_assert(_expr)
#define hr_assert_msg(_expr, _msg)
#define hr_invalid(_msg)


#else  /* IS_DEBUG */

  void setRequiresAreFatal(bool value);
  void requireHandler(zstring file, int line, zstring title, zstring msg,
                      zstring expr);


#define hr_assert(_expr)                                                \
  do {                                                                  \
    if (!(_expr))                                                       \
      herschel::requireHandler(__FILE__, __LINE__, "Offending expr",    \
                               "", #_expr);                             \
  } while (0)

#define hr_assert_msg(_expr, _msg)                                      \
  do {                                                                  \
    if (!(_expr))                                                       \
      herschel::requireHandler(__FILE__, __LINE__, "Offending expr",    \
                               _msg, #_expr);                           \
  } while (0)


#define hr_invalid(_msg)                                                \
  herschel::requireHandler(__FILE__, __LINE__, "Invalid", (_msg), nullptr)

#endif /* end IS_DEBUG */

}                               // namespace
