/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include "runtime/trace.h"

#if defined(UNITTESTS)
static void
traceImpl(const char* key, FILE* stream, const char* msg)
{
  if (key != NULL)
    fprintf(stream, "%s: %s\n", key, msg);
  else
    fprintf(stream, "-: %s\n", msg);
}
#endif


void
hr_trace(const char* key, const char* format, ...)
{
#if defined(UNITTESTS)
  static int checkedSetting = 0;
  static const char* runtimeDebugSetting = NULL;

  int shouldTrace = 0;

  if (!checkedSetting) {
    checkedSetting = 1;

    runtimeDebugSetting = getenv("HR_RUNTIME_DEBUG");
  }

  if (key == NULL)
    shouldTrace = 1;
  else if (runtimeDebugSetting != NULL)
    shouldTrace = (strstr(runtimeDebugSetting, key) != NULL);

  if (shouldTrace) {
    char buffer[2048];

    va_list args;
    va_start(args, format);
    vsnprintf(buffer, 2048, format, args);
    va_end(args);

    traceImpl(key, stderr, buffer);
  }
#endif
}
