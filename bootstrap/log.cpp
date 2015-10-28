/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "common.h"

#include <stdarg.h>
#include <stdio.h>

#include "log.h"
#include "str.h"
#include "srcpos.h"


using namespace herschel;

//------------------------------------------------------------------------------

static bool sBeSilent = false;

LogSurpressor::LogSurpressor()
  : fOldValue(sBeSilent)
{
  sBeSilent = true;
}


LogSurpressor::~LogSurpressor()
{
  sBeSilent = fOldValue;
}


bool herschel::isSilent()
{
  return sBeSilent;
}


//------------------------------------------------------------------------------

static const char* levelStr[] = {
  "debug",
  "info",
  "warning",
  "error"
};


static void
logImpl(const SrcPos& where, LogLevel level, int errorCode, FILE* stream,
        const char* msg)
{
  if (sBeSilent)
    return;

  if (where.isValid()) {
    if (errorCode == 0)
      fprintf(stream, "%s:%d: %s: %s\n",
              (const char*)StrHelper(where.file()),
              where.lineNumber(),
              levelStr[level],
              msg);
    else
      fprintf(stream, "%s:%d: %s: (%04x) %s\n",
              (const char*)StrHelper(where.file()),
              where.lineNumber(),
              levelStr[level],
              errorCode,
              msg);
  }
  else {
    if (errorCode == 0)
      fprintf(stream, "%s: %s\n",
              levelStr[level],
              msg);
    else
      fprintf(stream, "%s: (%04x) %s\n",
              levelStr[level],
              errorCode,
              msg);
  }
}


void
herschel::log(const SrcPos& where, LogLevel level, const String& msg)
{
  logImpl(where, level, 0, stderr, (const char*)StrHelper(msg));
}


void
herschel::logf(const SrcPos& where, LogLevel level, const char* format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, level, 0, stderr, buffer);
}


void
herschel::log(LogLevel level, const String& msg)
{
  static SrcPos sp;
  logImpl(sp, level, 0, stderr, (const char*)StrHelper(msg));
}


void
herschel::logf(LogLevel level, const char* format, ...)
{
  static SrcPos sp;
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(sp, level, 0, stderr, buffer);
}


void
herschel::error(const SrcPos& where, int errorCode, const String& msg)
{
  logImpl(where, kError, errorCode, stderr, (const char*)StrHelper(msg));
}


void
herschel::errorf(const SrcPos& where, int errorCode, const char* format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, kError, errorCode, stderr, buffer);
}


void
herschel::warning(const SrcPos& where, int errorCode, const String& msg)
{
  logImpl(where, kWarn, errorCode, stderr, (const char*)StrHelper(msg));
}


void
herschel::warningf(const SrcPos& where, int errorCode, const char* format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, kWarn, errorCode, stderr, buffer);
}
