/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "log.hpp"

#include "srcpos.hpp"
#include "str.hpp"

#include <stdarg.h>
#include <stdio.h>


namespace herschel {


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


bool isSilent()
{
  return sBeSilent;
}


//------------------------------------------------------------------------------

static zstring levelStr[] = { "debug", "info", "warning", "error" };


static void logImpl(const SrcPos& where, LogLevel level, int errorCode, FILE* stream,
                    zstring msg)
{
  if (sBeSilent)
    return;

  if (where.isValid()) {
    if (errorCode == 0)
      fprintf(stream, "%s:%d: %s: %s\n", (zstring)StrHelper(where.file()),
              where.lineNumber(), levelStr[level], msg);
    else
      fprintf(stream, "%s:%d: %s: (%04x) %s\n", (zstring)StrHelper(where.file()),
              where.lineNumber(), levelStr[level], errorCode, msg);
  }
  else {
    if (errorCode == 0)
      fprintf(stream, "%s: %s\n", levelStr[level], msg);
    else
      fprintf(stream, "%s: (%04x) %s\n", levelStr[level], errorCode, msg);
  }
}


void log(const SrcPos& where, LogLevel level, const String& msg)
{
  logImpl(where, level, 0, stderr, (zstring)StrHelper(msg));
}


void logf(const SrcPos& where, LogLevel level, zstring format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, level, 0, stderr, buffer);
}


void log(LogLevel level, const String& msg)
{
  static SrcPos sp;
  logImpl(sp, level, 0, stderr, (zstring)StrHelper(msg));
}


void logf(LogLevel level, zstring format, ...)
{
  static SrcPos sp;
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(sp, level, 0, stderr, buffer);
}


void error(const SrcPos& where, int errorCode, const String& msg)
{
  logImpl(where, kError, errorCode, stderr, (zstring)StrHelper(msg));
}


void errorf(const SrcPos& where, int errorCode, zstring format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, kError, errorCode, stderr, buffer);
}


void warning(const SrcPos& where, int errorCode, const String& msg)
{
  logImpl(where, kWarn, errorCode, stderr, (zstring)StrHelper(msg));
}


void warningf(const SrcPos& where, int errorCode, zstring format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, kWarn, errorCode, stderr, buffer);
}

}  // namespace herschel
