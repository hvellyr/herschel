/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include "stdarg.h"

#include "log.h"
#include "str.h"
#include "srcpos.h"


using namespace heather;

static char* levelStr[] = {
  "debug",
  "info",
  "warning",
  "error"
};


static void
logImpl(const SrcPos& where, LogLevel level, int errorCode, const char* msg)
{
  if (errorCode == 0)
    fprintf(stderr, "%s:%d: %s: %s\n",
            (const char*)StrHelper(where.file()),
            where.lineNumber(),
            levelStr[level],
            msg);
  else
    fprintf(stderr, "%s:%d: %s: (%04x) %s\n",
            (const char*)StrHelper(where.file()),
            where.lineNumber(),
            levelStr[level],
            errorCode,
            msg);
}


void
heather::log(const SrcPos& where, LogLevel level, const String& msg)
{
  logImpl(where, level, 0, (const char*)StrHelper(msg));
}


void
heather::logf(const SrcPos& where, LogLevel level, const char* format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, level, 0, buffer);
}


void
heather::error(const SrcPos& where, int errorCode, const String& msg)
{
  logImpl(where, kError, errorCode, (const char*)StrHelper(msg));
}


void
heather::errorf(const SrcPos& where, int errorCode, const char* format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, kError, errorCode, buffer);
}
