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
#include "unittests.h"


using namespace heather;

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


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class LogSurpressorUnitTest : public UnitTest
{
public:
  LogSurpressorUnitTest() : UnitTest("LogSurpressor") {}

  virtual void run()
  {
    assert(!sBeSilent);

    {
      LogSurpressor beSilent;
      assert(sBeSilent);

      {
        LogSurpressor beSilent2;
        assert(sBeSilent);
      }
      assert(sBeSilent);
    }

    assert(!sBeSilent);
  }
};

static LogSurpressorUnitTest logSurpressorUnitTest;

#endif  // #if defined(UNITTESTS)


//------------------------------------------------------------------------------

static char* levelStr[] = {
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
heather::log(const SrcPos& where, LogLevel level, const String& msg)
{
  logImpl(where, level, 0, stderr, (const char*)StrHelper(msg));
}


void
heather::logf(const SrcPos& where, LogLevel level, const char* format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, level, 0, stderr, buffer);
}


void
heather::log(LogLevel level, const String& msg)
{
  static SrcPos sp;
  logImpl(sp, level, 0, stderr, (const char*)StrHelper(msg));
}


void
heather::logf(LogLevel level, const char* format, ...)
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
heather::error(const SrcPos& where, int errorCode, const String& msg)
{
  logImpl(where, kError, errorCode, stderr, (const char*)StrHelper(msg));
}


void
heather::errorf(const SrcPos& where, int errorCode, const char* format, ...)
{
  char buffer[2048];

  va_list args;
  va_start(args, format);
  vsnprintf(buffer, 2048, format, args);
  va_end(args);

  logImpl(where, kError, errorCode, stderr, buffer);
}
