/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_log_h
#define bootstrap_log_h

#include "str.h"
#include "srcpos.h"


namespace herschel
{
  enum LogLevel
  {
    kDebug,
    kInfo,
    kWarn,
    kError
  };


  class LogSurpressor
  {
  public:
    LogSurpressor();
    ~LogSurpressor();

  private:
    bool fOldValue;
  };

  bool isSilent();

  void log(const SrcPos& where, LogLevel level, const String& msg);
  void logf(const SrcPos& where, LogLevel level, const char* format, ...);

  void log(LogLevel level, const String& msg);
  void logf(LogLevel level, const char* format, ...);

  void error(const SrcPos& where, int errorCode, const String& msg);
  void errorf(const SrcPos& where, int errorCode, const char* format, ...);

  void warning(const SrcPos& where, int errorCode, const String& msg);
  void warningf(const SrcPos& where, int errorCode, const char* format, ...);
};

#endif  // bootstrap_log_h
