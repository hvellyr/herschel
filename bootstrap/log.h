/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_log_h
#define bootstrap_log_h

#include "str.h"
#include "srcpos.h"


namespace heather
{
  enum LogLevel
  {
    kDebug,
    kInfo,
    kWarn,
    kError
  };

  void log(const SrcPos& where, LogLevel level, const String& msg);
  void logf(const SrcPos& where, LogLevel level, const char* format, ...);
};

#endif  // bootstrap_log_h
