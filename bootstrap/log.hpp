/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"
#include "srcpos.hpp"
#include "str.hpp"

#include <sstream>


namespace herschel {
enum LogLevel { kDebug, kInfo, kWarn, kError };


class LogSurpressor {
public:
  LogSurpressor();
  ~LogSurpressor();

private:
  bool fOldValue;
};

bool isSilent();


class LineLogger {
public:
  std::stringstream fMsgBuffer;

  LineLogger();
  LineLogger(LogLevel level, const SrcPos& where, int errorCode);
  LineLogger(LogLevel level, const SrcPos& where);
  LineLogger(LogLevel level);

  ~LineLogger();

  LineLogger& operator<<(const String& str)
  {
    fMsgBuffer << str.to_string();
    return *this;
  }

  LineLogger& operator<<(const char* str)
  {
    fMsgBuffer << str;
    return *this;
  }

  LineLogger& operator<<(const std::string& str)
  {
    fMsgBuffer << str;
    return *this;
  }

  LineLogger& operator<<(int val)
  {
    fMsgBuffer << val;
    return *this;
  }

  LineLogger& operator<<(bool val)
  {
    fMsgBuffer << (val ? "true" : "false");
    return *this;
  }
};


class Numf {
public:
  Numf(std::int64_t val, int base, int width = 0, char c = '\0')
      : fVal(val)
      , fWidth(width)
      , fBase(base)
      , fC(c)
  {
  }

  std::int64_t fVal;
  int fWidth;
  int fBase;
  char fC;
};


inline Numf hex(int val, int width = 0, char c = '\0')
{
  return Numf{val, 16, width, c};
}


inline Numf oct(int val, int width = 0, char c = '\0')
{
  return Numf{val, 8, width, c};
}


LineLogger& operator<<(LineLogger& ll, const Numf& numf);


template <typename T>
LineLogger& operator<<(LineLogger& ll, const T& t)
{
  ll.fMsgBuffer << t;
  return ll;
}


// clang-format off
#define HR_LOG(...)                             \
  LineLogger(__VA_ARGS__)
// clang-format on

}  // namespace herschel
