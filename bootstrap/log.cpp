/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "log.hpp"

#include "srcpos.hpp"
#include "str.hpp"

#include <stdio.h>

#include <iomanip>
#include <string>


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

static zstring levelStr[] = {"debug", "info", "warning", "error"};


LineLogger::LineLogger(LogLevel level, const SrcPos& where, int errorCode)
{
  if (sBeSilent)
    return;

  if (where.isValid()) {
    if (errorCode == 0)
      fMsgBuffer << where.file().to_string() << ":" << where.lineNumber() << ": "
                 << levelStr[level] << ": ";
    else
      fMsgBuffer << where.file().to_string() << ":" << where.lineNumber() << ": "
                 << levelStr[level] << ": (" << std::setw(4) << std::right << std::hex
                 << std::setfill('0') << errorCode << ") " << std::dec;
  }
  else {
    if (errorCode == 0)
      fMsgBuffer << levelStr[level] << ": ";
    else
      fMsgBuffer << levelStr[level] << ": (" << std::setw(4) << std::right << std::hex
                 << std::setfill('0') << errorCode << ") " << std::dec;
  }
}


LineLogger::LineLogger()
    : LineLogger(kInfo, SrcPos(), 0)
{
}


LineLogger::LineLogger(LogLevel level, const SrcPos& where)
    : LineLogger(level, where, 0)
{
}


LineLogger::LineLogger(LogLevel level)
    : LineLogger(level, SrcPos(), 0)
{
}


LineLogger::~LineLogger()
{
  fprintf(stdout, "%s\n", fMsgBuffer.str().c_str());
  fMsgBuffer.str("");
}


LineLogger& operator<<(LineLogger& ll, const Numf& numf)
{
  if (numf.fWidth > 0)
    ll.fMsgBuffer << std::setw(numf.fWidth) << std::right;

  if (numf.fBase == 16)
    ll.fMsgBuffer << std::hex;
  else if (numf.fBase == 8)
    ll.fMsgBuffer << std::oct;

  if (numf.fC != '\0')
    ll.fMsgBuffer << std::setfill(numf.fC);

  ll.fMsgBuffer << numf.fVal;
  return ll;
}

}  // namespace herschel
