/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_srcpos_h
#define bootstrap_srcpos_h

#include "str.h"


namespace heather
{
  class SrcPos
  {
  public:
    SrcPos()
      : fLineNo(0)
    { }

    SrcPos(const String& file, int lineno)
      : fFile(file),
        fLineNo(lineno)
    { }

    SrcPos(const char* file, int lineno)
      : fFile(String(file)),
        fLineNo(lineno)
    { }

    SrcPos(const SrcPos& other)
    {
      *this = other;
    }

    SrcPos& operator=(const SrcPos& other)
    {
      fFile = other.fFile;
      fLineNo = other.fLineNo;
      return *this;
    }

    const String& file() const
    {
      return fFile;
    }

    int lineNumber() const
    {
      return fLineNo;
    }

    String toString() const;

  private:
    String fFile;
    int fLineNo;
  };
};


#endif  // bootstrap_srcpos_h
