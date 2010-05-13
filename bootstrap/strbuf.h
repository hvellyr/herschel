/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_strbuf_h
#define bootstrap_strbuf_h

#include <vector>

#include "common.h"

namespace heather
{
  class String;

  class StringBuffer
  {
  public:
    StringBuffer();
    StringBuffer(const StringBuffer& other);
    StringBuffer(const String& other);
    StringBuffer(const char* other);

    int length() const;
    bool isEmpty() const;

    StringBuffer& operator<<(const StringBuffer& other);
    StringBuffer& operator<<(const String& other);
    StringBuffer& operator<<(const char* other);
    StringBuffer& operator<<(Char c);

    Char operator[] (int atIndex) const;

    String toString() const;

  private:
    std::vector<Char> fBuffer;
  };
};

#endif  // bootstrap_strbuf_h