/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_strbuf_h
#define bootstrap_strbuf_h

#include <vector>

#include "common.h"

namespace herschel
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

    void setAtIndex(int atIndex, Char c);
    void setAtIndex(int atIndex, const String& other);

    StringBuffer& insertAt(int atIndex, Char c);
    StringBuffer& insertAt(int atIndex, const String& other);
    StringBuffer& insertAt(int atIndex, const char* utf8);

    String toString() const;

  private:
    std::vector<Char> fBuffer;
  };
};

#endif  // bootstrap_strbuf_h
