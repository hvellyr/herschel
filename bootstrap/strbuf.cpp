/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "strbuf.hpp"

#include "str.hpp"

#include <string.h>


using namespace herschel;

StringBuffer::StringBuffer()
{
  fBuffer.reserve(64);
}


StringBuffer::StringBuffer(const StringBuffer& other)
{
  fBuffer.reserve(64);
  *this << other;
  hr_assert(fBuffer.size() == other.fBuffer.size());
}


StringBuffer::StringBuffer(const String& other)
{
  fBuffer.reserve(64);
  *this << other;
  hr_assert(int(fBuffer.size()) == other.length());
}


StringBuffer::StringBuffer(zstring c)
{
  fBuffer.reserve(64);
  *this << c;
  hr_assert(fBuffer.size() == ::strlen(c));
}


int StringBuffer::length() const
{
  return fBuffer.size();
}


bool StringBuffer::isEmpty() const
{
  return fBuffer.empty();
}


StringBuffer& StringBuffer::operator<<(const StringBuffer& other)
{
  fBuffer.insert(fBuffer.end(), other.fBuffer.begin(), other.fBuffer.end());
  return *this;
}


StringBuffer& StringBuffer::operator<<(const String& other)
{
  size_t items = other.length();
  if (items > 0) {
    int endidx = fBuffer.size();
    fBuffer.insert(fBuffer.end(), items, ' ');
    ::memcpy(&fBuffer[endidx], other.data(), items * sizeof(Char));
  }
  return *this;
}


StringBuffer& StringBuffer::operator<<(zstring utf8)
{
  int utf8len = ::strlen(utf8);
  int reqlen = str_utf8_to_wcs(utf8, utf8len, nullptr, 0);

  int endidx = fBuffer.size();
  fBuffer.insert(fBuffer.end(), reqlen, ' ');

#if defined(IS_DEBUG)
  int reallen =
#endif
      str_utf8_to_wcs(utf8, utf8len, &fBuffer[endidx], reqlen);

  hr_assert(reallen == reqlen);
  return *this;
}


StringBuffer& StringBuffer::operator<<(Char c)
{
  fBuffer.push_back(c);
  return *this;
}


String StringBuffer::toString() const
{
  return String(&fBuffer[0], fBuffer.size());
}


Char StringBuffer::operator[](int atIndex) const
{
  hr_assert(atIndex >= 0 && atIndex < int(fBuffer.size()));
  return fBuffer[atIndex];
}


void StringBuffer::setAtIndex(int atIndex, Char c)
{
  hr_assert(atIndex >= 0 && atIndex < int(fBuffer.size()));
  fBuffer[atIndex] = c;
}


void StringBuffer::setAtIndex(int atIndex, const String& other)
{
  hr_assert(atIndex >= 0 && atIndex + other.length() < int(fBuffer.size()));
  for (int i = 0; i < other.length(); i++)
    fBuffer[atIndex + i] = other[i];
}


StringBuffer& StringBuffer::insertAt(int atIndex, Char c)
{
  fBuffer.insert(fBuffer.begin() + atIndex, c);
  return *this;
}

StringBuffer& StringBuffer::insertAt(int atIndex, const String& other)
{
  size_t items = other.length();
  if (items > 0) {
    fBuffer.insert(fBuffer.begin() + atIndex, items, ' ');
    ::memcpy(&fBuffer[atIndex], other.data(), items * sizeof(Char));
  }
  return *this;
}


StringBuffer& StringBuffer::insertAt(int atIndex, zstring utf8)
{
  int utf8len = ::strlen(utf8);
  int reqlen = str_utf8_to_wcs(utf8, utf8len, nullptr, 0);

  if (reqlen > 0) {
    fBuffer.insert(fBuffer.begin() + atIndex, reqlen, ' ');

#if defined(IS_DEBUG)
    int reallen =
#endif
        str_utf8_to_wcs(utf8, utf8len, &fBuffer[atIndex], reqlen);
    hr_assert(reallen == reqlen);
  }
  return *this;
}
