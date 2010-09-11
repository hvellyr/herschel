/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "strbuf.h"
#include "str.h"


using namespace heather;

StringBuffer::StringBuffer()
{
  fBuffer.reserve(64);
}


StringBuffer::StringBuffer(const StringBuffer& other)
{
  fBuffer.reserve(64);
  *this << other;
  assert(fBuffer.size() == other.fBuffer.size());
}


StringBuffer::StringBuffer(const String& other)
{
  fBuffer.reserve(64);
  *this << other;
  assert(int(fBuffer.size()) == other.length());
}


StringBuffer::StringBuffer(const char* c)
{
  fBuffer.reserve(64);
  *this << c;
  assert(fBuffer.size() == ::strlen(c));
}


int
StringBuffer::length() const
{
  return fBuffer.size();
}


bool
StringBuffer::isEmpty() const
{
  return fBuffer.empty();
}


StringBuffer&
StringBuffer::operator<<(const StringBuffer& other)
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


StringBuffer& StringBuffer::operator<<(const char* utf8)
{
  int utf8len = ::strlen(utf8);
  int reqlen = str_utf8_to_wcs(utf8, utf8len, NULL, 0);

  int endidx = fBuffer.size();
  fBuffer.insert(fBuffer.end(), reqlen, ' ');

  int reallen = str_utf8_to_wcs(utf8, utf8len,
                                &fBuffer[endidx], reqlen);
  assert(reallen == reqlen);
  return *this;
}


StringBuffer& StringBuffer::operator<<(Char c)
{
  fBuffer.push_back(c);
  return *this;
}


String
StringBuffer::toString() const
{
  return String(&fBuffer[0], fBuffer.size());
}


Char
StringBuffer::operator[] (int atIndex) const
{
  assert(atIndex >= 0 && atIndex < int(fBuffer.size()));
  return fBuffer[atIndex];
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>


SUITE(StringBuffer)
{
  TEST(Basic)
  {
    StringBuffer buf("hello world");
    CHECK_EQUAL(buf.toString(), String("hello world"));
    CHECK_EQUAL(buf.length(), 11);
    CHECK(!buf.isEmpty());
  }

  TEST(Compare)
  {
    StringBuffer buf2(String("hello world"));
    CHECK_EQUAL(buf2.toString(), String("hello world"));

    StringBuffer buf3(buf2);
    CHECK_EQUAL(buf3.toString(), String("hello world"));
  }

  TEST(AppendStr)
  {
    StringBuffer buf4;
    buf4 << "hello world";
    CHECK_EQUAL(buf4.toString(), String("hello world"));
  }

  TEST(AppendChar)
  {
    StringBuffer buf5;
    buf5 << 'h';
    buf5 << 'e';
    buf5 << 'l' << 'l' << "o w" << String("orl") << Char('d');
    CHECK_EQUAL(buf5.toString(), String("hello world"));
  }

  TEST(AppendChar2)
  {
    StringBuffer buf6;
    for (int i = 0; i < 2048; i++) {
      Char c = Char((i % 65) + 32);
      buf6 << c;
    }

    String s = buf6.toString();
    CHECK_EQUAL(s.length(), 2048);
    CHECK_EQUAL(buf6.length(), 2048);

    for (int i = 0; i < 2048; i++) {
      Char c = Char((i % 65) + 32);
      CHECK_EQUAL(s[i], c);
      CHECK_EQUAL(buf6[i], c);
    }
  }

  TEST(EmptyBuf)
  {
    StringBuffer buf7;
    CHECK_EQUAL(buf7.length(), 0);
    CHECK(buf7.isEmpty());
  }


  TEST(EmptyBuf2)
  {
    StringBuffer buf8("");
    CHECK_EQUAL(buf8.length(), 0);
    CHECK(buf8.isEmpty());
  }


  TEST(EmptyBuf3)
  {
    StringBuffer buf9;
    buf9 << "" << String("");
    CHECK_EQUAL(buf9.length(), 0);
    CHECK(buf9.isEmpty());
  }
}

#endif  // #if defined(UNITTESTS)
