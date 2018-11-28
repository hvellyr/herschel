/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../str.hpp"
#include "../strbuf.hpp"

using namespace herschel;


TEST_CASE("StringBuffer basic", "[string][string-buffer]")
{
  StringBuffer buf("hello world");
  REQUIRE(buf.toString() == String("hello world"));
  REQUIRE(buf.length() == 11);
  REQUIRE(!buf.isEmpty());
}


TEST_CASE("StringBuffer compare", "[string][string-buffer]")
{
  StringBuffer buf2(String("hello world"));
  REQUIRE(buf2.toString() == String("hello world"));

  StringBuffer buf3(buf2);
  REQUIRE(buf3.toString() == String("hello world"));
}


TEST_CASE("StringBuffer append string", "[string][string-buffer]")
{
  StringBuffer buf4;
  buf4 << "hello world";
  REQUIRE(buf4.toString() == String("hello world"));
}


TEST_CASE("StringBuffer append character", "[string][string-buffer]")
{
  StringBuffer buf5;
  buf5 << 'h';
  buf5 << 'e';
  buf5 << 'l' << 'l' << "o w" << String("orl") << Char('d');
  REQUIRE(buf5.toString() == String("hello world"));
}


TEST_CASE("StringBuffer append character (large)", "[string][string-buffer]")
{
  StringBuffer buf6;
  for (int i = 0; i < 2048; i++) {
    Char c = Char((i % 65) + 32);
    buf6 << c;
  }

  String s = buf6.toString();
  REQUIRE(s.length() == 2048);
  REQUIRE(buf6.length() == 2048);

  for (int i = 0; i < 2048; i++) {
    Char c = Char((i % 65) + 32);
    REQUIRE(s[i] == c);
    REQUIRE(buf6[i] == c);
  }
}


TEST_CASE("StringBuffer empty buffer", "[string][string-buffer]")
{
  SECTION("Default constructor")
  {
    StringBuffer buf7;
    REQUIRE(buf7.length() == 0);
    REQUIRE(buf7.isEmpty());
  }

  SECTION("Constructed with empty string")
  {
    StringBuffer buf8("");
    REQUIRE(buf8.length() == 0);
    REQUIRE(buf8.isEmpty());
  }

  SECTION("Empty after adding empty string")
  {
    StringBuffer buf9;
    buf9 << "" << String("");
    REQUIRE(buf9.length() == 0);
    REQUIRE(buf9.isEmpty());
  }
}


TEST_CASE("StringBuffer set at index", "[string][string-buffer]")
{
  StringBuffer buf;
  buf << "__QN..core/2fcc";
  buf.setAtIndex(4, String("09"));
  REQUIRE(String("__QN09core/2fcc") == buf.toString());

  buf.setAtIndex(14, Char('x'));
  REQUIRE(String("__QN09core/2fcx") == buf.toString());
}


TEST_CASE("StringBuffer insert at", "[string][string-buffer]")
{
  SECTION("Insert const char*")
  {
    StringBuffer buf;
    buf << "__QNcore/2fcc";
    buf.insertAt(4, "9");
    REQUIRE(String("__QN9core/2fcc") == buf.toString());

    buf.insertAt(0, "1234");
    REQUIRE(String("1234__QN9core/2fcc") == buf.toString());

    buf.insertAt(18, "5432");
    REQUIRE(String("1234__QN9core/2fcc5432") == buf.toString());
  }

  SECTION("Insert String")
  {
    StringBuffer buf;
    buf << "__QNcore/2fcc";
    buf.insertAt(4, String("9"));
    REQUIRE(String("__QN9core/2fcc") == buf.toString());

    buf.insertAt(0, String("1234"));
    REQUIRE(String("1234__QN9core/2fcc") == buf.toString());

    buf.insertAt(18, String("5432"));
    REQUIRE(String("1234__QN9core/2fcc5432") == buf.toString());
  }

  SECTION("Insert Char")
  {
    StringBuffer buf;
    buf << "__QNcore/2fcc";
    buf.insertAt(4, Char('9'));
    REQUIRE(String("__QN9core/2fcc") == buf.toString());

    buf.insertAt(0, Char('x'));
    REQUIRE(String("x__QN9core/2fcc") == buf.toString());

    buf.insertAt(15, Char('!'));
    REQUIRE(String("x__QN9core/2fcc!") == buf.toString());
  }
}
