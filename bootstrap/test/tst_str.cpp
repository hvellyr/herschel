/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../str.hpp"

#include <iostream>
#include <string>


using namespace herschel;


TEST_CASE("String length", "[string]")
{
  REQUIRE(String().length() == 0);
  REQUIRE(String("").length() == 0);
  REQUIRE(String("hello, world!").length() == 13);
}


TEST_CASE("String compare", "[string]")
{
  REQUIRE(String("abc") == String("abc"));
  REQUIRE(String("") == String(""));
  REQUIRE(String("") != String("abc"));
  REQUIRE(String("abc") < String("xyz"));
  REQUIRE(String("abc") <= String("abc"));
  REQUIRE(String("abc") <= String("xyz"));
  REQUIRE(String("abc") != String("xyz"));
  REQUIRE(String("xyz") > String("abc"));
  REQUIRE(String("xyz") >= String("abc"));
  REQUIRE(String("xyz") >= String("xyz"));
  REQUIRE(String("abc") < String("xyz!"));
  REQUIRE(String("xyz!") > String("abc"));
}


TEST_CASE("String find parts", "[string]")
{
  String t("hello, world!");

  SECTION("Starts with")
  {
    REQUIRE(t.startsWith(String("hello")));
    REQUIRE(t.startsWith(String()));
  }

  SECTION("Ends with")
  {
    REQUIRE(t.endsWith(String("world!")));
    REQUIRE(t.endsWith(String()));

    REQUIRE(String("a").endsWith(String("a")));
    REQUIRE(!String().endsWith(String("a")));
    REQUIRE(String("a").endsWith(String()));
  }

  SECTION("Character access")
  {
    REQUIRE(t[6] == ' ');
    REQUIRE(t[0] == 'h');
    REQUIRE(t[12] == '!');
    REQUIRE(t[t.length() - 1] == '!');
  }

  SECTION("Find characters")
  {
    REQUIRE(t.indexOf('h') == 0);
    REQUIRE(t.indexOf('!') == 12);
    REQUIRE(t.indexOf('!', 12) == 12);
    REQUIRE(t.indexOf('x') == -1);

    REQUIRE(t.indexOf('o') == 4);
    REQUIRE(t.indexOf('o', 4) == 4);
    REQUIRE(t.indexOf('o', 5) == 8);
    REQUIRE(t.indexOf('o', 9) == -1);
  }

  SECTION("Find substrings")
  {
    REQUIRE(t.indexOf(String("hello")) == 0);
    REQUIRE(t.indexOf(String("hello"), 1) == -1);

    REQUIRE(t.indexOf(String("world")) == 7);
  }

  SECTION("Find characters reverse")
  {
    REQUIRE(t.lastIndexOf('!') == 12);
    REQUIRE(t.lastIndexOf('h') == 0);
    REQUIRE(t.lastIndexOf('x') == -1);

    REQUIRE(t.lastIndexOf('!', 13) == 12);
    REQUIRE(t.lastIndexOf('!', 12) == 12);
    REQUIRE(t.lastIndexOf('h', 7) == 0);
    REQUIRE(t.lastIndexOf('x', 11) == -1);
  }

  SECTION("Find substrings reverse")
  {
    REQUIRE(t.lastIndexOf(String("world")) == 7);
    REQUIRE(t.lastIndexOf(String("world"), 13) == 7);
    REQUIRE(t.lastIndexOf(String("world"), 12) == 7);
    REQUIRE(t.lastIndexOf(String("world"), 11) == -1);
    REQUIRE(t.lastIndexOf(String("world"), 6) == -1);
    REQUIRE(t.lastIndexOf(String("hello"), 6) == 0);
    REQUIRE(t.lastIndexOf(String("hello"), 0) == -1);

    REQUIRE(t.lastIndexOf(String()) == 13);
    REQUIRE(t.lastIndexOf(String(), 10) == 10);
  }
}


TEST_CASE("String append string and number", "[string]")
{
  REQUIRE(String("hello") +
          String(", ") + String("world!") == String("hello, world!"));
  REQUIRE(String() + "hello" + ", " + "world!" == String("hello, world!"));
  REQUIRE(String("midi: ") + 128 == String("midi: 128"));
  REQUIRE(String("midi: ") + fromInt(128) == String("midi: 128"));
  REQUIRE(String("midi?: ") + fromBool(true) == String("midi?: true"));
}


TEST_CASE("String append char", "[string]")
{
  String t = String("hello") + Char(',') + ' ' + String("world") + Char('!');
  char tmp[256];
  t.toUtf8(tmp, 256);
  REQUIRE(t == String("hello, world!"));
}


TEST_CASE("String convert to utf8", "[string]")
{
  String t("hello, world!");
  char tmp[128];
  REQUIRE(t.toUtf8(nullptr, 128) == 13);
  REQUIRE(t.toUtf8(tmp, 128) == 13);
  REQUIRE(tmp[13] == '\0');
  REQUIRE(strcmp(tmp, "hello, world!") == 0);
}


TEST_CASE("String convert to utf8 limit", "[string]")
{
  String t("hello, world!");
  char tmp[128];
  REQUIRE(t.toUtf8(nullptr, 6) == 5);
  REQUIRE(t.toUtf8(tmp, 6) == 5);
  REQUIRE(tmp[5] == '\0');
  REQUIRE(strcmp(tmp, "hello") == 0);
}


TEST_CASE("String convert empty string to utf8", "[string]")
{
  String t;
  char tmp[128];
  REQUIRE(t.toUtf8(nullptr, 128) == 0);
  REQUIRE(t.toUtf8(tmp, 128) == 0);
  REQUIRE(tmp[0] == '\0');
  REQUIRE(strcmp(tmp, "") == 0);
}


TEST_CASE("String split", "[string]")
{
  String t("hello, world!");

  SECTION("Split")
  {
    String before, after;

    REQUIRE(t.split(',', before, after) == 5);
    REQUIRE(before == String("hello"));
    REQUIRE(after == String(" world!"));

    REQUIRE(t.split('h', before, after) == 0);
    REQUIRE(before == String());
    REQUIRE(after == String("ello, world!"));

    REQUIRE(t.split('!', before, after) == 12);
    REQUIRE(before == String("hello, world"));
    REQUIRE(after == String());

    REQUIRE(t.split('x', before, after) == -1);

    REQUIRE(t.split(String(", "), before, after) == 5);
    REQUIRE(before == String("hello"));
    REQUIRE(after == String("world!"));

    REQUIRE(t.split(String("hello"), before, after) == 0);
    REQUIRE(before == String());
    REQUIRE(after == String(", world!"));

    REQUIRE(t.split(String("world"), before, after) == 7);
    REQUIRE(before == String("hello, "));
    REQUIRE(after == String("!"));

    REQUIRE(t.split(String("world!"), before, after) == 7);
    REQUIRE(before == String("hello, "));
    REQUIRE(after == String());

    REQUIRE(t.split(String("hello, world!"), before, after) == 0);
    REQUIRE(before == String());
    REQUIRE(after == String());

    REQUIRE(t.split(String("o"), before, after) == 4);
    REQUIRE(before == String("hell"));
    REQUIRE(after == String(", world!"));

    REQUIRE(String().split(String(), before, after) == 0);
    REQUIRE(before == String());
    REQUIRE(after == String());
  }

  SECTION("Part")
  {
    REQUIRE(t.part(0, t.length()) == t);
    REQUIRE(t.part(0, 1) == String("h"));
    REQUIRE(t.part(0, 5) == String("hello"));
    REQUIRE(t.part(7, 12) == String("world"));
    REQUIRE(t.part(13, 20) == String());
  }
}


TEST_CASE("String convert to int", "[string]")
{
  REQUIRE(String("123456").toInt() == 123456);
  REQUIRE(String("123456").toInt(16) == 0x123456);
  REQUIRE(String("123456").toInt(8) == 0123456);
  REQUIRE(String("0").toInt() == 0);
}


TEST_CASE("String convert to int64", "[string]")
{
  REQUIRE(String("-9223372036854775808").toInt64() == INT64_MIN);
  REQUIRE(String("9223372036854775807").toInt64() == INT64_MAX);
  REQUIRE(String("123456").toInt64() == 123456LL);
  REQUIRE(String("0").toInt64() == 0);
}


TEST_CASE("String convert to double", "[string]")
{
  REQUIRE(String("3.1415").toDouble() == 3.1415);
}


TEST_CASE("Macro specifier", "[string][macros]")
{
  SECTION("Macro open")
  {
    String t("\343\200\214");
    char tmp[128];

    REQUIRE(t.toUtf8(nullptr, 6) == 3);
    REQUIRE(t.toUtf8(tmp, 6) == 3);
    REQUIRE(t[0] == 0x300C);
  }

  SECTION("Macro close")
  {
    String t("\343\200\215");
    char tmp[128];

    REQUIRE(t.toUtf8(nullptr, 6) == 3);
    REQUIRE(t.toUtf8(tmp, 6) == 3);
    REQUIRE(t[0] == 0x300D);
  }
}
