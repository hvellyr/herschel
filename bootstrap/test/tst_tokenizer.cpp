/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../log.hpp"
#include "../tokenizer.hpp"
#include "../type.hpp"

#include <iostream>

using namespace herschel;


TEST_CASE("Tokenizer basic", "[tokenize]")
{
  SrcPos sp;

  static zstring test =
    "module zero (\"eyestep/zero 1.0:portables\")\n"
    "  export public(*)\n"
    "-- a simple portable class\n"
    "def class Portable<T>(x @ Int) : (Copyable, Comparable)\n"
    "{\n"
    "  slot first : T = x ;\n"
    "  slot data : Octet[]\n"
    "}\n";

  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, ::strlen(test))),
                String("n.n."));

  REQUIRE(tnz.nextToken() == Token(sp, kModuleId));
  REQUIRE(tnz.nextToken() == Token(sp, String("zero")));
  REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
  REQUIRE(tnz.nextToken() == Token(sp, kString, String("eyestep/zero 1.0:portables")));
  REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

  REQUIRE(tnz.nextToken() == Token(sp, kExportId));
  REQUIRE(tnz.nextToken() == Token(sp, String("public")));
  REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
  REQUIRE(tnz.nextToken() == Token(sp, kMultiply));
  REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

  REQUIRE(tnz.nextToken() == Token(sp, kDefId));
  REQUIRE(tnz.nextToken() == Token(sp, String("class")));
  REQUIRE(tnz.nextToken() == Token(sp, String("Portable")));
  REQUIRE(tnz.nextToken() == Token(sp, kGenericOpen));
  REQUIRE(tnz.nextToken() == Token(sp, String("T")));
  REQUIRE(tnz.nextToken() == Token(sp, kGenericClose));
  REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
  REQUIRE(tnz.nextToken() == Token(sp, String("x")));
  REQUIRE(tnz.nextToken() == Token(sp, kAt));
  REQUIRE(tnz.nextToken() == Token(sp, String("Int")));
  REQUIRE(tnz.nextToken() == Token(sp, kParanClose));
  REQUIRE(tnz.nextToken() == Token(sp, kColon));
  REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
  REQUIRE(tnz.nextToken() == Token(sp, String("Copyable")));
  REQUIRE(tnz.nextToken() == Token(sp, kComma));
  REQUIRE(tnz.nextToken() == Token(sp, String("Comparable")));
  REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

  REQUIRE(tnz.nextToken() == Token(sp, kBraceOpen));
  REQUIRE(tnz.nextToken() == Token(sp, String("slot")));
  REQUIRE(tnz.nextToken() == Token(sp, String("first")));
  REQUIRE(tnz.nextToken() == Token(sp, kColon));
  REQUIRE(tnz.nextToken() == Token(sp, String("T")));
  REQUIRE(tnz.nextToken() == Token(sp, kAssign));
  REQUIRE(tnz.nextToken() == Token(sp, String("x")));

  REQUIRE(tnz.nextToken() == Token(sp, kSemicolon));

  REQUIRE(tnz.nextToken() == Token(sp, String("slot")));
  REQUIRE(tnz.nextToken() == Token(sp, String("data")));
  REQUIRE(tnz.nextToken() == Token(sp, kColon));
  REQUIRE(tnz.nextToken() == Token(sp, String("Octet")));
  REQUIRE(tnz.nextToken() == Token(sp, kBracketOpen));
  REQUIRE(tnz.nextToken() == Token(sp, kBracketClose));
  REQUIRE(tnz.nextToken() == Token(sp, kBraceClose));
}


TEST_CASE("Tokenizer numbers", "[tokenize][numbers]")
{
  SrcPos sp;

  static zstring test =
    "true false\n"
    "12345 0aaaah 0aBcDeFh 07123q 101101y 1y 2t 3h 4\n"
    "12.34 0.12345e+10 123.45e+7 12.3456e-5 -3.1415\n"
    "2/3 120/33 1/1024\n"
    "5i  3.1415i\n";
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."));

  try {
    REQUIRE(tnz.nextToken() == Token(sp, kBool, true));
    REQUIRE(tnz.nextToken() == Token(sp, kBool, false));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 12345));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 0xaaaa));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 0xabcdef));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 07123));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 45));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 3));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 4));

    REQUIRE(tnz.nextToken() == Token(sp, kFloat, 12.34));
    REQUIRE(tnz.nextToken() == Token(sp, kFloat, 0.12345e+10));
    REQUIRE(tnz.nextToken() == Token(sp, kFloat, 0.12345e+10)); // normalized 123.45e+7
    REQUIRE(tnz.nextToken() == Token(sp, kFloat, 0.000123456)); // normalized
    REQUIRE(tnz.nextToken() == Token(sp, kFloat, -3.1415));

    REQUIRE(tnz.nextToken() == Token(sp, kRational, Rational(2, 3)));
    REQUIRE(tnz.nextToken() == Token(sp, kRational, Rational(120, 33)));
    REQUIRE(tnz.nextToken() == Token(sp, kRational, Rational(1, 1024)));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 5).setIsImaginary(true));
    REQUIRE(tnz.nextToken() == Token(sp, kFloat, 3.1415).setIsImaginary(true));
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}


TEST_CASE("Tokenizer chars", "[tokenize][chars]")
{
  SrcPos sp;

  static zstring test =
    "\\space  \\u60h  \\( \\newline \\cr\n"
    "\"hello,\\nl;world!\"  \"\\esc;\\u61h;\\(\\;;\"\n"
    "\\ga \\gong ";
  auto cr = std::make_shared<CharRegistry>();
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."), cr);
  cr->registerValue(String("ga"), 0xac00);
  cr->registerValue(String("gong"), 0xacf5);

  try {
    REQUIRE(tnz.nextToken() == Token(sp, kChar, 0x20));
    REQUIRE(tnz.nextToken() == Token(sp, kChar, 0x60));
    REQUIRE(tnz.nextToken() == Token(sp, kChar, '('));
    REQUIRE(tnz.nextToken() == Token(sp, kChar, 0x0a));
    REQUIRE(tnz.nextToken() == Token(sp, kChar, 0x0d));

    REQUIRE(tnz.nextToken() == Token(sp, kString, String("hello,\nworld!")));
    REQUIRE(tnz.nextToken() == Token(sp, kString, String("\033a(;;")));

    REQUIRE(tnz.nextToken() == Token(sp, kChar, 0xac00));
    REQUIRE(tnz.nextToken() == Token(sp, kChar, 0xacf5));
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}


TEST_CASE("Tokenizer function defs", "[tokenize][functions]")
{
  SrcPos sp;

  static zstring test =
    "def f(args : &(String, Uri, Boolean)[] ...) ...\n"
    "  ~ Some function f, does not contain \\~ or similar Spuk.~\n"
    "def f(arg: _x = 0 .. 20 by 2)\n"
    "def g(a @ ^'T)\n"
    "def h(a : ^Repo) a^ = 5 &m = 4\n";
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."));

  try {
    REQUIRE(tnz.nextToken() == Token(sp, kDefId));
    REQUIRE(tnz.nextToken() == Token(sp, String("f")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("args")));
    REQUIRE(tnz.nextToken() == Token(sp, kColon));
    REQUIRE(tnz.nextToken() == Token(sp, kUnionOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("String")));
    REQUIRE(tnz.nextToken() == Token(sp, kComma));
    REQUIRE(tnz.nextToken() == Token(sp, String("Uri")));
    REQUIRE(tnz.nextToken() == Token(sp, kComma));
    REQUIRE(tnz.nextToken() == Token(sp, String("Boolean")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));
    REQUIRE(tnz.nextToken() == Token(sp, kBracketOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kBracketClose));
    REQUIRE(tnz.nextToken() == Token(sp, kEllipsis));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));
    REQUIRE(tnz.nextToken() == Token(sp, kEllipsis));

    REQUIRE(tnz.nextToken() == Token(sp, kDocString,
                                       " Some function f, does not contain ~ or similar Spuk."));

    REQUIRE(tnz.nextToken() == Token(sp, kDefId));
    REQUIRE(tnz.nextToken() == Token(sp, String("f")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kKeyarg, String("arg")));
    REQUIRE(tnz.nextToken() == Token(sp, String("_x")));
    REQUIRE(tnz.nextToken() == Token(sp, kAssign));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 0));
    REQUIRE(tnz.nextToken() == Token(sp, kRange));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 20));
    REQUIRE(tnz.nextToken() == Token(sp, kBy));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

    REQUIRE(tnz.nextToken() == Token(sp, kDefId));
    REQUIRE(tnz.nextToken() == Token(sp, String("g")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kAt));
    REQUIRE(tnz.nextToken() == Token(sp, kReference));
    REQUIRE(tnz.nextToken() == Token(sp, kQuote));
    REQUIRE(tnz.nextToken() == Token(sp, String("T")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

    REQUIRE(tnz.nextToken() == Token(sp, kDefId));
    REQUIRE(tnz.nextToken() == Token(sp, String("h")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kColon));
    REQUIRE(tnz.nextToken() == Token(sp, kReference));
    REQUIRE(tnz.nextToken() == Token(sp, String("Repo")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kReference));
    REQUIRE(tnz.nextToken() == Token(sp, kAssign));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 5));

    REQUIRE(tnz.nextToken() == Token(sp, kAmpersand));
    REQUIRE(tnz.nextToken() == Token(sp, String("m")));
    REQUIRE(tnz.nextToken() == Token(sp, kAssign));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 4));
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}


TEST_CASE("Tokenizer keyword static container", "[tokenize]")
{
  SrcPos sp;

  static zstring test =
    "#abc #delft\n"
    "#[1, 2] #[]\n"
    "#(1 -> 2) #()\n"
    "&(1, 2)\n";
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."));

  try {
    REQUIRE(tnz.nextToken() == Token(sp, kKeyword, String("abc")));
    REQUIRE(tnz.nextToken() == Token(sp, kKeyword, String("delft")));

    REQUIRE(tnz.nextToken() == Token(sp, kLiteralArrayOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kComma));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kBracketClose));

    REQUIRE(tnz.nextToken() == Token(sp, kLiteralArrayOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kBracketClose));

    REQUIRE(tnz.nextToken() == Token(sp, kLiteralVectorOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kMapTo));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

    REQUIRE(tnz.nextToken() == Token(sp, kLiteralVectorOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

    REQUIRE(tnz.nextToken() == Token(sp, kUnionOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kComma));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}


TEST_CASE("Tokenizer generics", "[tokenize][generics]")
{
  SrcPos sp;

  static zstring test =
    "Buffer<Int>()[i] < 10 and true or false\n"
    "T<S<Y>>  T<S<Y> >  a < b\n"
    "val << 5 val >> 2\n"
    "2 < 1  2 <= 1  2 > 1  2 >= 1  2 <=> 1  2 <> 1  2 == 1\n"
    "a + b  \"a\" ++ \"b\" a - b  a * b  a / b  a ** 2  a mod 5  a rem 5\n"
    "1 XOR 2  1 OR 2  1 AND 2\n"
    "1 % 2  1 -> 2  1 in 2  1 isa Number  1 as Octet\n"
    "|abc ->abc\n";
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."));

  try {
    REQUIRE(tnz.nextToken() == Token(sp, String("Buffer")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("Int")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericClose));
    REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));
    REQUIRE(tnz.nextToken() == Token(sp, kBracketOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("i")));
    REQUIRE(tnz.nextToken() == Token(sp, kBracketClose));
    REQUIRE(tnz.nextToken() == Token(sp, kLess));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 10));
    REQUIRE(tnz.nextToken() == Token(sp, kLogicalAnd));
    REQUIRE(tnz.nextToken() == Token(sp, kBool, true));
    REQUIRE(tnz.nextToken() == Token(sp, kLogicalOr));
    REQUIRE(tnz.nextToken() == Token(sp, kBool, false));

    REQUIRE(tnz.nextToken() == Token(sp, String("T")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("S")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("Y")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericClose));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericClose));

    REQUIRE(tnz.nextToken() == Token(sp, String("T")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("S")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericOpen));
    REQUIRE(tnz.nextToken() == Token(sp, String("Y")));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericClose));
    REQUIRE(tnz.nextToken() == Token(sp, kGenericClose));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kLess));
    REQUIRE(tnz.nextToken() == Token(sp, String("b")));

    REQUIRE(tnz.nextToken() == Token(sp, String("val")));
    REQUIRE(tnz.nextToken() == Token(sp, kShiftLeft));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 5));
    REQUIRE(tnz.nextToken() == Token(sp, String("val")));
    REQUIRE(tnz.nextToken() == Token(sp, kShiftRight));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kLess));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kLessEqual));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kGreater));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kGreaterEqual));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kCompare));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kUnequal));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));
    REQUIRE(tnz.nextToken() == Token(sp, kEqual));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kPlus));
    REQUIRE(tnz.nextToken() == Token(sp, String("b")));

    REQUIRE(tnz.nextToken() == Token(sp, kString, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kConcat));
    REQUIRE(tnz.nextToken() == Token(sp, kString, String("b")));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kMinus));
    REQUIRE(tnz.nextToken() == Token(sp, String("b")));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kMultiply));
    REQUIRE(tnz.nextToken() == Token(sp, String("b")));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kDivide));
    REQUIRE(tnz.nextToken() == Token(sp, String("b")));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kExponent));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kMod));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 5));

    REQUIRE(tnz.nextToken() == Token(sp, String("a")));
    REQUIRE(tnz.nextToken() == Token(sp, kRem));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 5));


    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kBitXor));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kBitOr));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kBitAnd));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));


    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kFold));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kMapTo));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kIn));
    REQUIRE(tnz.nextToken() == Token(sp, kInt, 2));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kIsa));
    REQUIRE(tnz.nextToken() == Token(sp, String("Number")));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 1));
    REQUIRE(tnz.nextToken() == Token(sp, kAs));
    REQUIRE(tnz.nextToken() == Token(sp, String("Octet")));

    REQUIRE(tnz.nextToken() == Token(sp, String("|abc")));
    REQUIRE(tnz.nextToken() == Token(sp, String("->abc")));
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}


TEST_CASE("Tokenizer namespaces", "[tokenize][namespaces]")
{
  SrcPos sp;

  static zstring test =
    "io|File  self.io|val.display\n"
    "f('T)  12'mm\n";
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."));

  try {
    REQUIRE(tnz.nextToken() == Token(sp, String("io|File")));

    REQUIRE(tnz.nextToken() == Token(sp, String("self")));
    REQUIRE(tnz.nextToken() == Token(sp, kDot));
    REQUIRE(tnz.nextToken() == Token(sp, String("io|val")));
    REQUIRE(tnz.nextToken() == Token(sp, kDot));
    REQUIRE(tnz.nextToken() == Token(sp, String("display")));

    REQUIRE(tnz.nextToken() == Token(sp, String("f")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kQuote));
    REQUIRE(tnz.nextToken() == Token(sp, String("T")));
    REQUIRE(tnz.nextToken() == Token(sp, kParanClose));

    REQUIRE(tnz.nextToken() == Token(sp, kInt, 12));
    REQUIRE(tnz.nextToken() == Token(sp, kQuote));
    REQUIRE(tnz.nextToken() == Token(sp, String("mm")));
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}


TEST_CASE("Tokenizer macro vars", "[tokenize][macros]")
{
  SrcPos sp;

  static zstring test =
    "##  ?val:name ?\"abc\" ?\"\" ";
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."));

  try {
    REQUIRE(tnz.nextToken() == Token(sp, kSangHash));
    REQUIRE(tnz.nextToken() == Token(sp, kMacroParam, "val:name"));
    REQUIRE(tnz.nextToken() == Token(sp, kMacroParamAsStr, "abc"));

    {
      LogSurpressor beSilent;
      // ?"" is not allowed.
      REQUIRE(tnz.nextToken() == Token());
    }
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}


TEST_CASE("Tokenizer special macro brackets", "[tokenize][macros]")
{
  SrcPos sp;

  static zstring test =
    "\343\200\214 xyz \343\200\215 ";
  Tokenizer tnz(std::make_shared<CharPort>(
                  std::make_shared<DataPort>((Octet*)test, strlen(test))),
                String("n.n."));

  try {
    REQUIRE(tnz.nextToken() == Token(sp, kMacroOpen));
    REQUIRE(tnz.nextToken() == Token(sp, kSymbol, "xyz"));
    REQUIRE(tnz.nextToken() == Token(sp, kMacroClose));
  }
  catch (const Exception& ne) {
    logf(kError, StrHelper(ne.message()));
  }
}
