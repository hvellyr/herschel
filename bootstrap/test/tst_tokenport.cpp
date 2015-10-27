/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../tokenport.h"

using namespace herschel;


TEST_CASE("TokenPort file token port", "[port][data-port][token-port]")
{
  static const char* fTest =
    "module zero (\"eyestep/zero 1.0:portables\")\n"
    "  export public(*)\n"
    "-- a simple portable class\n"
    "def class Portable<T>(x @ Int) : (Copyable, Comparable)\n"
    "{\n"
    "  slot first : T = x ;\n"
    "  slot data : Octet[]\n"
    "}\n";

  SrcPos sp;
  Ptr<TokenPort> p = new FileTokenPort(new DataPort((Octet*)fTest, ::strlen(fTest)),
                                       String("test"));
  REQUIRE(p->read() == Token(sp, kModuleId));
  REQUIRE(p->read() == Token(sp, kSymbol, String("zero")));
  REQUIRE(p->read() == Token(sp, kParanOpen));
  REQUIRE(p->read() == Token(sp, kString, String("eyestep/zero 1.0:portables")));
  REQUIRE(p->read() == Token(sp, kParanClose));

  REQUIRE(p->read() == Token(sp, kExportId));
  REQUIRE(p->read() == Token(sp, kSymbol, String("public")));
  REQUIRE(p->read() == Token(sp, kParanOpen));
  REQUIRE(p->read() == Token(sp, kMultiply));
  REQUIRE(p->read() == Token(sp, kParanClose));

  REQUIRE(p->read() == Token(sp, kDefId));
  REQUIRE(p->read() == Token(sp, kSymbol, String("class")));
  REQUIRE(p->read() == Token(sp, kSymbol, String("Portable")));
  REQUIRE(p->read() == Token(sp, kGenericOpen));
  REQUIRE(p->read() == Token(sp, kSymbol, String("T")));
  REQUIRE(p->read() == Token(sp, kGenericClose));
  REQUIRE(p->read() == Token(sp, kParanOpen));
  REQUIRE(p->read() == Token(sp, kSymbol, String("x")));
  REQUIRE(p->read() == Token(sp, kAt));
  REQUIRE(p->read() == Token(sp, kSymbol, String("Int")));
  REQUIRE(p->read() == Token(sp, kParanClose));
  REQUIRE(p->read() == Token(sp, kColon));
  REQUIRE(p->read() == Token(sp, kParanOpen));
  REQUIRE(p->read() == Token(sp, kSymbol, String("Copyable")));
  REQUIRE(p->read() == Token(sp, kComma));
  REQUIRE(p->read() == Token(sp, kSymbol, String("Comparable")));
  REQUIRE(p->read() == Token(sp, kParanClose));

  p->unread(Token(sp, kSymbol, "xyz"));
  p->unread(Token(sp, kAssign));
  p->unread(Token(sp, kInt, 1234));
  REQUIRE(p->read() == Token(sp, kInt, 1234));
  REQUIRE(p->read() == Token(sp, kAssign));
  REQUIRE(p->read() == Token(sp, kSymbol, "xyz"));
  REQUIRE(!p->hasUnreadData());

  REQUIRE(p->read() == Token(sp, kBraceOpen));
  REQUIRE(p->read() == Token(sp, kSymbol, String("slot")));
  REQUIRE(p->read() == Token(sp, kSymbol, String("first")));
  REQUIRE(p->read() == Token(sp, kColon));
  REQUIRE(p->read() == Token(sp, kSymbol, String("T")));
  REQUIRE(p->read() == Token(sp, kAssign));
  REQUIRE(p->read() == Token(sp, kSymbol, String("x")));

  REQUIRE(p->read() == Token(sp, kSemicolon));

  REQUIRE(p->read() == Token(sp, kSymbol, String("slot")));
  REQUIRE(p->read() == Token(sp, kSymbol, String("data")));
  REQUIRE(p->read() == Token(sp, kColon));
  REQUIRE(p->read() == Token(sp, kSymbol, String("Octet")));
  REQUIRE(p->read() == Token(sp, kBracketOpen));
  REQUIRE(p->read() == Token(sp, kBracketClose));
  REQUIRE(p->read() == Token(sp, kBraceClose));

  REQUIRE(p->isEof());
}


TEST_CASE("InternalTokenPort", "[port][data-port][token-port]")
{
  SrcPos sp;

  std::list<Token> tokens;
  tokens.push_back(Token(sp, kSymbol, String("def")));
  tokens.push_back(Token(sp, kSymbol, String("const")));
  tokens.push_back(Token(sp, kSymbol, String("x")));
  tokens.push_back(Token(sp, kAssign));
  tokens.push_back(Token(sp, kRational, Rational(2, 3)));

  Ptr<TokenPort> p = new InternalTokenPort(tokens);

  REQUIRE(p->read() == Token(sp, kSymbol, String("def")));
  REQUIRE(p->read() == Token(sp, kSymbol, String("const")));
  REQUIRE(p->read() == Token(sp, kSymbol, String("x")));
  REQUIRE(p->read() == Token(sp, kAssign));
  REQUIRE(p->read() == Token(sp, kRational, Rational(2, 3)));

  REQUIRE(p->isEof());
}

