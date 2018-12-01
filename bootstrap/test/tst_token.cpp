/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../srcpos.hpp"
#include "../token.hpp"


namespace herschel {

TEST_CASE("Token simple tokens", "[token]")
{
  SrcPos sp;

  REQUIRE(Token(sp, kFloat, 3.1415) == Token(sp, kFloat, 3.1415));
  REQUIRE(Token(sp, kInt, 12345) == Token(sp, kInt, 12345));
  REQUIRE(Token(sp, kChar, 0xac00) == Token(sp, kChar, 0xac00));
  REQUIRE(Token(sp, kString, "abc") == Token(sp, kString, "abc"));
  REQUIRE(Token(sp, kDocString, "abc") == Token(sp, kDocString, "abc"));
  REQUIRE(Token(sp, kSymbol, "abc") == Token(sp, kSymbol, "abc"));
  REQUIRE(Token(sp, kDefId) == Token(sp, kDefId));
  REQUIRE(Token(sp, kRational, Rational(7, 4)) == Token(sp, kRational, Rational(7, 4)));

  REQUIRE(Token() == Token());
  REQUIRE(Token(sp, kParanOpen, kParanClose) == Token(sp, kParanOpen, kParanClose));
  REQUIRE(Token(sp, kMacroOpen, kMacroClose) == Token(sp, kMacroOpen, kMacroClose));

  REQUIRE((Token() << Token(sp, kInt, 25)) == (Token() << Token(sp, kInt, 25)));
  REQUIRE((Token(sp, kParanOpen, kParanClose) << Token(sp, kInt, 25)) ==
          (Token(sp, kParanOpen, kParanClose) << Token(sp, kInt, 25)));
  REQUIRE((Token(sp, kMacroOpen, kMacroClose) << Token(sp, kInt, 25)) ==
          (Token(sp, kMacroOpen, kMacroClose) << Token(sp, kInt, 25)));

  REQUIRE(Token(sp, kFloat, 3.1415).floatValue() == 3.1415);
  REQUIRE(Token(sp, kFloat, 1.2345).tokenType() == kFloat);
  REQUIRE(Token(sp, kBool, true).boolValue() == true);
  REQUIRE(Token(sp, kInt, 0x10000).intValue() == 0x10000);
  REQUIRE(Token(sp, kRational, Rational(23, 27)).rationalValue() == Rational(23, 27));

  REQUIRE(Token(sp, kSymbol, "abc").idValue() == String("abc"));
  REQUIRE(Token(sp, kMacroParam, String("abc")).idValue() == String("abc"));
  REQUIRE(Token(sp, kMacroParamAsStr, String("abc")).idValue() == String("abc"));
  REQUIRE(Token(sp, kString, String("abc")).stringValue() == String("abc"));
  REQUIRE(Token(sp, kDocString, String("abc")).stringValue() == String("abc"));
  REQUIRE(Token(sp, kKeyword, String("abc")).stringValue() == String("abc"));

  REQUIRE(Token(sp, kSymbol, "abc").idValue() == String("abc"));
  REQUIRE(Token(sp, kMacroParam, "abc").idValue() == String("abc"));
  REQUIRE(Token(sp, kMacroParamAsStr, "abc").idValue() == String("abc"));
  REQUIRE(Token(sp, kString, "abc").stringValue() == String("abc"));
  REQUIRE(Token(sp, kDocString, "abc").stringValue() == String("abc"));
  REQUIRE(Token(sp, kKeyword, "abc").stringValue() == String("abc"));
  REQUIRE(Token(sp, kKeyarg, "abc").isKeyArg());
}


TEST_CASE("Token range tokens", "[token]")
{
  SrcPos sp;

#define MAKE_RANGE(_fromty, _fromv, _toty, _tov) \
  (Token() << Token(sp, _fromty, _fromv) << Token(sp, kRange) << Token(sp, _toty, _tov))
#define MAKE_RANGE_2(_fromty, _fromv, _toty, _tov, _stepty, _stepv)                     \
  (Token() << Token(sp, _fromty, _fromv) << Token(sp, kRange) << Token(sp, _toty, _tov) \
           << Token(sp, kBy) << Token(sp, _stepty, _stepv))

  REQUIRE(MAKE_RANGE(kInt, 0, kInt, 25).isConstRange());
  REQUIRE(MAKE_RANGE(kFloat, 0.0, kFloat, 25.0).isConstRange());
  REQUIRE(MAKE_RANGE(kChar, 'a', kChar, 'z').isConstRange());
  REQUIRE(MAKE_RANGE(kBool, false, kBool, true).isConstRange());
  REQUIRE(MAKE_RANGE(kString, "a", kString, "z").isConstRange());
  REQUIRE(MAKE_RANGE(kKeyword, "a", kKeyword, "z").isConstRange());

  REQUIRE(MAKE_RANGE_2(kInt, 0, kInt, 25, kInt, 2).isConstRange());
  REQUIRE(MAKE_RANGE_2(kFloat, 0.0, kFloat, 25.0, kFloat, 0.2).isConstRange());
  REQUIRE(MAKE_RANGE_2(kChar, 'a', kChar, 'z', kInt, 1).isConstRange());
  REQUIRE(MAKE_RANGE_2(kString, "a", kString, "z", kInt, 1).isConstRange());
  REQUIRE(MAKE_RANGE_2(kKeyword, "a", kKeyword, "z", kInt, 2).isConstRange());

  REQUIRE(!(Token(sp, kInt, 5).isConstRange()));
  REQUIRE(
      !(Token() << Token(sp, kSymbol, "abc") << Token(sp, kRange) << Token(sp, kInt, 27))
           .isConstRange());
}


TEST_CASE("Token assign operator", "[token]")
{
  SrcPos sp;

#define TEST_ASSIGNOP2(_type, _value, _member) \
  {                                            \
    Token t = Token(sp, _type, _value);        \
    REQUIRE(t.tokenType() == _type);           \
    REQUIRE(t._member() == _value);            \
  }

  TEST_ASSIGNOP2(kFloat, 3.1415, floatValue);
  TEST_ASSIGNOP2(kBool, true, boolValue);
  TEST_ASSIGNOP2(kInt, 0x20000, intValue);
  TEST_ASSIGNOP2(kRational, Rational(1, 127), rationalValue);
  TEST_ASSIGNOP2(kString, String("abc"), stringValue);
#undef TEST_ASSIGNOP2
}


TEST_CASE("Token copy constructor", "[token]")
{
  SrcPos sp;

#define TEST_COPYCTOR2(_type, _value, _member) \
  {                                            \
    Token t(Token(sp, _type, _value));         \
    REQUIRE(t.tokenType() == _type);           \
    REQUIRE(t._member() == _value);            \
  }

  TEST_COPYCTOR2(kFloat, 3.1415, floatValue);
  TEST_COPYCTOR2(kBool, true, boolValue);
  TEST_COPYCTOR2(kInt, 0x20000, intValue);
  TEST_COPYCTOR2(kRational, Rational(1, 127), rationalValue);
  TEST_COPYCTOR2(kString, String("abc"), stringValue);
#undef TEST_COPYCTOR2
}


TEST_CASE("Token imaginary numbers", "[token][numbers]")
{
  SrcPos sp;
  Token t(Token(sp, kFloat, 12.345).setIsImaginary(true));
  REQUIRE(t.tokenType() == kFloat);
  REQUIRE(t.floatValue() == 12.345);
  REQUIRE(t.isImaginary());
}


TEST_CASE("Token is qualified ID", "[token]")
{
  SECTION("Unqualified names")
  {
    SrcPos sp;
    Token t = Token(sp, kSymbol, "File");
    REQUIRE(!t.isQualifiedId());
    REQUIRE(t.baseName() == String("File"));
    REQUIRE(t.nsName() == String());
  }


  SECTION("Qualified names")
  {
    SrcPos sp;
    Token t = Token(sp, kSymbol, "io.File");
    REQUIRE(t.isQualifiedId());
    REQUIRE(t.baseName() == String("File"));
    REQUIRE(t.nsName() == String("io"));
  }


  SECTION("Multi qualified names")
  {
    SrcPos sp;
    Token t = Token(sp, kSymbol, "core.rubitz.packs.Package");
    REQUIRE(t.isQualifiedId());
    REQUIRE(t.baseName() == String("Package"));
    REQUIRE(t.nsName() == String("core.rubitz.packs"));
  }
}


TEST_CASE("Token unwrap singleton", "[token]")
{
  SrcPos sp;
  REQUIRE((Token() << Token(sp, kSymbol, "abc")).unwrapSingleton() ==
          Token(sp, kSymbol, "abc"));

  Token t = Token() << Token(sp, kSymbol, "abc") << Token(sp, kInt, 27);
  REQUIRE(t.unwrapSingleton() == t);

  REQUIRE(Token().unwrapSingleton() == Token());
  REQUIRE(Token(sp, kInt, 4).unwrapSingleton() == Token(sp, kInt, 4));
}


TEST_CASE("Token qualified macro name", "[token][macros]")
{
  SrcPos sp;
  Token t = Token(sp, kMacroParam, "abc:name");
  REQUIRE(t == kMacroParam);
  REQUIRE(t.macroParamName() == String("abc"));
  REQUIRE(t.macroParamType() == String("name"));
}


TEST_CASE("Token wilcard macro name", "[token][macros]")
{
  SrcPos sp;
  Token t = Token(sp, kMacroParam, "*:expr");
  REQUIRE(t == kMacroParam);
  REQUIRE(t.macroParamName() == String("*"));
  REQUIRE(t.macroParamType() == String("expr"));
}


TEST_CASE("Token name only macro name", "[token][macros]")
{
  SrcPos sp;
  Token t = Token(sp, kMacroParam, "abc");
  REQUIRE(t == kMacroParam);
  REQUIRE(t.macroParamName() == String("abc"));
  REQUIRE(t.macroParamType() == String());
}


TEST_CASE("Token macro name as string", "[token][string][macros]")
{
  SrcPos sp;
  Token t = Token(sp, kMacroParamAsStr, "abc");
  REQUIRE(t == kMacroParamAsStr);
  REQUIRE(t.macroParamName() == String("abc"));
  REQUIRE(t.macroParamType() == String());
}

}  // namespace herschel
