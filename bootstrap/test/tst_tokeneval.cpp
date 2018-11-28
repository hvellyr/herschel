/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../tokeneval.hpp"

using namespace herschel;

namespace
{
  bool approxEqual(float a, float b, float epsilon)
  {
    return fabs(a - b) <= ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
  }
}

struct TokenEvalContextFixture
{
  TokenEvalContextFixture()
    : ctx(reg)
  {
  }

  ~TokenEvalContextFixture() {
  }

  ConfigVarRegistry reg;
  TokenEvalContext ctx;
  SrcPos sp;
  Token t;
};


TEST_CASE("TokenEval basic", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(Token(f.sp, kInt, 25));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 25);

  f.t = f.ctx.evalToken(Token(f.sp, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 3.1415);

  f.t = f.ctx.evalToken(Token(f.sp, kRational, Rational(101, 127)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(101, 127));

  f.t = f.ctx.evalToken(Token(f.sp, kString, "hello world"));
  REQUIRE(f.t.isString());
  REQUIRE(f.t.stringValue() == String("hello world"));

  f.t = f.ctx.evalToken(Token(f.sp, kChar, 0xac00));
  REQUIRE(f.t.isChar());
  REQUIRE(f.t.charValue() == Char(0xac00));
}


#define MAKE_BINARY_SEQ(_ltype, _lvalue, _op, _rtype, _rvalue)  \
  Token() << Token(f.sp, _ltype, _lvalue)                       \
  << Token(f.sp, _op)                                           \
  << Token(f.sp, _rtype, _rvalue)

TEST_CASE("TokenEval add", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kPlus, kInt, 17));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 42);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kPlus, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 28.1415);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kPlus, kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(103, 4));


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kPlus, kInt, 25));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 28.1415);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kPlus, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 1000) == 6283);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kPlus, kRational, Rational(3, 4)));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 3.8915);


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus, kInt, 25));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(103, 4));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 3.8915);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus,
                                        kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(6, 4));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus,
                                        kRational, Rational(2, 5)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(23, 20));
}


TEST_CASE("TokenEval minus", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMinus, kInt, 17));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 8);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMinus, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 21.8585);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMinus, kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(97, 4));


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kMinus, kInt, 25));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == -21.8585);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kMinus, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 1000) == 0.0);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kMinus, kRational, Rational(3, 4)));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 2.3915);


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus, kInt, 25));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(-97, 4));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == -2.3915);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus,
                                        kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(0, 4));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus,
                                        kRational, Rational(2, 5)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(7, 20));
}


TEST_CASE("TokenEval multiply", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMultiply, kInt, 17));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 425);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMultiply, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 10000) == 785375);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMultiply, kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(75, 4));


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kMultiply, kInt, 25));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 10000) == 785375);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kMultiply, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 100000000) == 986902225);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kMultiply, kRational, Rational(3, 4)));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 1000000) == 2356125);


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply, kInt, 25));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(75, 4));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 1000000) == 2356125);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply,
                                        kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(9, 16));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply,
                                        kRational, Rational(2, 5)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(6, 20));
}


TEST_CASE("TokenEval divide", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kDivide, kInt, 17));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 1);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kDivide, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 100000000) == 795798185);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kDivide, kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(100, 3));


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kDivide, kInt, 25));
  REQUIRE(f.t.isFloat());
  REQUIRE(approxEqual(f.t.floatValue(), 0.12566, 0.0001));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kDivide, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(f.t.floatValue() == 1.0);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kDivide, kRational, Rational(3, 4)));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 100000000) == 418866666);


  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide, kInt, 25));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(3, 100));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide, kFloat, 3.1415));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 100000000) == 23873945);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide,
                                        kRational, Rational(3, 4)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(12, 12));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide,
                                        kRational, Rational(2, 5)));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(15, 8));
}


TEST_CASE("TokenEval modulo", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, -340, kMod, kInt, 60));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 20);
}


TEST_CASE("TokenEval remainder", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, -340, kRem, kInt, 60));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == -40);
}


TEST_CASE("TokenEval exponent", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 2, kExponent, kInt, 16));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 65536);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kFloat, 3.1415, kExponent, kInt, 4));
  REQUIRE(f.t.isFloat());
  REQUIRE(int(f.t.floatValue() * 10000) == 973976);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kExponent,
                                        kInt, 4));
  REQUIRE(f.t.isRational());
  REQUIRE(f.t.rationalValue() == Rational(81, 256));
}


TEST_CASE("TokenEval bitAND", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xacde, kBitAnd, kInt, 0x0ff0));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0x0cd0);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xacde, kBitAnd, kInt, 0xf0f0));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0xa0d0);
}


TEST_CASE("TokenEval bitOR", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xa0d0, kBitOr, kInt, 0x0c0e));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0xacde);
}


TEST_CASE("TokenEval bitXOR", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xacde, kBitXor, kInt, 0xffff));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0x5321);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0x0000, kBitXor, kInt, 0xfefe));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0xfefe);
}


TEST_CASE("TokenEval shift left", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xabcd, kShiftLeft, kInt, 3));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0x55e68);
}


TEST_CASE("TokenEval shift right", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0x55e68, kShiftRight, kInt, 3));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0xabcd);
}


TEST_CASE("TokenEval compare operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kCompare, kInt, 17));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() > 0);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 17, kCompare, kInt, 25));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() < 0);

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 17, kCompare, kInt, 17));
  REQUIRE(f.t.isInt());
  REQUIRE(f.t.intValue() == 0);
}


TEST_CASE("TokenEval equal operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kEqual, kInt, 17));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kEqual, kInt, 25));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
}


TEST_CASE("TokenEval unequal operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kUnequal, kInt, 17));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kUnequal, kInt, 25));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
}


TEST_CASE("TokenEval greater operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreater, kInt, 17));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreater, kInt, 25));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
}


TEST_CASE("TokenEval greater equal operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreaterEqual, kInt, 17));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreaterEqual, kInt, 25));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
}


TEST_CASE("TokenEval less operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLess, kInt, 17));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLess, kInt, 25));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
}


TEST_CASE("TokenEval less equal operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLessEqual, kInt, 17));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLessEqual, kInt, 25));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
}


TEST_CASE("TokenEval logical and operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalAnd, kBool, true));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalAnd, kBool, false));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalAnd, kBool, true));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalAnd, kBool, false));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
}


TEST_CASE("TokenEval logical or operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalOr, kBool, true));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalOr, kBool, false));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalOr, kBool, true));
  REQUIRE(f.t.isBool());
  REQUIRE(f.t.boolValue());
  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalOr, kBool, false));
  REQUIRE(f.t.isBool());
  REQUIRE(!f.t.boolValue());
}


TEST_CASE("TokenEval concat operator", "[token-eval][token]")
{
  TokenEvalContextFixture f;

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kString, String("hello "), kConcat,
                                        kString, String("world")));
  REQUIRE(f.t.isString());
  REQUIRE(f.t.stringValue() == String("hello world"));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kString, String("hello "), kConcat,
                                        kString, String("")));
  REQUIRE(f.t.isString());
  REQUIRE(f.t.stringValue() == String("hello "));

  f.t = f.ctx.evalToken(MAKE_BINARY_SEQ(kString, String(""), kConcat,
                                        kString, String("")));
  REQUIRE(f.t.isString());
  REQUIRE(f.t.stringValue() == String(""));
}

