/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/


#include "token.h"
#include "tokeneval.h"


using namespace heather;

TokenEvalContext::TokenEvalContext(ConfigVarRegistry* registry)
  : fRegistry(registry)
{
}


Token
TokenEvalContext::evalAdd(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt()) {
    int value = left.intValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kInt, value + right.intValue());
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, double(value) + right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) + right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isReal()) {
    double value = left.realValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kReal, value + double(right.intValue()));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value + right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kReal, value + right.rationalValue().toReal());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value + Rational(right.intValue(), 1));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value.toReal() + right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, value + right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalMinus(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt()) {
    int value = left.intValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kInt, value - right.intValue());
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, double(value) - right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) - right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isReal()) {
    double value = left.realValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kReal, value - double(right.intValue()));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value - right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kReal, value - right.rationalValue().toReal());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value - Rational(right.intValue(), 1));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value.toReal() - right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, value - right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalMultiply(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt()) {
    int value = left.intValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kInt, value * right.intValue());
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, double(value) * right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) * right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isReal()) {
    double value = left.realValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kReal, value * double(right.intValue()));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value * right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kReal, value * right.rationalValue().toReal());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value * Rational(right.intValue(), 1));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value.toReal() * right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, value * right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalDivide(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt()) {
    int value = left.intValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kInt, value / right.intValue());
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, double(value) / right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) / right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isReal()) {
    double value = left.realValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kReal, value / double(right.intValue()));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value / right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kReal, value / right.rationalValue().toReal());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (value.numerator() == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value / Rational(right.intValue(), 1));
    else if (right.isReal())
      return Token(left.srcpos(),
                   kReal, value.toReal() / right.realValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, value / right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalModulo(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt()) {
    int value = left.intValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kInt, value % right.intValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalExponent(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt()) {
    int value = left.intValue();

    if (right.isInt())
      return Token(left.srcpos(),
                   kInt, heather::exponent(value, right.intValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isReal()) {
    double value = left.realValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kReal, heather::exponent(value, right.intValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (value.numerator() == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value.exponent(right.intValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalBitOp(const Token& lexpr, const Token& rexpr,
                            OperatorType op) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt() && right.isInt()) {
    int lvalue = left.intValue();
    int rvalue = right.intValue();

    int result = 0;
    switch (op) {
    case kOpBitAnd:     result = lvalue & rvalue; break;
    case kOpBitXor:     result = lvalue ^ rvalue; break;
    case kOpBitOr:      result = lvalue | rvalue; break;
    case kOpShiftLeft:  result = lvalue << rvalue; break;
    case kOpShiftRight: result = lvalue >> rvalue; break;
    default:
      assert(0);
    }
    return Token(left.srcpos(), kInt, result);
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalCompare(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.tokenType() == right.tokenType()) {
    switch (left.tokenType()) {
    case kString:
      return Token(left.srcpos(),
                   kInt, left.stringValue().compare(right.stringValue()));
    case kBool:
      if (left.boolValue() == right.boolValue())
        return Token(left.srcpos(), kInt, 0);
      else
        return Token(left.srcpos(), kInt, -1);
    case kInt:
      return Token(left.srcpos(), kInt, left.intValue() - right.intValue());
    case kReal:
      if (left.realValue() < right.realValue())
        return Token(left.srcpos(), kInt, -1);
      else if (left.realValue() > right.realValue())
        return Token(left.srcpos(), kInt, 1);
      else
        return Token(left.srcpos(), kInt, 0);
    case kRational:
      if (left.rationalValue() < right.rationalValue())
        return Token(left.srcpos(), kInt, -1);
      else if (left.rationalValue() > right.rationalValue())
        return Token(left.srcpos(), kInt, 1);
      else
        return Token(left.srcpos(), kInt, 0);
    case kChar:
      return Token(left.srcpos(),
                   kInt, left.intValue() - right.intValue());

    default:
      throw BadExpressionException(fromInt(__LINE__));
    }
  }

  return Token(left.srcpos(), kInt, -1);
}


Token
TokenEvalContext::evalLogical(const Token& lexpr, const Token& rexpr,
                              OperatorType op) const
{
  Token p = evalCompare(lexpr, rexpr);
  bool result = false;

  if (p.isInt()) {
    switch (op) {
    case kOpEqual:      result = p.intValue() == 0; break;
    case kOpUnequal:    result = p.intValue() != 0; break;
    case kOpGreater:    result = p.intValue() > 0; break;
    case kOpGreaterEqual: result = p.intValue() >= 0; break;
    case kOpLess:         result = p.intValue() < 0; break;
    case kOpLessEqual:    result = p.intValue() <= 0; break;
    default:
      assert(0);
    }
  }

  return Token(p.srcpos(), kBool, result);
}


Token
TokenEvalContext::evalAnd(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  if (left.isBool()) {
    if (left.boolValue()) {
      Token right = evalToken(rexpr);
      if (right.isBool()) {
        return Token(left.srcpos(), kBool, right.boolValue());
      }

      throw BadExpressionException(fromInt(__LINE__));
    }

    return Token(left.srcpos(), kBool, false);
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalOr(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  if (left.isBool()) {
    if (left.boolValue())
      return Token(left.srcpos(), kBool, true);

    Token right = evalToken(rexpr);
    if (right.isBool())
      return right;

    throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalAppend(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isString() && right.isString()) {
    return Token(left.srcpos(),
                 kString, left.stringValue() + right.stringValue());
  }
  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalBinaryToken(const Token& lexpr,
                                  OperatorType op,
                                  const Token& rexpr) const
{
  switch (op) {
  case kOpPlus:
    return evalAdd(lexpr, rexpr);
  case kOpMinus:
    return evalMinus(lexpr, rexpr);
  case kOpMultiply:
    return evalMultiply(lexpr, rexpr);
  case kOpDivide:
    return evalDivide(lexpr, rexpr);
  case kOpMod:
    return evalModulo(lexpr, rexpr);
  case kOpExponent:
    return evalExponent(lexpr, rexpr);

  case kOpBitAnd:
  case kOpBitOr:
  case kOpBitXor:
  case kOpShiftLeft:
  case kOpShiftRight:
    return evalBitOp(lexpr, rexpr, op);

  case kOpCompare:
    return evalCompare(lexpr, rexpr);
  case kOpEqual:
  case kOpUnequal:
  case kOpGreater:
  case kOpGreaterEqual:
  case kOpLess:
  case kOpLessEqual:
    return evalLogical(lexpr, rexpr, op);

  case kOpLogicalAnd:
    return evalAnd(lexpr, rexpr);
  case kOpLogicalOr:
    return evalOr(lexpr, rexpr);

  case kOpAppend:
    return evalAppend(lexpr, rexpr);

  case kOpRange:
    // TODO
  case kOpBy:
    // TODO
    throw BadExpressionException(fromInt(__LINE__));


  case kOpIn:
    // TODO

  case kOpFold:
    // TODO

  case kOpAs:
    // TODO
  case kOpIsa:
    // TODO

  case kOpMapTo:
  default:
    throw BadExpressionException(fromInt(__LINE__));
  }
}


Token
TokenEvalContext::evalToken(const Token& expr) const
{
  switch(expr.type()) {
  case kSeq:
    if (expr.isBinarySeq()) {
      return evalBinaryToken(expr[0], expr.binarySeqOperator(), expr[2]);
    }
    else
      throw BadExpressionException(fromInt(__LINE__));

  case kNested:
    // TODO
    throw NotSupportedException(__FUNCTION__);

  case kPunct:
    throw BadExpressionException(expr.toString());

  case kLit:
    switch (expr.tokenType()) {
    case kString:
    case kKeyword:
    case kChar:
    case kBool:
    case kInt:
    case kReal:
    case kRational:
      return expr;
    default:
      throw BadExpressionException(expr.toString());
    }
    break;

  case kId:
    {
      Token value;
      if (!fRegistry->lookup(expr.idValue(), &value))
        throw UndefinedSymbolException(expr.idValue());
      return value;
    }
    break;
  }

  return Token();
}





#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>

struct TokenEvalContextFixture
{
  TokenEvalContextFixture()
    : reg(new ConfigVarRegistry),
      ctx(reg)
  {
  }

  ~TokenEvalContextFixture() {
  }

  Ptr<ConfigVarRegistry> reg;
  TokenEvalContext ctx;
  SrcPos sp;
  Token t;
};


SUITE(TokenEvalContext)
{
  TEST_FIXTURE(TokenEvalContextFixture, Basic)
  {
    t = ctx.evalToken(Token(sp, kInt, 25));
    CHECK(t.isInt() && t.intValue() == 25);

    t = ctx.evalToken(Token(sp, kReal, 3.1415));
    CHECK(t.isReal() && t.realValue() == 3.1415);

    t = ctx.evalToken(Token(sp, kRational, Rational(101, 127)));
    CHECK(t.isRational() && t.rationalValue() == Rational(101, 127));

    t = ctx.evalToken(Token(sp, kString, "hello world"));
    CHECK(t.isString() && t.stringValue() == String("hello world"));

    t = ctx.evalToken(Token(sp, kChar, 0xac00));
    CHECK(t.isChar() && t.charValue() == Char(0xac00));
  }


#define MAKE_BINARY_SEQ(_ltype, _lvalue, _op, _rtype, _rvalue)  \
    Token() << Token(sp, _ltype, _lvalue)                       \
            << Token(sp, _op)                                   \
            << Token(sp, _rtype, _rvalue)

  TEST_FIXTURE(TokenEvalContextFixture, Add)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kPlus, kInt, 17));
    CHECK(t.isInt() && t.intValue() == 42);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kPlus, kReal, 3.1415));
    CHECK(t.isReal() && t.realValue() == 28.1415);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kPlus, kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(103, 4));


    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kPlus, kInt, 25));
    CHECK(t.isReal() && t.realValue() == 28.1415);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kPlus, kReal, 3.1415));
    CHECK(t.isReal() && int(t.realValue() * 1000) == 6283);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kPlus, kRational, Rational(3, 4)));
    CHECK(t.isReal() && t.realValue() == 3.8915);


    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus, kInt, 25));
    CHECK(t.isRational() && t.rationalValue() == Rational(103, 4));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus, kReal, 3.1415));
    CHECK(t.isReal() && t.realValue() == 3.8915);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus,
                                      kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(6, 4));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus,
                                      kRational, Rational(2, 5)));
    CHECK(t.isRational() && t.rationalValue() == Rational(23, 20));
  }


  TEST_FIXTURE(TokenEvalContextFixture, Minus)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMinus, kInt, 17));
    CHECK(t.isInt() && t.intValue() == 8);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMinus, kReal, 3.1415));
    CHECK(t.isReal() && t.realValue() == 21.8585);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMinus, kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(97, 4));


    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kMinus, kInt, 25));
    CHECK(t.isReal() && t.realValue() == -21.8585);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kMinus, kReal, 3.1415));
    CHECK(t.isReal() && int(t.realValue() * 1000) == 0.0);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kMinus, kRational, Rational(3, 4)));
    CHECK(t.isReal() && t.realValue() == 2.3915);


    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus, kInt, 25));
    CHECK(t.isRational() && t.rationalValue() == Rational(-97, 4));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus, kReal, 3.1415));
    CHECK(t.isReal() && t.realValue() == -2.3915);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus,
                                      kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(0, 4));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus,
                                      kRational, Rational(2, 5)));
    CHECK(t.isRational() && t.rationalValue() == Rational(7, 20));
  }


  TEST_FIXTURE(TokenEvalContextFixture, Multiply)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMultiply, kInt, 17));
    CHECK(t.isInt() && t.intValue() == 425);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMultiply, kReal, 3.1415));
    CHECK(t.isReal() && int(t.realValue() * 10000) == 785375);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMultiply, kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(75, 4));


    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kMultiply, kInt, 25));
    CHECK(t.isReal() && int(t.realValue() * 10000) == 785375);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kMultiply, kReal, 3.1415));
    CHECK(t.isReal() && int(t.realValue() * 100000000) == 986902225);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kMultiply, kRational, Rational(3, 4)));
    CHECK(t.isReal() && int(t.realValue() * 1000000) == 2356125);


    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply, kInt, 25));
    CHECK(t.isRational() && t.rationalValue() == Rational(75, 4));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply, kReal, 3.1415));
    CHECK(t.isReal() && int(t.realValue() * 1000000) == 2356125);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply,
                                      kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(9, 16));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply,
                                      kRational, Rational(2, 5)));
    CHECK(t.isRational() && t.rationalValue() == Rational(6, 20));
  }


  TEST_FIXTURE(TokenEvalContextFixture, Divide)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kDivide, kInt, 17));
    CHECK(t.isInt() && t.intValue() == 1);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kDivide, kReal, 3.1415));
    CHECK(t.isReal() && int(t.realValue() * 100000000) == 795798185);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kDivide, kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(100, 3));


    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kDivide, kInt, 25));
    CHECK(t.isReal() && int(t.realValue() * 100000) == 12566);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kDivide, kReal, 3.1415));
    CHECK(t.isReal() && t.realValue() == 1.0);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kDivide, kRational, Rational(3, 4)));
    CHECK(t.isReal() && int(t.realValue() * 100000000) == 418866666);


    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide, kInt, 25));
    CHECK(t.isRational() && t.rationalValue() == Rational(3, 100));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide, kReal, 3.1415));
    CHECK(t.isReal() && int(t.realValue() * 100000000) == 23873945);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide,
                                      kRational, Rational(3, 4)));
    CHECK(t.isRational() && t.rationalValue() == Rational(12, 12));

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide,
                                      kRational, Rational(2, 5)));
    CHECK(t.isRational() && t.rationalValue() == Rational(15, 8));
  }


  TEST_FIXTURE(TokenEvalContextFixture, Modulo)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kMod, kInt, 3));
    CHECK(t.isInt() && t.intValue() == 1);
  }


  TEST_FIXTURE(TokenEvalContextFixture, Exponent)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 2, kExponent, kInt, 16));
    CHECK(t.isInt() && t.intValue() == 65536);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kReal, 3.1415, kExponent, kInt, 4));
    CHECK(t.isReal() && int(t.realValue() * 10000) == 973976);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kExponent,
                                      kInt, 4));
    CHECK(t.isRational() && t.rationalValue() == Rational(81, 256));
  }


  TEST_FIXTURE(TokenEvalContextFixture, BitAND)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xacde, kBitAnd, kInt, 0x0ff0));
    CHECK(t.isInt() && t.intValue() == 0x0cd0);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xacde, kBitAnd, kInt, 0xf0f0));
    CHECK(t.isInt() && t.intValue() == 0xa0d0);
  }


  TEST_FIXTURE(TokenEvalContextFixture, BitOR)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xa0d0, kBitOr, kInt, 0x0c0e));
    CHECK(t.isInt() && t.intValue() == 0xacde);
  }


  TEST_FIXTURE(TokenEvalContextFixture, BitXOR)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xacde, kBitXor, kInt, 0xffff));
    CHECK(t.isInt() && t.intValue() == 0x5321);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0x0000, kBitXor, kInt, 0xfefe));
    CHECK(t.isInt() && t.intValue() == 0xfefe);
  }


  TEST_FIXTURE(TokenEvalContextFixture, ShiftLeft)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0xabcd, kShiftLeft, kInt, 3));
    CHECK(t.isInt() && t.intValue() == 0x55e68);
  }


  TEST_FIXTURE(TokenEvalContextFixture, ShiftRight)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 0x55e68, kShiftRight, kInt, 3));
    CHECK(t.isInt() && t.intValue() == 0xabcd);
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpCompare)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kCompare, kInt, 17));
    CHECK(t.isInt() && t.intValue() > 0);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 17, kCompare, kInt, 25));
    CHECK(t.isInt() && t.intValue() < 0);

    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 17, kCompare, kInt, 17));
    CHECK(t.isInt() && t.intValue() == 0);
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpEqual)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kEqual, kInt, 17));
    CHECK(t.isBool() && !t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kEqual, kInt, 25));
    CHECK(t.isBool() && t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpUnequal)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kUnequal, kInt, 17));
    CHECK(t.isBool() && t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kUnequal, kInt, 25));
    CHECK(t.isBool() && !t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpGreater)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreater, kInt, 17));
    CHECK(t.isBool() && t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreater, kInt, 25));
    CHECK(t.isBool() && !t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpGreaterEqual)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreaterEqual, kInt, 17));
    CHECK(t.isBool() && t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kGreaterEqual, kInt, 25));
    CHECK(t.isBool() && t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpLess)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLess, kInt, 17));
    CHECK(t.isBool() && !t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLess, kInt, 25));
    CHECK(t.isBool() && !t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpLessEqual)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLessEqual, kInt, 17));
    CHECK(t.isBool() && !t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kInt, 25, kLessEqual, kInt, 25));
    CHECK(t.isBool() && t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpLogicalAnd)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalAnd, kBool, true));
    CHECK(t.isBool() && t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalAnd, kBool, false));
    CHECK(t.isBool() && !t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalAnd, kBool, true));
    CHECK(t.isBool() && !t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalAnd, kBool, false));
    CHECK(t.isBool() && !t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpLogicalOr)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalOr, kBool, true));
    CHECK(t.isBool() && t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, true, kLogicalOr, kBool, false));
    CHECK(t.isBool() && t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalOr, kBool, true));
    CHECK(t.isBool() && t.boolValue());
    t = ctx.evalToken(MAKE_BINARY_SEQ(kBool, false, kLogicalOr, kBool, false));
    CHECK(t.isBool() && !t.boolValue());
  }


  TEST_FIXTURE(TokenEvalContextFixture, OpAppend)
  {
    t = ctx.evalToken(MAKE_BINARY_SEQ(kString, String("hello "), kAppend,
                                      kString, String("world")));
    CHECK(t.isString() && t.stringValue() == String("hello world"));
  }
}

#endif  // #if defined(UNITTESTS)
