/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/


#include "pexpr.h"
#include "pexpreval.h"
#include "unittests.h"

using namespace heather;

PexprEvalContext::PexprEvalContext(ConfigVarRegistry* registry)
  : fRegistry(registry)
{
}


Pexpr
PexprEvalContext::evalAdd(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isIntLit()) {
    int value = left.intLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kInteger,
                         value + right.intLitValue()));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         double(value) + right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         Rational(value, 1) + right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRealLit()) {
    double value = left.realLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kReal,
                         value + double(right.intLitValue())));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value + right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kReal,
                         value + right.rationalLitValue().toReal()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRationalLit()) {
    Rational value = left.rationalLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kRational,
                         value + Rational(right.intLitValue(), 1)));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value.toReal() + right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         value + right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalMinus(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isIntLit()) {
    int value = left.intLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kInteger,
                         value - right.intLitValue()));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         double(value) - right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         Rational(value, 1) - right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRealLit()) {
    double value = left.realLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kReal,
                         value - double(right.intLitValue())));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value - right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kReal,
                         value - right.rationalLitValue().toReal()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRationalLit()) {
    Rational value = left.rationalLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kRational,
                         value - Rational(right.intLitValue(), 1)));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value.toReal() - right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         value - right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalMultiply(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isIntLit()) {
    int value = left.intLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kInteger,
                         value * right.intLitValue()));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         double(value) * right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         Rational(value, 1) * right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRealLit()) {
    double value = left.realLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kReal,
                         value * double(right.intLitValue())));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value * right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kReal,
                         value * right.rationalLitValue().toReal()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRationalLit()) {
    Rational value = left.rationalLitValue();
    if (right.isIntLit())
      return Pexpr(Token(kRational,
                         value * Rational(right.intLitValue(), 1)));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value.toReal() * right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         value * right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalDivide(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isIntLit()) {
    int value = left.intLitValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isIntLit())
      return Pexpr(Token(kInteger,
                         value / right.intLitValue()));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         double(value) / right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         Rational(value, 1) / right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRealLit()) {
    double value = left.realLitValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isIntLit())
      return Pexpr(Token(kReal,
                         value / double(right.intLitValue())));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value / right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kReal,
                         value / right.rationalLitValue().toReal()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRationalLit()) {
    Rational value = left.rationalLitValue();
    if (value.numerator() == 0)
      throw DivisionByZeroException();

    if (right.isIntLit())
      return Pexpr(Token(kRational,
                         value / Rational(right.intLitValue(), 1)));
    else if (right.isRealLit())
      return Pexpr(Token(kReal,
                         value.toReal() / right.realLitValue()));
    else if (right.isRationalLit())
      return Pexpr(Token(kRational,
                         value / right.rationalLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalModulo(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isIntLit()) {
    int value = left.intLitValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isIntLit())
      return Pexpr(Token(kInteger, value % right.intLitValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalExponent(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isIntLit()) {
    int value = left.intLitValue();

    if (right.isIntLit())
      return Pexpr(Token(kInteger,
                         heather::exponent(value, right.intLitValue())));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRealLit()) {
    double value = left.realLitValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isIntLit())
      return Pexpr(Token(kReal,
                         heather::exponent(value, right.intLitValue())));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRationalLit()) {
    Rational value = left.rationalLitValue();
    if (value.numerator() == 0)
      throw DivisionByZeroException();

    if (right.isIntLit())
      return Pexpr(Token(kRational,
                         value.exponent(right.intLitValue())));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalBitOp(const Pexpr& lexpr, const Pexpr& rexpr,
                            OperatorType op) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isIntLit() && right.isIntLit()) {
    int lvalue = left.intLitValue();
    int rvalue = right.intLitValue();

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
    return Pexpr(Token(kInteger, result));
  }
  
  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalCompare(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);

  if (left.isLit() && right.isLit() &&
      left.tokenValue().fType == right.tokenValue().fType) {
    switch (left.tokenValue().fType) {
    case kString:
      return Pexpr(Token(kInteger,
                         left.stringLitValue().compare(right.stringLitValue())));
    case kBool:
      if (left.boolLitValue() == right.boolLitValue())
        return Pexpr(Token(kInteger, 0));
      else
        return Pexpr(Token(kInteger, -1));
    case kInteger:
      return Pexpr(Token(kInteger,
                         left.intLitValue() - right.intLitValue()));
    case kReal:
      if (left.realLitValue() < right.realLitValue())
        return Pexpr(Token(kInteger, -1));
      else if (left.realLitValue() > right.realLitValue())
        return Pexpr(Token(kInteger, 1));
      else
        return Pexpr(Token(kInteger, 0));
    case kRational:
      if (left.rationalLitValue() < right.rationalLitValue())
        return Pexpr(Token(kInteger, -1));
      else if (left.rationalLitValue() > right.rationalLitValue())
        return Pexpr(Token(kInteger, 1));
      else
        return Pexpr(Token(kInteger, 0));
    case kChar:
      return Pexpr(Token(kInteger,
                         left.intLitValue() - right.intLitValue()));

    default:
      throw BadExpressionException(fromInt(__LINE__));
    }
  }

  return Pexpr(Token(kInteger, -1));
}


Pexpr
PexprEvalContext::evalLogical(const Pexpr& lexpr, const Pexpr& rexpr,
                              OperatorType op) const
{
  Pexpr p = evalCompare(lexpr, rexpr);
  bool result = false;

  if (p.isIntLit()) {
    switch (op) {
    case kOpEqual:      result = p.intLitValue() == 0; break;
    case kOpUnequal:    result = p.intLitValue() != 0; break;
    case kOpGreater:    result = p.intLitValue() > 0; break;
    case kOpGreaterEqual: result = p.intLitValue() >= 0; break;
    case kOpLess:         result = p.intLitValue() < 0; break;
    case kOpLessEqual:    result = p.intLitValue() <= 0; break;
    default:
      assert(0);
    }
  }

  return Pexpr(Token(kBool, result));
}


Pexpr
PexprEvalContext::evalAnd(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  if (left.isBoolLit()) {
    if (left.boolLitValue()) {
      Pexpr right = evalPexpr(rexpr);
      if (right.isBoolLit()) {
        return Pexpr(Token(kBool, right.boolLitValue()));
      }

      throw BadExpressionException(fromInt(__LINE__));
    }

    return Pexpr(Token(kBool, false));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalOr(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  if (left.isBoolLit()) {
    if (left.boolLitValue())
      return Pexpr(Token(kBool, true));

    Pexpr right = evalPexpr(rexpr);
    if (right.isBoolLit()) {
      return Pexpr(Token(kBool, right.boolLitValue()));
    }

    throw BadExpressionException(fromInt(__LINE__));
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalAppend(const Pexpr& lexpr, const Pexpr& rexpr) const
{
  Pexpr left = evalPexpr(lexpr);
  Pexpr right = evalPexpr(rexpr);
  
  if (left.isStringLit() && right.isStringLit()) {
    return Pexpr(Token(kString, left.stringLitValue() + right.stringLitValue()));
  }
  throw BadExpressionException(fromInt(__LINE__));
}


Pexpr
PexprEvalContext::evalBinaryPexpr(const Pexpr& lexpr,
                                  OperatorType op,
                                  const Pexpr& rexpr) const
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

  case kOpEllipsis:
    // TODO
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


Pexpr
PexprEvalContext::evalPexpr(const Pexpr& expr) const
{
  switch(expr.type()) {
  case kSeq:
    if (expr.isBinarySeq()) {
      return evalBinaryPexpr(expr[0], expr.binarySeqOperator(), expr[2]);
    }
    else
      throw BadExpressionException(fromInt(__LINE__));

  case kNested:
    // TODO
    throw NotSupportedException(__FUNCTION__);

  case kPunct:
    throw BadExpressionException(expr.tokenValue().toString());

  case kLit:
    switch (expr.tokenValue().fType) {
    case kString:
    case kChar:
    case kBool:
    case kInteger:
    case kReal:
    case kRational:
      return expr;
    default:
      throw BadExpressionException(expr.tokenValue().toString());
    }
    break;

  case kId:
    {
      Pexpr value;
      if (!fRegistry->lookup(expr.idValue(), &value))
        throw UndefinedSymbolException(expr.idValue());
      return value;
    }
    break;
  }

  return Pexpr();
}





#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class PexpEvalContextUnitTest : public UnitTest
{
public:
  PexpEvalContextUnitTest() : UnitTest("PexpEvalContext") {}

  virtual void run()
  {
    Ptr<ConfigVarRegistry> reg = new ConfigVarRegistry;
    PexprEvalContext ctx(reg);

    Pexpr t;

    t = ctx.evalPexpr(Pexpr(Token(kInteger, 25)));
    assert(t.isIntLit() && t.intLitValue() == 25);

    t = ctx.evalPexpr(Pexpr(Token(kReal, 3.1415)));
    assert(t.isRealLit() && t.realLitValue() == 3.1415);

    t = ctx.evalPexpr(Pexpr(Token(kRational, Rational(101, 127))));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(101, 127));

    t = ctx.evalPexpr(Pexpr(Token(kString, "hello world")));
    assert(t.isStringLit() && t.stringLitValue() == String("hello world"));

    t = ctx.evalPexpr(Pexpr(Token(kChar, 0xac00)));
    assert(t.isCharLit() && t.charLitValue() == Char(0xac00));


#define MAKE_BINARY_SEQ(_ltype, _lvalue, _op, _rtype, _rvalue)  \
    Pexpr() << Pexpr(Token(_ltype, _lvalue))                    \
            << Pexpr(_op)                                       \
            << Pexpr(Token(_rtype, _rvalue))

    //-------- test add

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kPlus, kInteger, 17));
    assert(t.isIntLit() && t.intLitValue() == 42);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kPlus, kReal, 3.1415));
    assert(t.isRealLit() && t.realLitValue() == 28.1415);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kPlus, kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(103, 4));


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kPlus, kInteger, 25));
    assert(t.isRealLit() && t.realLitValue() == 28.1415);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kPlus, kReal, 3.1415));
    assert(t.isRealLit() && int(t.realLitValue() * 1000) == 6283);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kPlus, kRational, Rational(3, 4)));
    assert(t.isRealLit() && t.realLitValue() == 3.8915);


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus, kInteger, 25));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(103, 4));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus, kReal, 3.1415));
    assert(t.isRealLit() && t.realLitValue() == 3.8915);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus,
                                      kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(6, 4));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kPlus,
                                      kRational, Rational(2, 5)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(23, 20));


    //-------- test minus

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kMinus, kInteger, 17));
    assert(t.isIntLit() && t.intLitValue() == 8);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kMinus, kReal, 3.1415));
    assert(t.isRealLit() && t.realLitValue() == 21.8585);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kMinus, kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(97, 4));


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kMinus, kInteger, 25));
    assert(t.isRealLit() && t.realLitValue() == -21.8585);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kMinus, kReal, 3.1415));
    assert(t.isRealLit() && int(t.realLitValue() * 1000) == 0.0);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kMinus, kRational, Rational(3, 4)));
    assert(t.isRealLit() && t.realLitValue() == 2.3915);


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus, kInteger, 25));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(-97, 4));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus, kReal, 3.1415));
    assert(t.isRealLit() && t.realLitValue() == -2.3915);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus,
                                      kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(0, 4));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMinus,
                                      kRational, Rational(2, 5)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(7, 20));

    //-------- test multiply

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kMultiply, kInteger, 17));
    assert(t.isIntLit() && t.intLitValue() == 425);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kMultiply, kReal, 3.1415));
    assert(t.isRealLit() && int(t.realLitValue() * 10000) == 785375);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kMultiply, kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(75, 4));


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kMultiply, kInteger, 25));
    assert(t.isRealLit() && int(t.realLitValue() * 10000) == 785375);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kMultiply, kReal, 3.1415));
    assert(t.isRealLit() && int(t.realLitValue() * 100000000) == 986902225);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kMultiply, kRational, Rational(3, 4)));
    assert(t.isRealLit() && int(t.realLitValue() * 1000000) == 2356125);


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply, kInteger, 25));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(75, 4));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply, kReal, 3.1415));
    assert(t.isRealLit() && int(t.realLitValue() * 1000000) == 2356125);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply,
                                      kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(9, 16));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kMultiply,
                                      kRational, Rational(2, 5)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(6, 20));

    //-------- test divide

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kDivide, kInteger, 17));
    assert(t.isIntLit() && t.intLitValue() == 1);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kDivide, kReal, 3.1415));
    assert(t.isRealLit() && int(t.realLitValue() * 100000000) == 795798185);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kDivide, kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(100, 3));


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kDivide, kInteger, 25));
    assert(t.isRealLit() && int(t.realLitValue() * 100000) == 12566);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kDivide, kReal, 3.1415));
    assert(t.isRealLit() && t.realLitValue() == 1.0);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kDivide, kRational, Rational(3, 4)));
    assert(t.isRealLit() && int(t.realLitValue() * 100000000) == 418866666);


    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide, kInteger, 25));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(3, 100));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide, kReal, 3.1415));
    assert(t.isRealLit() && int(t.realLitValue() * 100000000) == 23873945);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide,
                                      kRational, Rational(3, 4)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(12, 12));

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kDivide,
                                      kRational, Rational(2, 5)));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(15, 8));


    //-------- test modulo

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kMod, kInteger, 3));
    assert(t.isIntLit() && t.intLitValue() == 1);

    //-------- test exponent

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 2, kExponent, kInteger, 16));
    assert(t.isIntLit() && t.intLitValue() == 65536);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kReal, 3.1415, kExponent, kInteger, 4));
    assert(t.isRealLit() && int(t.realLitValue() * 10000) == 973976);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kRational, Rational(3, 4), kExponent,
                                      kInteger, 4));
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(81, 256));

    //-------- test bitAND

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 0xacde, kBitAnd, kInteger, 0x0ff0));
    assert(t.isIntLit() && t.intLitValue() == 0x0cd0);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 0xacde, kBitAnd, kInteger, 0xf0f0));
    assert(t.isIntLit() && t.intLitValue() == 0xa0d0);

    //-------- test bitOR

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 0xa0d0, kBitOr, kInteger, 0x0c0e));
    assert(t.isIntLit() && t.intLitValue() == 0xacde);

    //-------- test bitXOR

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 0xacde, kBitXor, kInteger, 0xffff));
    assert(t.isIntLit() && t.intLitValue() == 0x5321);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 0x0000, kBitXor, kInteger, 0xfefe));
    assert(t.isIntLit() && t.intLitValue() == 0xfefe);

    //-------- test shiftLeft

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 0xabcd, kShiftLeft, kInteger, 3));
    assert(t.isIntLit() && t.intLitValue() == 0x55e68);

    //-------- test shiftRight

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 0x55e68, kShiftRight, kInteger, 3));
    assert(t.isIntLit() && t.intLitValue() == 0xabcd);

    //-------- test kOpCompare:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kCompare, kInteger, 17));
    assert(t.isIntLit() && t.intLitValue() > 0);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 17, kCompare, kInteger, 25));
    assert(t.isIntLit() && t.intLitValue() < 0);

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 17, kCompare, kInteger, 17));
    assert(t.isIntLit() && t.intLitValue() == 0);

    //-------- test kOpEqual:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kEqual, kInteger, 17));
    assert(t.isBoolLit() && !t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kEqual, kInteger, 25));
    assert(t.isBoolLit() && t.boolLitValue());

    //-------- test kOpUnequal:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kUnequal, kInteger, 17));
    assert(t.isBoolLit() && t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kUnequal, kInteger, 25));
    assert(t.isBoolLit() && !t.boolLitValue());

    //-------- test kOpGreater:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kGreater, kInteger, 17));
    assert(t.isBoolLit() && t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kGreater, kInteger, 25));
    assert(t.isBoolLit() && !t.boolLitValue());

    //-------- test kOpGreaterEqual:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kGreaterEqual, kInteger, 17));
    assert(t.isBoolLit() && t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kGreaterEqual, kInteger, 25));
    assert(t.isBoolLit() && t.boolLitValue());

    //-------- test kOpLess:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kLess, kInteger, 17));
    assert(t.isBoolLit() && !t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kLess, kInteger, 25));
    assert(t.isBoolLit() && !t.boolLitValue());

    //-------- test kOpLessEqual:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kLessEqual, kInteger, 17));
    assert(t.isBoolLit() && !t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kInteger, 25, kLessEqual, kInteger, 25));
    assert(t.isBoolLit() && t.boolLitValue());

    //-------- test kOpLogicalAnd:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, true, kLogicalAnd, kBool, true));
    assert(t.isBoolLit() && t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, true, kLogicalAnd, kBool, false));
    assert(t.isBoolLit() && !t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, false, kLogicalAnd, kBool, true));
    assert(t.isBoolLit() && !t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, false, kLogicalAnd, kBool, false));
    assert(t.isBoolLit() && !t.boolLitValue());

    //-------- test kOpLogicalOr:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, true, kLogicalOr, kBool, true));
    assert(t.isBoolLit() && t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, true, kLogicalOr, kBool, false));
    assert(t.isBoolLit() && t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, false, kLogicalOr, kBool, true));
    assert(t.isBoolLit() && t.boolLitValue());
    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kBool, false, kLogicalOr, kBool, false));
    assert(t.isBoolLit() && !t.boolLitValue());

    //-------- test kOpAppend:

    t = ctx.evalPexpr(MAKE_BINARY_SEQ(kString, String("hello "), kAppend,
                                      kString, String("world")));
    assert(t.isStringLit() && t.stringLitValue() == String("hello world"));

  }
};

static PexpEvalContextUnitTest pexpEvalContextUnitTest;

#endif  // #if defined(UNITTESTS)
