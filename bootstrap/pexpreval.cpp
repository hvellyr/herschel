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
    case kOpEqual:      result = p.intLitValue() == 0;
    case kOpUnequal:    result = p.intLitValue() != 0;
    case kOpGreater:    result = p.intLitValue() > 0;
    case kOpGreaterEqual: result = p.intLitValue() >= 0;
    case kOpLess:         result = p.intLitValue() < 0;
    case kOpLessEqual:    result = p.intLitValue() <= 0;
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
    assert(t.isRationalLit() && t.rationalLitValue() == Rational(24, 16));
  }
};

static PexpEvalContextUnitTest pexpEvalContextUnitTest;

#endif  // #if defined(UNITTESTS)
