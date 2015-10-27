/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


#include "token.h"
#include "tokeneval.h"


using namespace herschel;

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
      return Token::newInt(left.srcpos(),
                   left.bitwidth(), value + right.intValue());
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, double(value) + right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) + right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isFloat()) {
    double value = left.floatValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kFloat, value + double(right.intValue()));
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value + right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kFloat, value + right.rationalValue().toFloat());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value + Rational(right.intValue(), 1));
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value.toFloat() + right.floatValue());
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
      return Token::newInt(left.srcpos(),
                   left.bitwidth(), value - right.intValue());
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, double(value) - right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) - right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isFloat()) {
    double value = left.floatValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kFloat, value - double(right.intValue()));
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value - right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kFloat, value - right.rationalValue().toFloat());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value - Rational(right.intValue(), 1));
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value.toFloat() - right.floatValue());
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
      return Token::newInt(left.srcpos(),
                           left.bitwidth(), value * right.intValue());
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, double(value) * right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) * right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isFloat()) {
    double value = left.floatValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kFloat, value * double(right.intValue()));
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value * right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kFloat, value * right.rationalValue().toFloat());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isRational()) {
    Rational value = left.rationalValue();
    if (right.isInt())
      return Token(left.srcpos(),
                   kRational, value * Rational(right.intValue(), 1));
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value.toFloat() * right.floatValue());
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
      return Token::newInt(left.srcpos(),
                           left.bitwidth(), value / right.intValue());
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, double(value) / right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kRational, Rational(value, 1) / right.rationalValue());
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isFloat()) {
    double value = left.floatValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kFloat, value / double(right.intValue()));
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value / right.floatValue());
    else if (right.isRational())
      return Token(left.srcpos(),
                   kFloat, value / right.rationalValue().toFloat());
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
    else if (right.isFloat())
      return Token(left.srcpos(),
                   kFloat, value.toFloat() / right.floatValue());
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
    if (right.isInt()) {
      int rvalue = right.intValue();

      if (rvalue == 0)
        throw DivisionByZeroException();

      int value = left.intValue() % rvalue;
      if ( (value > 0 && rvalue < 0) ||
           (value < 0 && rvalue > 0) )
        value = value + rvalue;
      return Token(left.srcpos(), kInt, value);
    }
  }

  throw BadExpressionException(fromInt(__LINE__));
}


Token
TokenEvalContext::evalRemainder(const Token& lexpr, const Token& rexpr) const
{
  Token left = evalToken(lexpr);
  Token right = evalToken(rexpr);

  if (left.isInt()) {
    if (right.isInt()) {
      if (right.intValue() == 0)
        throw DivisionByZeroException();

      return Token::newInt(left.srcpos(),
                           left.bitwidth(), left.intValue() % right.intValue());
    }
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
                   kInt, herschel::exponent(value, right.intValue()));
    else
      throw BadExpressionException(fromInt(__LINE__));
  }
  else if (left.isFloat()) {
    double value = left.floatValue();
    if (value == 0)
      throw DivisionByZeroException();

    if (right.isInt())
      return Token(left.srcpos(),
                   kFloat, herschel::exponent(value, right.intValue()));
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
      hr_invalid("");
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
      return Token::newInt(left.srcpos(),
                           left.bitwidth(), left.intValue() - right.intValue());
    case kFloat:
      if (left.floatValue() < right.floatValue())
        return Token(left.srcpos(), kInt, -1);
      else if (left.floatValue() > right.floatValue())
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
      return Token::newInt(left.srcpos(),
                           left.bitwidth(), left.intValue() - right.intValue());

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
      hr_invalid("");
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
TokenEvalContext::evalConcat(const Token& lexpr, const Token& rexpr) const
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
  case kOpRem:
    return evalRemainder(lexpr, rexpr);
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

  case kOpConcat:
    return evalConcat(lexpr, rexpr);

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
    case kFloat:
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
