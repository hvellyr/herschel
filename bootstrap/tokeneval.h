/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "token.h"
#include "compiler.h"
#include "ptr.h"


namespace herschel
{
  //--------------------------------------------------------------------------

  class BadExpressionException : public Exception
  {
  public:
    BadExpressionException(const String& details)
      : Exception(String("Bad expression: ") + details)
    { }
  };


  //--------------------------------------------------------------------------

  class DivisionByZeroException : public Exception
  {
  public:
    DivisionByZeroException()
      : Exception(String("Division by zero"))
    { }
  };


  //--------------------------------------------------------------------------

  class TokenEvalContext
  {
  public:
    TokenEvalContext(ConfigVarRegistry& registry);

    Token evalToken(const Token& expr) const;

  private:
    Token evalAdd(const Token& lexpr, const Token& rexpr) const;
    Token evalMinus(const Token& lexpr, const Token& rexpr) const;
    Token evalMultiply(const Token& lexpr, const Token& rexpr) const;
    Token evalDivide(const Token& lexpr, const Token& rexpr) const;
    Token evalModulo(const Token& lexpr, const Token& rexpr) const;
    Token evalRemainder(const Token& lexpr, const Token& rexpr) const;
    Token evalExponent(const Token& lexpr, const Token& rexpr) const;

    Token evalBitOp(const Token& lexpr, const Token& rexpr,
                    OperatorType op) const;

    Token evalCompare(const Token& lexpr, const Token& rexpr) const;
    Token evalLogical(const Token& lexpr, const Token& rexpr,
                      OperatorType op) const;

    Token evalAnd(const Token& lexpr, const Token& rexpr) const;
    Token evalOr(const Token& lexpr, const Token& rexpr) const;

    Token evalConcat(const Token& lexpr, const Token& rexpr) const;

    Token evalBinaryToken(const Token& lexpr,
                          OperatorType op,
                          const Token& rexpr) const;

    //-------- data members
    ConfigVarRegistry& fRegistry;
  };
};

