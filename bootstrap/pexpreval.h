/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pexpreval_h
#define bootstrap_pexpreval_h

#include "pexpr.h"
#include "parser.h"
#include "ptr.h"


namespace heather
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

  class PexprEvalContext
  {
  public:
    PexprEvalContext(ConfigVarRegistry* registry);

    Pexpr evalPexpr(const Pexpr& expr) const;

  private:
    Pexpr evalAdd(const Pexpr& lexpr, const Pexpr& rexpr) const;
    Pexpr evalCompare(const Pexpr& lexpr, const Pexpr& rexpr) const;
    Pexpr evalBinaryPexpr(const Pexpr& lexpr,
                          OperatorType op,
                          const Pexpr& rexpr) const;

    //-------- data members
    Ptr<ConfigVarRegistry> fRegistry;
  };
};

#endif  // bootstrap_pexpreval_h
