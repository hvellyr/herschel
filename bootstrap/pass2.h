/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass2_h
#define bootstrap_pass2_h

#include "refcountable.h"
#include "apt.h"
#include "port.h"
#include "tokenport.h"
#include "pexpr.h"
#include "parser.h"


namespace heather
{
  //--------------------------------------------------------------------------

  class SecondPass
  {
  public:
    SecondPass(Parser* parser);

    AptNode* parse(const Pexpr& exprs);

  private:
    AptNode* parseExpr(const Pexpr& expr);
    AptNode* parseSeq(const Pexpr& expr);

    AptNode* parseModule(const Pexpr& expr, bool isModule);

    //-------- data member
    Ptr<Parser> fParser;
  };
};

#endif  // bootstrap_pass2_h
