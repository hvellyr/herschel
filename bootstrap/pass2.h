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
    AptNode* parseExport(const Pexpr& expr);
    AptNode* parseImport(const Pexpr& expr);


    void parseTopExprlist(AptNode* rootNode, const Pexpr& expr);

    //-------- data member
    Ptr<Parser> fParser;
    std::list<Ptr<AptNode> > fLastModules;
  };
};

#endif  // bootstrap_pass2_h
