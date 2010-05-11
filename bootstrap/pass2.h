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
#include "token.h"
#include "parser.h"


namespace heather
{
  //--------------------------------------------------------------------------

  class SecondPass
  {
  public:
    SecondPass(Parser* parser);

    AptNode* parse(const Token& exprs);

  private:
    AptNode* parseExpr(const Token& expr);
    AptNode* parseSeq(const Token& expr);

    AptNode* parseModule(const Token& expr, bool isModule);
    AptNode* parseExport(const Token& expr);
    AptNode* parseImport(const Token& expr);


    void parseTopExprlist(AptNode* rootNode, const Token& expr);

    //-------- data member
    Ptr<Parser> fParser;
    std::list<Ptr<AptNode> > fLastModules;
  };
};

#endif  // bootstrap_pass2_h
