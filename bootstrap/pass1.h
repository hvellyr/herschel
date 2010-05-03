/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass1_h
#define bootstrap_pass1_h

#include "refcountable.h"
#include "apt.h"
#include "port.h"
#include "tokenport.h"
#include "pexpr.h"
#include "parser.h"


namespace heather
{
  //--------------------------------------------------------------------------

  class FirstPass
  {
  public:
    FirstPass(Parser* parser, Token currentToken);

    Token nextToken();

    //! first pass parse
    Pexpr parse();

    Pexpr parseTop();
    Pexpr parseModule(bool isModule);
    Pexpr parseExport();
    Pexpr parseImport();
    Pexpr parseDef();
    void parseCharDef();
    Pexpr parseVarDef(VardefFlags flags);
    Pexpr parseExpr();
    Pexpr parseAtomicExpr();
    Pexpr parseTypeSpec();
    Pexpr parseLiteralVector();
    Pexpr parseLiteralArray();

  private:
    Ptr<Parser> fParser;
    Token       fToken;
  };
};

#endif  // bootstrap_pass1_h
