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
    Pexpr parseDef(bool isLocal);
    void parseCharDef();
    Pexpr parseVarDef(VardefFlags flags, bool isLocal);
    Pexpr parseVarDef2(const String& symbolName, VardefFlags flags,
                       bool isLocal);
    Pexpr parseFunction(const String& sym, bool isGeneric, bool isLocal);
    Pexpr parseFunctionOrVar(bool isLocal);

    Pexpr parseExpr();
    Pexpr parseAtomicExpr();
    Pexpr parseExprRec(const Pexpr& expr1, TokenType op1);

    Pexpr parseTypeSpec();
    Pexpr parseLiteralVector();
    Pexpr parseLiteralArray();

    Pexpr parseIf();
    Pexpr parseOn();
    Pexpr parseAccess(const Pexpr& expr);
    Pexpr parseGroup();
    Pexpr parseBlock();
    Pexpr parseAnonFun();


    TokenType mapOperator(const Token& token) const;
    Pexpr makeBinaryPexpr(const Pexpr& expr1, TokenType op1,
                          const Pexpr& expr2) const;
    Pexpr makeAssignPexpr(const Pexpr& expr1, const Pexpr& expr2) const;

    bool isRightOperator(TokenType op1) const;
    bool isOpWeightAbove(TokenType op1, TokenType op2) const;
    int weightOperator(TokenType op1) const;


  private:
    Ptr<Parser> fParser;
    Token       fToken;
  };
};

#endif  // bootstrap_pass1_h
