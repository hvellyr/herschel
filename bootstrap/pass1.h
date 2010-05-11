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
    FirstPass(Parser* parser, const Token& currentToken);

    Token nextToken();

    //! first pass parse
    Token parse();

    Token parseTop();
    Token parseModule(bool isModule);
    Token parseExport();
    Token parseImport();
    Token parseDef(bool isLocal);
    Token parseCharDef();
    Token parseVarDef(VardefFlags flags, bool isLocal);
    Token parseVarDef2(const String& symbolName, VardefFlags flags,
                       bool isLocal);
    Token parseFunctionDef(const String& sym, bool isGeneric, bool isLocal);
    Token parseFunctionOrVarDef(bool isLocal);

    Token parseExpr();
    Token parseAtomicExpr();
    Token parseExprRec(const Token& expr1, OperatorType op1);

    Token parseTypeSpec();
    Token parseLiteralVector();
    Token parseLiteralArray();

    Token parseIf();
    Token parseOn();

    Token parseGroup();
    Token parseBlock();
    Token parseAnonFun();

    Token parseAccess(const Token& expr);
    Token parseSlice(const Token& expr);

    Token parseParamCall(const Token& expr,
                         const TokenVector& preScannedArgs,
                         bool parseParams);
    Token parseFunctionCall(const Token& expr,
                            const TokenVector& preScannedArgs,
                            bool parseParams);
    void parseFuncallParams(TokenVector* params);

    Token makeBinaryToken(const Token& expr1, OperatorType op1,
                          const Token& expr2) const;
    Token makeAssignToken(const Token& expr1, const Token& expr2) const;

    bool isRightOperator(OperatorType op1) const;
    bool isOpWeightAbove(OperatorType op1, OperatorType op2) const;
    int weightOperator(OperatorType op1) const;

    void parseExprListUntilBrace(TokenVector* exprlist);
    void parseTopExprUntilBrace(TokenVector* result);
    Token parseTopOrExprList(bool isTopLevel);

    void parseFunctionsParams(TokenVector* exprlist);

    Token parseWhen(bool isTopLevel);

    Token evaluateConfigExpr(const Token& initExpr);

  private:
    Ptr<Parser> fParser;
    Token       fToken;
    bool        fEvaluateExprs;
  };
};

#endif  // bootstrap_pass1_h
