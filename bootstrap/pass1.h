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
#include "token.h"
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
    Token parseCharDef(const Token& defToken);
    Token parseVarDef(const Token& defToken, const Token& tagToken,
                      bool isLocal);
    Token parseVarDef2(const Token& defToken, const Token& tagToken,
                       const Token& symbolToken, bool isLocal);
    Token parseFunctionDef(const Token& defToken, const Token& tagToken, 
                           const Token& symToken,
                           bool isGeneric, bool isLocal);
    Token parseFunctionOrVarDef(const Token& defToken, bool isLocal);

    Token parseExpr();
    Token parseAtomicExpr();
    Token parseExprRec(const Token& expr1, OperatorType op1,
                       const SrcPos& op1Srcpos);

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
                          const Token& expr2,
                          const SrcPos& op1Srcpos) const;
    Token makeAssignToken(const Token& expr1, const Token& expr2,
                          const SrcPos& op1Srcpos) const;

    bool isRightOperator(OperatorType op1) const;
    bool isOpWeightAbove(OperatorType op1, OperatorType op2) const;
    int weightOperator(OperatorType op1) const;

    void parseExprListUntilBrace(TokenVector* exprlist);
    void parseTopExprUntilBrace(TokenVector* result);
    Token parseTopOrExprList(bool isTopLevel);

    void parseFunctionsParams(TokenVector* exprlist);

    Token parseWhen(bool isTopLevel);

    Token evaluateConfigExpr(const Token& initExpr);


    // resume functions after (syntax) error
    Token scanUntilTopExprAndResume();

  private:
    Ptr<Parser> fParser;
    Token       fToken;
    bool        fEvaluateExprs;
  };
};

#endif  // bootstrap_pass1_h
