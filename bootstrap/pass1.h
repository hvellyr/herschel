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
    Pexpr parseExprRec(const Pexpr& expr1, OperatorType op1);

    Pexpr parseTypeSpec();
    Pexpr parseLiteralVector();
    Pexpr parseLiteralArray();

    Pexpr parseIf();
    Pexpr parseOn();

    Pexpr parseGroup();
    Pexpr parseBlock();
    Pexpr parseAnonFun();

    Pexpr parseAccess(const Pexpr& expr);
    Pexpr parseSlice(const Pexpr& expr);

    Pexpr parseParamCall(const Pexpr& expr,
                         const std::vector<Pexpr>& preScannedArgs,
                         bool parseParams);
    Pexpr parseFunctionCall(const Pexpr& expr,
                            const std::vector<Pexpr>& preScannedArgs,
                            bool parseParams);
    void parseFuncallParams(std::vector<Pexpr>* params);

    OperatorType tokenTypeToOperator(TokenType type) const;
    TokenType operatorToTokenType(OperatorType op) const;

    Pexpr makeBinaryPexpr(const Pexpr& expr1, OperatorType op1,
                          const Pexpr& expr2) const;
    Pexpr makeAssignPexpr(const Pexpr& expr1, const Pexpr& expr2) const;

    bool isRightOperator(OperatorType op1) const;
    bool isOpWeightAbove(OperatorType op1, OperatorType op2) const;
    int weightOperator(OperatorType op1) const;

    void parseExprListUntilBrace(std::vector<Pexpr>* exprlist);
    void parseFunctionsParams(std::vector<Pexpr>* exprlist);


  private:
    Ptr<Parser> fParser;
    Token       fToken;
  };
};

#endif  // bootstrap_pass1_h
