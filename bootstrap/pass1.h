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
#include "errcodes.h"


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

  private:
    friend struct ModuleParser;
    friend struct ExportParser;
    friend struct ImportRenameParser;
    friend struct LiteralVectorParser;
    friend struct LiteralArrayParser;
    friend struct ParseFuncParamsParser;
    friend struct TypeParser;
    friend struct FuncallArgsParser;
    friend struct SelectPatternParser;
    friend struct MatchPatternParser;
    friend class BasePatternParser;
    friend struct ForClauseParser;

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
                           const Token& symToken, bool isLocal);
    Token parseFunctionOrVarDef(const Token& defToken, bool isLocal);
    Token parseSelect();
    Token parseMatch();
    Token parseFor();

    Token parseExpr();
    Token parseAtomicExpr();
    Token parseExprRec(const Token& expr1, OperatorType op1,
                       const SrcPos& op1Srcpos);

    Token parseTypeSpec(bool onlyNestedConstraints);
    Token parseSimpleType();
    Token parseUnionType();
    Token parseGroupType();
    Token parseFunctionType();
    Token parseFunctionSignature();
    Token parseQuotedType();
    Token parseArrayExtend(const Token& baseType);
    Token parseConstraintExtend(const Token& baseType);

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
    void parseFuncallArgs(TokenVector* args);

    Token makeBinaryToken(const Token& expr1, OperatorType op1,
                          const Token& expr2,
                          const SrcPos& op1Srcpos) const;
    Token makeAssignToken(const Token& expr1, const Token& expr2,
                          const SrcPos& op1Srcpos) const;

    bool isRightOperator(OperatorType op1) const;
    bool isOpWeightAbove(OperatorType op1, OperatorType op2) const;
    int weightOperator(OperatorType op1) const;

    bool parseExprListUntilBrace(TokenVector* exprlist,
                                 bool endAtToplevelId,
                                 bool isLocal);
    void parseTopExprUntilBrace(TokenVector* result);
    Token parseTopOrExprList(bool isTopLevel);

    bool parseFunctionsParams(TokenVector* exprlist);

    Token parseWhen(bool isTopLevel);
    Token parseExtend();


    Token evaluateConfigExpr(const Token& initExpr);

    Token parseWhereClause();
    Token parseReifyClause();

    // resume functions after (syntax) error
    Token scanUntilTopExprAndResume();
    Token scanUntilNextParameter();
    Token scanUntilBrace();


    Token parseGenericFunctionDef(const Token& defToken, bool isLocal);

    Token parseUnaryOp(const Token& inOpToken);

    Token parseAliasDef(const Token& defToken, bool isLocal);

    template<typename ParseFunctor>
    void parseSequence(ParseFunctor functor,
                       TokenType startToken, TokenType endToken,
                       bool hasSeparator,
                       ErrCodes errorCode,
                       Token& result,
                       const char* ctx,
                       bool skipFirst = true);

    bool isConstraintOperator(const Token& token) const;

    //-------- data members
    Ptr<Parser> fParser;
    Token       fToken;
    bool        fEvaluateExprs;
  };
};

#endif  // bootstrap_pass1_h
