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
  class MacroPattern
  {
  public:
    MacroPattern(const TokenVector& pattern, const TokenVector& replc)
      : fPattern(pattern),
        fReplacement(replc)
    { }

    MacroPattern(const MacroPattern& other)
    {
      *this = other;
    }


    MacroPattern& operator=(const MacroPattern& other)
    {
      fPattern = other.fPattern;
      fReplacement = other.fReplacement;
      return *this;
    }

    TokenVector fPattern;
    TokenVector fReplacement;
  };

  typedef std::vector<MacroPattern> MacroPatternVector;


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
    friend struct EnumItemParser;

    enum ScopeType {
      kNonScopedDef,
      kInTypeDef,
      kInClassDef
    };

    Token parseTop(ScopeType scope);
    Token parseModule(bool isModule);
    Token parseExport();
    Token parseImport();
    Token parseDef(bool isLocal, ScopeType scope);
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
    Token parseUnitNumber(const Token& token);
    Token parseExplicitTypedNumber(const Token& token);
    Token parseExprRec(const Token& expr1, OperatorType op1,
                       const SrcPos& op1Srcpos);

    Token parseTypeSpec(bool onlyNestedConstraints);
    Token parseSimpleType(const Token& baseType, bool nextIsParsedYet = false);
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
    void parseTopExprUntilBrace(TokenVector* result, ScopeType scope);
    Token parseTopOrExprList(bool isTopLevel, ScopeType scope);

    bool parseFunctionsParams(TokenVector* exprlist,
                              bool autoCompleteType = true,
                              bool exceptEmptyList = true);

    Token parseWhen(bool isTopLevel, ScopeType scope);
    Token parseExtend(ScopeType scope);


    Token evaluateConfigExpr(const Token& initExpr);

    Token parseWhereClause();
    Token parseReifyClause();

    Token parseMeasure(const Token& defToken, bool isLocal);
    Token parseUnit(const Token& defToken, bool isLocal);
    Token parseEnumDef(const Token& defToken, bool isLocal);


    // resume functions after (syntax) error
    Token scanUntilTopExprAndResume();
    Token scanUntilNextParameter(TokenType endTokenType = kParanClose);
    Token scanUntilBrace();
    Token scanUntilEndOfParameters();


    Token parseGenericFunctionDef(const Token& defToken, bool isLocal);

    Token parseUnaryOp(const Token& inOpToken);

    Token parseAliasDef(const Token& defToken, bool isLocal);
    Token parseTypeDef(const Token& defToken, bool isClass, bool isLocal);
    Token parseSlotDef(const Token& defToken);

    template<typename ParseFunctor>
    void parseSequence(ParseFunctor functor,
                       TokenType startToken, TokenType endToken,
                       bool hasSeparator,
                       ErrCodes errorCode,
                       Token& result,
                       const char* ctx,
                       bool skipFirst = true);

    bool isConstraintOperator(const Token& token) const;


    Token parseMacroDef(const Token& defToken);
    bool parseMacroPatterns(MacroPatternVector* patterns);
    bool parseMacroComponent(TokenVector* component);

    //-------- data members
    Ptr<Parser> fParser;
    Token       fToken;
    bool        fEvaluateExprs;
  };
};

#endif  // bootstrap_pass1_h
