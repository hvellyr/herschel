/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass1_h
#define bootstrap_pass1_h

#include "apt.h"
#include "errcodes.h"
#include "macro.h"
#include "parser.h"
#include "port.h"
#include "refcountable.h"
#include "token.h"
#include "tokenport.h"


namespace heather
{
  //--------------------------------------------------------------------------

  class FirstPass
  {
  public:
    FirstPass(Parser* parser, const Token& currentToken);

    Token nextToken();
    void unreadToken(const Token& token);

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
    friend struct BasePatternParser;
    friend struct ForClauseParser;
    friend struct EnumItemParser;

    friend struct ExprParamSyntaxMatcher;
    friend struct NameParamSyntaxMatcher;
    friend struct ParamParamSyntaxMatcher;
    friend struct ParamListParamSyntax;

    enum ScopeType {
      kNonScopedDef,
      kInTypeDef,
      kInClassDef
    };

    enum ParamType {
      kPositional,
      kNamed,
      kRest
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
    Token parseOn(ScopeType scopeType);

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

    bool scanBlock(bool isTopLevel, ScopeType scope);

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

    Token parseOptDocString();

    Token parseMacroDef(const Token& defToken);
    bool parseMacroPatterns(MacroPatternVector* patterns);
    bool parseMacroComponent(TokenVector* component);

    MacroType dertermineMacroPatternType(const Token& macroName,
                                         const SrcPos& patternPos,
                                         const TokenVector& pattern);
    MacroType determineMacroType(const Token& macroName,
                                 const MacroPatternVector& patterns);

    String qualifiedIdForLookup(const String& id) const;

    //-------- macro calls

    Token parseMakeMacroCall(const Token& expr, const TokenVector& args,
                             Macro* macro,
                             bool shouldParseParams,
                             bool isLocal,
                             ScopeType scopeType);

    bool parseDoMatchSyntaxDef(TokenVector* result,
                               const Token& expr, SyntaxTable* syntaxTable,
                               bool isLocal);
    bool parseDoMatchSyntaxOn(TokenVector* filtered,
                              const Token& expr, SyntaxTable* syntaxTable,
                              bool isLocal);
    bool parseDoMatchSyntaxFunc(TokenVector* filtered,
                                const Token& expr,
                                const TokenVector& args,
                                SyntaxTable* syntaxTable,
                                bool shouldParseParams);

    bool parseExprStream(TokenVector* result, bool isTopLevel,
                         ScopeType scopeType);

    bool matchSyntax(TokenVector* result, SyntaxTable* syntaxTable);
    bool replaceMatchBindings(TokenVector* result,
                              const TokenVector& replacement,
                              const std::map<String, Token>& bindings);
    bool replaceSangHashIds(TokenVector* result, const TokenVector& source);
    Token findReplaceToken(const Token& token,
                           const std::map<String, Token>& bindings);

    bool matchParameter(const String& paramName,
                        MacroParamType type,
                        std::map<String, Token>* bindings);

    Token parseParameter(ParamType* expected, bool autoCompleteTypes);

    //-------- data members

    Ptr<Parser> fParser;
    Token       fToken;
    bool        fEvaluateExprs;
  };
};

#endif  // bootstrap_pass1_h
