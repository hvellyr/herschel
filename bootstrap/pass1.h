/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass1_h
#define bootstrap_pass1_h

#include <map>

#include "apt.h"
#include "errcodes.h"
#include "macro.h"
#include "parser.h"
#include "port.h"
#include "refcountable.h"
#include "scope.h"
#include "token.h"
#include "tokenport.h"


namespace heather
{
  class SyntaxTreeNode;

  //--------------------------------------------------------------------------

  typedef std::map<String, TokenVector> NamedReplacementMap;


  //--------------------------------------------------------------------------

  class FirstPass
  {
  public:
    FirstPass(Parser* parser, const Token& currentToken, Scope* scope);

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
    friend struct AnyParamParamSyntaxMatcher;
    friend struct SpecParamParamSyntaxMatcher;
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
    Token parseModule();
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

    bool parseFunctionsParamsFull(TokenVector* exprlist,
                                  TokenType startToken, TokenType endToken,
                                  bool autoCompleteType,
                                  bool exceptEmptyList,
                                  bool skipFirst,
                                  bool eatLast);
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
                       bool skipFirst = true,
                       bool eatLast = true);

    bool isConstraintOperator(const Token& token) const;

    Token parseOptDocString();

    Token parseMacroDef(const Token& defToken);
    bool parseMacroPatterns(MacroPatternVector* patterns);
    bool parseMacroComponent(TokenVector* component,
                             TokenType beginTokenType, TokenType endTokenType);

    MacroType dertermineMacroPatternType(const Token& macroName,
                                         const SrcPos& patternPos,
                                         const TokenVector& pattern);
    MacroType determineMacroType(const Token& macroName,
                                 const MacroPatternVector& patterns);

    String qualifiedIdForLookup(const String& id) const;

    //-------- macro calls

    Token parseMakeMacroCall(const Token& expr, const TokenVector& args,
                             const Macro* macro,
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
                              const NamedReplacementMap& bindings);
    bool replaceSangHashIds(TokenVector* result, const TokenVector& source);
    const TokenVector& findReplaceToken(const Token& token,
                                        const NamedReplacementMap& bindings,
                                        bool& found);

    bool matchParameter(const Token& macroParam,
                        NamedReplacementMap* bindings,
                        SyntaxTreeNode* followSet);

    Token parseParameter(ParamType* expected, bool autoCompleteTypes);


    String currentModuleName() const;
    void pushModule(const String& name);
    void popModule();


    friend class ModuleHelper;
    class ModuleHelper
    {
    public:
      ModuleHelper(FirstPass* pass, const String& name)
        : fPass(pass)
      {
        fPass->pushModule(name);
      }

      ~ModuleHelper()
      {
        fPass->popModule();
      }

      FirstPass* fPass;
    };


    //-------- data members

    Ptr<Parser> fParser;
    Token       fToken;
    bool        fEvaluateExprs;
    Ptr<Scope>  fScope;

    String            fCurrentModuleName;
    std::list<String> fModuleNameStack;
  };
};

#endif  // bootstrap_pass1_h
