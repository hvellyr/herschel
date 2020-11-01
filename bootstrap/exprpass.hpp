/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "abstrpass.hpp"
#include "ast.hpp"
#include "compilepass.hpp"
#include "compiler.hpp"
#include "errcodes.hpp"
#include "macro.hpp"
#include "port.hpp"
#include "scope.hpp"
#include "token.hpp"
#include "tokenport.hpp"

#include <map>


namespace herschel {
class SyntaxTreeNode;


using NamedReplacementMap = std::map<String, TokenVector>;


//! The first pass in compiling.  It read the raw token stream and digests
//! it into token expressions.  Macros definitions are identified and
//! registered and calls to macros are resolved in this pass.  The result is
//! a list of a few well defined token expressions, which are translated
//! into the \c AptNode tree by the \c SecondPass.
class FirstPass : public AbstractPass {
public:
  FirstPass(Compiler& compiler, const Token& currentToken, std::shared_ptr<Scope> scope);

  //! Get the next token from the input stream and return it.
  Token nextToken();

  //! Return the token which has been return last from \c nextToken().
  Token currentToken();

  //! Push back \c token into the input stream.  \c token will be the next
  //! token to returned by \c nextToken().
  void unreadToken(const Token& token);
  void unreadTokens(const TokenVector& tokens);

  //! Start the parse run.
  Token parse();

private:
  friend struct AppKeysParser;
  friend struct BasePatternParser;
  friend struct EnumItemParser;
  friend struct ExportParser;
  friend struct ForClauseParser;
  friend struct FuncallArgsParser;
  friend struct ImportParser;
  friend struct IncludeParser;
  friend struct LibraryParser;
  friend struct LiteralArrayParser;
  friend struct LiteralVectorParser;
  friend struct MatchPatternParser;
  friend struct ModuleParser;
  friend struct ParseFuncParamsParser;
  friend struct ParseRecordSlotsParser;
  friend struct SelectPatternParser;
  friend struct TypeParser;

  friend struct ExprParamSyntaxMatcher;
  friend struct NameParamSyntaxMatcher;
  friend struct OperatorParamSyntaxMatcher;
  friend struct AnyParamParamSyntaxMatcher;
  friend struct SpecParamParamSyntaxMatcher;
  friend struct ParamListParamSyntax;

  friend class ExternCParser;

  enum ParamType { kPositional, kNamed, kRest };

  TokenVector parseQualifiedName(bool acceptLeadingDot);

  TokenVector parseTop();
  Token parseLibrary();
  Token parseApplication();
  Token parseModule();
  Token parseExport();
  TokenVector parseImport();
  TokenVector parseInclude();
  TokenVector parseDef(bool isLocal);
  Token parseCharDef(const Token& defToken, const Token& vizToken, VizType vizType);
  TokenVector parseVarDef(const Token& defToken, const Token& vizToken,
                          const Token& tagToken, bool isLocal, VizType vizType);
  TokenVector parseVarDef2(const Token& defToken, const Token& vizToken,
                           const Token& tagToken, const Token& symbolToken, bool isLocal,
                           const Token& linkage);
  Token parseFunctionDef(const Token& defToken, const Token& vizToken,
                         const Token& tagToken, const Token& symToken,
                         const Token& linkage);
  TokenVector parseFunctionOrVarDef(const Token& defToken, const Token& vizToken,
                                    bool isLocal, const Token& linkage, VizType vizType);
  Token parseSelect();
  Token parseMatch();
  Token parseFor();
  Token parseForTestClause();
  Token parseWhile();

  Token parseExpr(bool acceptComma);
  Token parseAtomicExpr0();
  Token parseAtomicExpr();
  Token parseUnitNumber(const Token& token);
  Token parseExplicitTypedNumber(const Token& token);
  Token parseExprRec(const TokenVector& exprs, OperatorType op1, const SrcPos& op1Srcpos,
                     bool hasRest);

  Token parseTypeSpec(bool onlyNestedConstraints, bool needParans);
  Token parseSymbolOrSimpleType(const Token& baseType);
  Token parseGroupType();
  Token parseFunctionType();
  Token parseFunctionSignature();
  Token parseQuotedType();
  Token parseArrayExtend(const Token& baseType);
  Token parseConstraintExtend(const Token& baseType);

  Token parseLiteralVector();
  Token parseLiteralArray();

  Token parseIf();

  Token parseGroup();
  Token parseBlock();
  Token parseAnonFun();

  Token parseAccess(const Token& expr);
  Token parseSlice(const Token& expr);

  Token parseParamCall(const Token& expr, const TokenVector& preScannedArgs,
                       bool parseParams);
  Token parseFunctionCall(const Token& expr, const TokenVector& preScannedArgs,
                          bool parseParams);
  void parseFuncallArgs(TokenVector* args);
  void parseAppArgs(TokenVector* argsVector);

  Token makeBinaryToken(const Token& expr1, OperatorType op1, const Token& expr2,
                        const SrcPos& op1Srcpos) const;
  Token makeAssignToken(const Token& exprs, const Token& expr2,
                        const SrcPos& op1Srcpos) const;
  Token makeAssignToken(const TokenVector& exprs, const Token& expr2,
                        const SrcPos& op1Srcpos, bool hasRest) const;

  bool isRightOperator(OperatorType op1) const;
  bool isOpWeightAbove(OperatorType op1, OperatorType op2) const;
  int weightOperator(OperatorType op1) const;

  bool parseExprListUntilBrace(TokenVector* exprlist, bool endAtToplevelId, bool isLocal);
  void parseTopExprUntilBrace(TokenVector* result);
  TokenVector parseTopOrExprList(bool isTopLevel);
  Token multiExprsToBlock(const TokenVector& exprs);
  Token wrapInBlock(const SrcPos& srcpos, const TokenVector& exprs);

  bool parseFunctionsParamsFull(TokenVector* exprlist, TokenType startToken,
                                TokenType endToken, bool autoCompleteType,
                                bool exceptEmptyList, bool skipFirst, bool eatLast);
  bool parseFunctionsParams(TokenVector* exprlist, bool autoCompleteType = true,
                            bool exceptEmptyList = true);

  bool parseRecordSlotsFull(TokenVector* exprlist, TokenType startToken,
                            TokenType endToken, bool skipFirst,
                            bool eatLast);
  bool parseRecordSlots(TokenVector* exprlist);

  Token parseWhen(bool isTopLevel);
  Token parseWith();

  TokenVector parseExtern();

  bool scanBlock(bool isTopLevel);

  Token evaluateConfigExpr(const Token& initExpr);

  Token parseWhereClause();

  Token parseEnumDef(const Token& defToken, const Token& vizToken, bool isLocal,
                     VizType vizType);


  //@{ resume functions after (syntax) error
  Token scanUntilTopExprAndResume();
  Token scanUntilNextParameter(TokenType endTokenType = kParanClose);
  Token scanUntilBrace();
  Token scanUntilEndOfParameters();
  //@}


  Token parseGenericFunctionDef(const Token& defToken, const Token& vizToken,
                                bool isLocal, VizType vizType);

  Token parseUnaryOp(const Token& inOpToken);

  Token parseAliasDef(const Token& defToken, const Token& vizToken, bool isLocal,
                      VizType vizType);
  Token parseTypeDef(const Token& defToken, const Token& vizToken, bool isRecord,
                     bool isLocal, VizType vizType);

  template <typename ParseFunctor>
  void parseSequence(ParseFunctor functor, TokenType startToken, TokenType endToken,
                     bool hasSeparator, ErrCodes errorCode, Token& result, zstring ctx,
                     bool skipFirst = true, bool eatLast = true);
  template <typename ParseFunctor>
  void parseChoiceSequence(ParseFunctor functor, TokenType choiceToken, Token& result);
  template <typename ParseFunctor>
  void parseSumType(ParseFunctor functor, ErrCodes errorCode, Token& result, zstring ctx);

  bool isConstraintOperator(const Token& token) const;

  Token parseOptDocString();

  Token parseMacroDef(const Token& defToken, VizType vizType);
  bool parseMacroPatterns(MacroPatternVector* patterns);
  bool parseMacroComponent(TokenVector* component, TokenType beginTokenType,
                           TokenType endTokenType);

  MacroType dertermineMacroPatternType(const Token& macroName, const SrcPos& patternPos,
                                       const TokenVector& pattern);
  MacroType determineMacroType(const Token& macroName,
                               const MacroPatternVector& patterns);

  //@{ Macro calls

  TokenVector parseMakeMacroCall(const Token& expr, const TokenVector& args,
                                 const Macro* macro, bool shouldParseParams,
                                 bool isLocal);

  bool parseDoMatchSyntaxDef(TokenVector* result, const Token& expr,
                             SyntaxTable& syntaxTable, bool isLocal);
  bool parseDoMatchSyntaxFunc(TokenVector* filtered, const Token& expr,
                              const TokenVector& args, SyntaxTable& syntaxTable,
                              bool shouldParseParams);

  bool parseExprStream(TokenVector* result, bool isTopLevel);

  bool matchSyntax(TokenVector* result, SyntaxTable& syntaxTable);
  bool replaceMatchBindings(TokenVector* result, const TokenVector& replacement,
                            const NamedReplacementMap& bindings);
  bool replaceSangHashIds(TokenVector* result, const TokenVector& source);
  const TokenVector& findReplaceToken(const Token& token,
                                      const NamedReplacementMap& bindings, bool& found);
  //@}

  bool matchParameter(const Token& macroParam, NamedReplacementMap* bindings,
                      SyntaxTreeNode& followSet);

  Token parseParameter(ParamType* expected, bool autoCompleteTypes);

  Token parseSlot();

  Token parseLinkageType();

  void registerSymbolForExport(const String& sym, VizType vizType,
                               Scope::ScopeDomain domain = Scope::kNormal);

  //-------- data members

  Token fToken;
  bool fEvaluateExprs = true;
  bool fInLibrary = false;
  bool fInApplication = false;
};


//! \c TokenCompilePass wrapper for the \c FirstPass pass to be used in the
//! process pipeline as very first pass.
class ExprPass : public TokenCompilePass {
public:
  ExprPass(int level, Compiler& compiler, const Token& currentToken,
           std::shared_ptr<Scope> scope);
  Token doApply(const Token& src) override;

private:
  Token fCurrentToken;
  std::shared_ptr<Scope> fScope;
  Compiler& fCompiler;
};

}  // namespace herschel
