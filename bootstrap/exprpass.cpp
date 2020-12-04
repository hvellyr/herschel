/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "compiler.hpp"

#include "errcodes.hpp"
//#include "externc.hpp"
#include "exprpass.hpp"
#include "log.hpp"
#include "macro.hpp"
#include "properties.hpp"
#include "scope.hpp"
#include "strbuf.hpp"
#include "symbol.hpp"
#include "tokeneval.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"
#include "valuesaver.hpp"

#include <map>


namespace herschel {

ExprPass::ExprPass(int level, Compiler& compiler, const Token& currentToken,
                   std::shared_ptr<Scope> scope)
    : TokenCompilePass(level)
    , fCurrentToken(currentToken)
    , fScope(std::move(scope))
    , fCompiler(compiler)
{
}


Token ExprPass::doApply(const Token& src)
{
  FirstPass fp{fCompiler, fCurrentToken, fScope};
  return fp.parse();
}


//----------------------------------------------------------------------------

namespace {
  static Token qualifyIdToken(const TokenVector& qSymbol)
  {
    return !qSymbol.empty() ? Token(qSymbol[0].srcpos(), qualifyId(qSymbol)) : Token();
  }


  static Token makeAnySymbol(const SrcPos& pos) { return Token(pos, kSymbol, "Any"); }


  enum class Retry { kDone, kRetry, kError };

  template <typename Functor>
  bool withRetryAfterMacroExpansion(const Functor& functor)
  {
    bool retry = false;
    bool result = true;

    do {
      switch (functor()) {
      case Retry::kRetry: retry = true; continue;
      case Retry::kError:
        retry = false;
        result = false;
        break;
      case Retry::kDone: retry = false; break;
      }
    } while (retry);

    return result;
  }


  bool isUsable(const TokenVector& n)
  {
    return !n.empty() && (n.size() != 1 || !n[0].isContinuation());
  }


  bool isPrescannedTypeToken(const Token& token)
  {
    return token.isSeq() && token.count() == 2 && token[0] == kColon;
  }

}  // namespace


//----------------------------------------------------------------------------

FirstPass::FirstPass(Compiler& compiler, const Token& currentToken,
                     std::shared_ptr<Scope> scope)
    : AbstractPass(compiler, std::move(scope))
    , fToken(currentToken)
{
}


//----------------------------------------------------------------------------

Token FirstPass::nextToken()
{
  fToken = fCompiler.nextToken();
  return fToken;
}


Token FirstPass::currentToken()
{
  return fToken;
}


void FirstPass::unreadToken(const Token& token)
{
  fCompiler.unreadToken(token);
}


void FirstPass::unreadTokens(const TokenVector& tokens)
{
  for (auto iToken = tokens.rbegin(); iToken != tokens.rend(); ++iToken) {
    fCompiler.unreadToken(*iToken);
  }
  nextToken();
}


Token FirstPass::scanUntilTopExprAndResume()
{
  while (fToken != kEOF && fToken != kDefId && fToken != kModuleId &&
         fToken != kLibraryId && fToken != kExportId && fToken != kImportId &&
         fToken != kIncludeId && fToken != kWhenId && fToken != kExternId &&
         fToken != kApplicationId)
    nextToken();

  return Token();
}


Token FirstPass::scanUntilNextParameter(TokenType endTokenType)
{
  while (fToken != kEOF && fToken != kComma && fToken != endTokenType)
    nextToken();

  return Token();
}


Token FirstPass::scanUntilBrace()
{
  while (fToken != kEOF && fToken != kBraceClose)
    nextToken();
  return Token();
}


Token FirstPass::scanUntilEndOfParameters()
{
  int paranLevel = 0;
  while (fToken != kEOF) {
    if (fToken == kParanOpen)
      paranLevel++;
    else if (fToken == kParanClose) {
      if (paranLevel > 0)
        paranLevel--;
      else {
        nextToken();
        break;
      }
    }
    nextToken();
  }

  return Token();
}


template <typename ParseFunctor>
void FirstPass::parseSequence(ParseFunctor functor, TokenType startToken,
                              TokenType endToken, bool hasSeparator, ErrCodes errorCode,
                              Token& result, zstring ctx, bool skipFirst, bool eatLast,
                              bool firstUnexpectedEnds)
{
  SrcPos startPos = fToken.srcpos();
  if (skipFirst)
    nextToken();

  Token delayedCommaToken;
  while (fToken != endToken) {
    if (fToken == kEOF)
      break;

    Token tmp;
    if (!functor(this, tmp))
      break;

    if (tmp.isSet()) {
      if (delayedCommaToken == kComma)
        result << delayedCommaToken;
      result << tmp.children();
      delayedCommaToken = Token();
    }

    if (hasSeparator) {
      if (fToken == kComma) {
        delayedCommaToken = fToken;
        nextToken();
      }
      else if (fToken != endToken) {
        if (firstUnexpectedEnds)
          break;

        HR_LOG(kError, fToken.srcpos(), errorCode)
            << ctx << ": expected '" << Token(SrcPos(), endToken) << "' or ','"
            << "; found: " << fToken;
        break;
      }
    }
  }

  if (fToken == endToken) {
    if (eatLast)
      nextToken();
  }
  else {
    if (!firstUnexpectedEnds) {
      HR_LOG(kError, fToken.srcpos(), errorCode)
          << ctx << ": expected '" << Token(SrcPos(), endToken) << "'";

      if (startToken != kInvalid && startPos != fToken.srcpos())
        HR_LOG(kError, startPos, errorCode)
            << ctx << ": beginning '" << Token(SrcPos(), startToken) << "' was here";
      scanUntilTopExprAndResume();
    }
  }
}


template <typename ParseFunctor>
void FirstPass::parseChoiceSequence(ParseFunctor functor, TokenType choiceToken,
                                    Token& result)
{
  while (fToken == choiceToken) {
    Token tmp;
    if (!functor(this, tmp))
      break;

    if (tmp.isSet())
      result << tmp.children();
  }
}


template <typename ParseFunctor>
void FirstPass::parseSumType(ParseFunctor functor, ErrCodes errorCode, Token& result,
                             zstring ctx)
{
  TokenType groupType = kInvalid;
  Token delayedSepToken;
  while (fToken != kParanClose && fToken != kEOF) {
    Token tmp;
    if (!functor(this, tmp))
      break;

    if (tmp.isSet()) {
      if (delayedSepToken != Token()) {
        result << delayedSepToken;
      }
      result << tmp.children();
      delayedSepToken = Token();
    }

    if (fToken == kPipe || fToken == kComma) {
      if (groupType == kInvalid || groupType == fToken.tokenType()) {
        groupType = fToken.tokenType();
        delayedSepToken = fToken;
        nextToken();
        continue;
      }
      else {
        HR_LOG(kError, fToken.srcpos(), E_InconsistentGroupType)
            << ctx << ": expected wrong group type operator: " << fToken;
        nextToken();
        continue;
      }
    }
  }

  if (fToken == kParanClose) {
    nextToken();
  }
  else {
    HR_LOG(kError, fToken.srcpos(), errorCode)
        << ctx << ": expected '" << Token(SrcPos(), kParanClose) << "'";

    scanUntilTopExprAndResume();
  }
}


//----------------------------------------------------------------------------

TokenVector FirstPass::parseQualifiedName(bool acceptLeadingDot)
{
  TokenVector result;

  if (fToken == kSymbol || fToken == kDot) {
    if (fToken == kSymbol) {
      result.push_back(fToken);
      nextToken();
    }
    else if (fToken == kDot) {
      if (!acceptLeadingDot) {
        nextToken();

        if (fToken == kSymbol) {
          HR_LOG(kError, fToken.srcpos(), E_UnexpectedRootedSymbol)
              << "Rooted qualified name not allowed in this postion";

          result.push_back(fToken);
          nextToken();
        }
        else {
          HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken) << "unexpected dot";
          return TokenVector();
        }
      }
    }

    while (fToken == kDot) {
      result.push_back(fToken);
      nextToken();

      if (fToken == kSymbol) {
        result.push_back(fToken);
        nextToken();
      }
      else {
        HR_LOG(kError, fToken.srcpos(), E_SymbolExpected) << "qualified name ends in '.'";
      }
    }
  }

  return result;
}


//----------------------------------------------------------------------------

struct ModuleParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    TokenVector n = pass->parseTop();
    if (isUsable(n))
      result << n;

    return true;
  }
};


Token FirstPass::parseLibrary()
{
  Token tagToken = fToken;
  nextToken();

  Token libExpr;

  auto qSymbol = parseQualifiedName(false);
  if (!qSymbol.empty()) {
    Token libName = qualifyIdToken(qSymbol);

    libExpr = Token() << tagToken << libName;

    Token docString = parseOptDocString();
    if (docString.isSet())
      libExpr << docString;

    Token defines = Token(fToken.srcpos(), kBraceOpen, kBraceClose);

    {
      ScopeHelper scopeHelper(fScope, K(doExport), !K(isInnerScope), !K(doPropIntern),
                              kScopeL_Library);
      ModuleHelper moduleScope(this, libName.idValue());

      fInLibrary = true;

      if (fToken == kBraceOpen) {
        parseSequence(ModuleParser(), kBraceOpen, kBraceClose, !K(hasSeparator),
                      E_MissingBraceClose, defines, "library-body");
      }
      else
        parseTops(defines);

      fInLibrary = false;
    }

    libExpr << defines;
  }

  return libExpr;
}


struct AppKeysParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken.isKeyArg()) {
      Token key = pass->fToken;
      pass->nextToken();

      Token val = pass->parseExpr(!K(acceptComma));
      if (!val.isSet()) {
        HR_LOG(kError, pass->fToken.srcpos(), E_UnexpectedToken)
            << "Unexpected token while parsing application keyed argument's expr: "
            << pass->fToken;
        pass->scanUntilNextParameter();
        return true;
      }
      result << key;
      result << val;
    }
    else {
      HR_LOG(kError, pass->fToken.srcpos(), E_UnexpectedToken)
          << "unexpected token while parsing application arguments: " << pass->fToken;
      pass->scanUntilNextParameter();
      return true;
    }

    return true;
  }
};


void FirstPass::parseAppArgs(TokenVector* argsVector)
{
  Token args;
  parseSequence(AppKeysParser(), kParanOpen, kParanClose, K(hasSeparator),
                E_BadParameterList, args, "funcall-args", !K(skipFirst));

  if (args.isSeq())
    *argsVector = args.children();
}


Token FirstPass::parseApplication()
{
  Token tagToken = fToken;
  nextToken();

  Token appExpr;

  auto qSymbol = parseQualifiedName(false);
  if (!qSymbol.empty()) {
    Token appName = qualifyIdToken(qSymbol);

    appExpr = Token() << tagToken << appName;

    Token docString = parseOptDocString();
    if (docString.isSet())
      appExpr << docString;

    Token params = Token(fToken.srcpos(), kParanOpen, kParanClose);

    if (fToken == kParanOpen) {
      nextToken();

      TokenVector args;
      parseAppArgs(&args);
      params << args;
    }

    Token defines = Token(fToken.srcpos(), kBraceOpen, kBraceClose);

    {
      ScopeHelper scopeHelper(fScope, K(doExport), !K(isInnerScope), !K(doPropIntern),
                              kScopeL_Library);
      ModuleHelper moduleScope(this, appName.idValue());

      fInApplication = true;

      if (fToken == kBraceOpen) {
        parseSequence(ModuleParser(), kBraceOpen, kBraceClose, !K(hasSeparator),
                      E_MissingBraceClose, defines, "application-body");
      }
      else
        parseTops(defines);

      fInApplication = false;
    }

    appExpr << params << defines;
  }

  return appExpr;
}


Token FirstPass::parseModule()
{
  Token tagToken = fToken;
  nextToken();

  Token modExpr;

  auto qSymbol = parseQualifiedName(false);
  if (!qSymbol.empty()) {
    Token modName = qualifyIdToken(qSymbol);

    modExpr = Token() << tagToken << modName;

    Token docString = parseOptDocString();
    if (docString.isSet())
      modExpr << docString;

    Token defines = Token(fToken.srcpos(), kBraceOpen, kBraceClose);

    {
      ScopeHelper scopeHelper(fScope, K(doExport), K(isInnerScope), K(doPropIntern),
                              kScopeL_Module);

      ModuleHelper moduleScope(this, modName.idValue());
      if (fToken == kBraceOpen) {
        parseSequence(ModuleParser(), kBraceOpen, kBraceClose, !K(hasSeparator),
                      E_MissingBraceClose, defines, "module-body");
      }
      else {
        parseTops(defines);
      }
    }

    modExpr << defines;
  }

  return modExpr;
}


struct ExportParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken.isSymbol()) {
      Token symbol = pass->fToken;
      pass->nextToken();

      if (pass->fToken == kColon) {
        Token colon = pass->fToken;
        // pass symbol-domain identifier
        pass->nextToken();

        if (pass->fToken != kSymbol) {
          HR_LOG(kError, pass->fToken.srcpos(), E_SymbolExpected)
              << "expected symbol domain identifier";
          pass->scanUntilNextParameter();
        }
        else {
          result << (Token() << symbol << colon << pass->fToken);
          pass->nextToken();
        }
      }
      else
        result << symbol;
    }
    else if (pass->fToken == kMultiply) {
      result << Token(pass->fToken.srcpos(), "*");
      pass->nextToken();
    }
    else {
      HR_LOG(kError, pass->fToken.srcpos(), E_SymbolExpected) << "expected SYMBOL or '*'";
      pass->scanUntilNextParameter();
    }

    return true;
  }
};


Token FirstPass::parseExport()
{
  Token expr;
  expr << fToken;

  nextToken();

  VizType vizType = kIntern;
  if (fToken == kSymbol) {
    if (fToken == Compiler::publicToken || fToken == Compiler::pubToken) {
      vizType = kPublic;
      expr << fToken;
    }
    else if (fToken == Compiler::internToken) {
      vizType = kIntern;
      expr << fToken;
    }
    else
      HR_LOG(kError, fToken.srcpos(), E_ExportVisibility)
          << "unknown visibility type '" << fToken << "'";

    nextToken();
  }

  bool ignore = false;
  Token symbols = Token(fToken.srcpos(), kParanOpen, kParanClose);
  if (fToken == kSymbol) {
    symbols << fToken;
    expr << symbols;

    nextToken();
  }
  else if (fToken == kMultiply) {
    symbols << Token(fToken.srcpos(), "*");
    expr << symbols;

    nextToken();
  }
  else if (fToken == kParanOpen) {
    parseSequence(ExportParser(), kParanOpen, kParanClose, K(hasSeparator),
                  E_BadParameterList, symbols, "export-symbols");
    if (symbols.isEmpty()) {
      HR_LOG(kError, fToken.srcpos(), E_EmptyExportList) << "empty export list";
      ignore = true;
    }
    else
      expr << symbols;
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }

  bool isFinal = false;
  if (fToken == kAs) {
    Token asToken = fToken;
    nextToken();

    if (fToken != Compiler::finalToken) {
      HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken) << "expected 'final'";
    }
    else {
      isFinal = true;
      expr << asToken << fToken;
      nextToken();
    }
  }

  if (!ignore) {
    TokenVector children = symbols.children();
    for (TokenVector::const_iterator it = children.begin(); it != children.end(); it++) {
      if (*it == kSymbol) {
        String fullId = (isQualified(it->idValue()) || it->idValue() == String("*")
                             ? it->idValue()
                             : qualifyId(currentModuleName(), it->idValue()));
        fScope->registerSymbolForExport(Scope::kNormal, fullId, vizType, isFinal);
      }
    }

    return expr;
  }

  return Token();
}


struct IncludeParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken == kString) {
      Token str = pass->fToken;
      pass->nextToken();

      result << str;
    }
    else {
      HR_LOG(kError, pass->fToken.srcpos(), E_StringExpected) << "expected STRING";
      pass->scanUntilNextParameter();
    }

    return true;
  }
};


TokenVector FirstPass::parseInclude()
{
  Token inclToken = fToken;
  nextToken();

  bool isPublic = false;
  if (fToken == kSymbol) {
    if (fToken == Compiler::publicToken || fToken == Compiler::pubToken)
      isPublic = true;
    else if (fToken == Compiler::internToken)
      isPublic = false;
    else
      HR_LOG(kError, fToken.srcpos(), E_UnknownIncludeScope)
          << "unknown include scope '" << fToken << "'";

    nextToken();
  }

  std::vector<std::pair<SrcPos, String>> srcNames;

  if (fToken == kString) {
    srcNames.push_back(std::make_pair(fToken.srcpos(), fToken.stringValue()));
    nextToken();
  }
  else if (fToken == kParanOpen) {
    Token sources;
    parseSequence(IncludeParser(), kParanOpen, kParanClose, K(hasSeparator),
                  E_BadParameterList, sources, "include paths");
    if (!sources.isEmpty()) {
      for (const auto& token : sources.children()) {
        if (token == kString) {
          srcNames.push_back(std::make_pair(token.srcpos(), token.stringValue()));
        }
      }
    }
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_StringExpected)
        << "expected STRING or '('.  Found " << fToken;
    return scanUntilTopExprAndResume().toTokenVector();
  }


  TokenVector result;

  if (isPublic || !fCompiler.isParsingInterface()) {
    for (const auto& srcp : srcNames) {
      try {
        auto incl = fCompiler.includeFile(srcp.first, srcp.second, [&]() {
          nextToken();

          Token seq;
          parseTops(seq);
          return seq.children();
        });
        result.insert(result.end(), incl.begin(), incl.end());

        nextToken();
      }
      catch (const Exception& e) {
        HR_LOG(kError, srcp.first, E_UnknownInputFile) << e.message();
      }
    }
  }

  return result;
}


struct ImportParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken == kSymbol) {
      Token sym = pass->fToken;
      pass->nextToken();

      result << sym;
    }
    else {
      HR_LOG(kError, pass->fToken.srcpos(), E_SymbolExpected) << "expected SYMBOL";
      pass->scanUntilNextParameter();
    }

    return true;
  }
};


TokenVector FirstPass::parseImport()
{
  Token importToken = fToken;
  nextToken();

  std::vector<Token> libNames;

  if (fToken == kSymbol) {
    libNames.push_back(fToken);
    nextToken();
  }
  else if (fToken == kParanOpen) {
    Token libs;
    parseSequence(ImportParser(), kParanOpen, kParanClose, K(hasSeparator),
                  E_BadParameterList, libs, "import libraries");
    if (!libs.isEmpty()) {
      for (const auto& token : libs.children()) {
        if (token == kSymbol)
          libNames.push_back(token);
      }
    }
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_SymbolExpected) << "expected SYMBOL";
    return scanUntilTopExprAndResume().toTokenVector();
  }

  TokenVector result;

  for (const auto& libNameSymb : libNames) {
    try {
      Token expr;
      expr << importToken << libNameSymb;

      if (fEvaluateExprs) {
        String libName = libNameSymb.idValue();
        if (fCompiler.requireLibrary(libNameSymb.srcpos(), libName, fScope)) {
          result.push_back(expr);
        }
      }
      else {
        result.push_back(expr);
      }
    }
    catch (const Exception& e) {
      HR_LOG(kError, libNameSymb.srcpos(), E_UnknownLibrary) << e.message();
    }
  }

  return result;
}


struct TypeParser {
  TokenType fEndToken;

  TypeParser(TokenType endToken)
      : fEndToken(endToken)
  {
  }

  bool operator()(FirstPass* pass, Token& result)
  {
    SrcPos pos = pass->fToken.srcpos();
    Token type = pass->parseTypeSpec(!K(onlyNestedConstr), K(needParans));
    if (!type.isSet()) {
      HR_LOG(kError, pos, E_UnexpectedToken)
          << "returntype expression expected, but found: " << pass->fToken;
      pass->scanUntilNextParameter(fEndToken);
    }
    else
      result << type;

    return true;
  }
};


Token FirstPass::parseSymbolOrSimpleType(const Token& baseToken)
{
  hr_assert(baseToken == kSymbol || baseToken == kQuote || baseToken == kDot);

  Token typeName = baseToken;

  if (baseToken == kSymbol || baseToken == kDot) {
    auto qSymbol = parseQualifiedName(true);

    if (qSymbol.empty()) {
      HR_LOG(kError, baseToken.srcpos(), E_UnexpectedQuote) << "Unexpected quote";
      return Token();
    }

    typeName = qualifyIdToken(qSymbol);
  }
  else if (baseToken == kQuote) {
    Token t = baseToken;
    nextToken();

    if (fToken != kSymbol) {
      HR_LOG(kError, t.srcpos(), E_UnexpectedQuote) << "Unexpected quote";
      return Token();
    }

    typeName = Token() << t << fToken;
    nextToken();
  }
  else
    nextToken();

  if (fToken == kGenericOpen) {
    Token generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(kGenericClose), kGenericOpen, kGenericClose, K(hasSeparator),
                  E_GenericTypeList, generics, "type-params");
    return Token() << typeName << generics;
  }

  return typeName;
}


Token FirstPass::parseGroupType()
{
  nextToken();

  Token nested = Token(fToken.srcpos(), kParanOpen, kParanClose);
  parseSumType(TypeParser(kParanClose), E_BadParameterList, nested, "sum-type");

  return nested;
}


Token FirstPass::parseArrayExtend(const Token& baseType)
{
  Token result = baseType;

  while (fToken == kBracketOpen) {
    Token arrayType = Token(fToken.srcpos(), kBracketOpen, kBracketClose);
    nextToken();

    Token idxExpr;
    if (fToken != kBracketClose) {
      SrcPos idxPos = fToken.srcpos();
      idxExpr = parseExpr(!K(acceptComma));
      if (!idxExpr.isSet())
        HR_LOG(kError, idxPos, E_UnexpectedToken) << "expected index expression";
      else
        arrayType << idxExpr;
    }

    if (fToken != kBracketClose) {
      HR_LOG(kError, fToken.srcpos(), E_MissingBracketClose) << "expected ']'";
    }
    else
      nextToken();

    result = Token() << result << arrayType;
  }

  return result;
}


bool FirstPass::isConstraintOperator(const Token& token) const
{
  return (token == kEqual || token == kUnequal || token == kLess || token == kGreater ||
          token == kLessEqual || token == kGreaterEqual || token == kIn || token == kIsa);
}


Token FirstPass::parseConstraintExtend(const Token& baseType)
{
  if (isConstraintOperator(fToken)) {
    Token op = fToken;
    nextToken();

    Token constExpr = parseExpr(!K(acceptComma));
    if (!(constExpr.isLit() || constExpr.isSymbol() || constExpr.isConstRange())) {
      HR_LOG(kError, constExpr.srcpos(), E_ConstExprExpected)
          << "constraint types only accept constant expressions.  Found " << constExpr;
      return baseType;
    }

    return Token() << baseType << op << constExpr;
  }

  return baseType;
}


Token FirstPass::parseFunctionSignature()
{
  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }

  SrcPos paranPos = fToken.srcpos();
  TokenVector params;
  if (parseFunctionsParams(&params)) {
    Token returnTyToken;
    Token returnType;

    if (fToken == kMapTo) {
      returnTyToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      returnType = parseTypeSpec(K(onlyNestedConstr), !K(needParans));
      if (!returnType.isSet()) {
        HR_LOG(kError, pos, E_MissingType) << "returntype expression expected";
        returnType = makeAnySymbol(pos);
      }
    }
    else {
      returnTyToken = Token(fToken.srcpos(), kMapTo);
      returnType = makeAnySymbol(fToken.srcpos());
    }

    return Token() << (Token(paranPos, kParanOpen, kParanClose) << params)
                   << returnTyToken << returnType;
  }
  return Token();
}


Token FirstPass::parseFunctionType()
{
  Token funcToken = fToken;
  nextToken();

  Token signature = parseFunctionSignature();
  if (signature.isSet())
    return Token() << funcToken << signature.children();
  return Token();
}


Token FirstPass::parseQuotedType()
{
  hr_assert(fToken == kQuote);
  Token quote = fToken;
  nextToken();

  if (fToken != kSymbol) {
    HR_LOG(kError, fToken.srcpos(), E_SymbolExpected) << "missing type name";
    return Token();
  }

  Token result = Token() << quote << fToken;
  nextToken();

  return result;
}


Token FirstPass::parseTypeSpec(bool onlyNestedConstraints, bool needParans,
                               bool acceptEmpty)
{
  bool maybeEmpty = acceptEmpty;

  if (fToken.isSeq()) {
    if (isPrescannedTypeToken(fToken))
      return fToken[1];

    if (maybeEmpty)
      return Token();
  }

  Token isRefToken;
  if (fToken == kReference) {
    isRefToken = fToken;
    nextToken();
    maybeEmpty = false;
  }

  // TODO: pass the isRefType into the following functions
  Token typeGroup = Token(fToken.srcpos(), kParanOpen, kParanClose);
  bool isTypeGroupSet = false;

  Token retval;
  bool tryNext = false;
  do {
    tryNext = false;

    if (fToken == kSymbol || fToken == kDot) {
      auto typeNode = parseArrayExtend(parseSymbolOrSimpleType(fToken));
      retval = onlyNestedConstraints ? typeNode : parseConstraintExtend(typeNode);
    }
    else if (fToken == kFUNCTIONId) {
      retval = parseArrayExtend(parseFunctionType());
    }
    else if (fToken == kQuote) {
      // no constraints for generics
      retval = parseArrayExtend(parseQuotedType());
    }
    else if (fToken == kParanOpen) {
      // no constraints for sequence types
      retval = parseArrayExtend(parseGroupType());
    }
    else if (fToken.isNested()) {
      // prescanned type
      retval = fToken;
      nextToken();
    }
    else {
      if (maybeEmpty) {
        return Token();
      }

      HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken)
          << "Unexpected token [3], type expression expected: " << fToken << " "
          << maybeEmpty;
    }

    if (!needParans && fToken == kComma) {
      typeGroup << retval << fToken;
      isTypeGroupSet = true;

      nextToken();
      tryNext = true;
    }
    else if (isTypeGroupSet) {
      typeGroup << retval;
      retval = typeGroup;
    }
  } while (tryNext);

  if (isRefToken.isSet())
    return Token() << isRefToken << retval;

  return retval;
}


struct LiteralVectorParser {
  LiteralVectorParser()
      : fIsDict(false)
      , fIsFirst(true)
  {
  }

  bool operator()(FirstPass* pass, Token& result)
  {
    Token expr = pass->parseExpr(!K(acceptComma));

    if (!expr.isSet()) {
      pass->scanUntilNextParameter();
    }
    else {
      if (pass->fToken == kMapTo) {
        if (fIsFirst)
          fIsDict = true;
        else if (!fIsDict) {
          HR_LOG(kError, pass->fToken.srcpos(), E_InconsistentArgs)
              << "For literal dictionaries all elements must be '->' pairs";
          pass->scanUntilNextParameter();
          return true;
        }

        Token mapToken = pass->fToken;
        pass->nextToken();

        Token toValue = pass->parseExpr(!K(acceptComma));
        if (!toValue.isSet()) {
          HR_LOG(kError, mapToken.srcpos(), E_MissingRHExpr)
              << "'->' requires a second expression";
          pass->scanUntilNextParameter();
        }
        else
          result << (Token() << expr << mapToken << toValue);
      }
      else {
        if (fIsDict)
          HR_LOG(kError, expr.srcpos(), E_InconsistentArgs)
              << "For literal dictionaries all elements must be '->' pairs";
        else
          result << expr;
      }
    }

    fIsFirst = false;
    return true;
  }

  bool fIsDict;
  bool fIsFirst;
};


Token FirstPass::parseLiteralVector()
{
  Token nested = Token(fToken.srcpos(), kLiteralVectorOpen, kParanClose);
  parseSequence(LiteralVectorParser(), kLiteralVectorOpen, kParanClose, K(hasSeparator),
                E_BadParameterList, nested, "literal-vector");
  return nested;
}


struct LiteralArrayParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    Token n = pass->parseExpr(!K(acceptComma));
    if (n.isSet())
      result << n;
    else {
      HR_LOG(kError, pass->fToken.srcpos(), E_UnexpectedToken)
          << "Unexpected token while parsing array: " << pass->fToken;
      return false;
    }

    return true;
  }
};


Token FirstPass::parseLiteralArray()
{
  Token array = Token(fToken.srcpos(), kLiteralArrayOpen, kBracketClose);
  parseSequence(LiteralArrayParser(), kLiteralArrayOpen, kBracketClose, K(hasSeparator),
                E_BadParameterList, array, "literal-array");
  return array;
}


Token FirstPass::parseIf()
{
  Token ifToken = fToken;

  nextToken();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  Token test = parseExpr(!K(acceptComma));
  if (fToken != kParanClose) {
    HR_LOG(kError, fToken.srcpos(), E_ParamMissParanClose) << "Syntax error, missing ')'";
  }
  else
    nextToken();

  Token consequent = parseExpr(!K(acceptComma));

  Token result;
  result << ifToken;

  if (test.isSeq())
    result << (Token(paranPos, kParanOpen, kParanClose) << test.children());
  else
    result << (Token(paranPos, kParanOpen, kParanClose) << test);
  result << consequent;

  if (fToken == kElseId) {
    Token elseToken = fToken;
    nextToken();

    Token alternate = parseExpr(!K(acceptComma));

    result << elseToken << alternate;
  }

  return result;
}


Token FirstPass::parseSlot()
{
  auto parseSlotImpl = [&](Token& paramSeq) {
    bool doScanOn = true;

    if (fToken.isSeq() && fToken.count() >= 1) {
      size_t ofs = 0;
      if (ofs < fToken.count()) {
        if (fToken[ofs] == kSymbol) {
          ofs++;
          if (ofs + 1 < fToken.count() && fToken[ofs] == kColon) {
            ofs += 2;
          }
          if (ofs + 1 < fToken.count() && fToken[ofs] == kAssign) {
            paramSeq << fToken;
            nextToken();
            ofs += 2;
            doScanOn = false;
          }
          else if (ofs == fToken.count()) {
            paramSeq << fToken;
            nextToken();
            doScanOn = false;
          }
        }
      }
    }

    if (doScanOn) {
      if (fToken == kSymbol) {
        auto symbol = fToken;
        nextToken();

        if (fToken == kParanOpen) {
          nextToken();

          auto t = parseParamCall(symbol);
          if (t.isContinuation()) {
            return Retry::kRetry;
          }
          else
            paramSeq << t.unwrapSingleton();
        }
        else
          paramSeq << symbol;
      }

      Token typeIntroToken = fToken;
      if (fToken == kColon) {
        nextToken();

        SrcPos pos = fToken.srcpos();
        Token type = parseTypeSpec(K(onlyNestedConstr), K(needParans));
        if (!type.isSet()) {
          HR_LOG(kError, pos, E_MissingType) << "type expression expected";
          paramSeq << typeIntroToken << makeAnySymbol(pos);
        }
        else
          paramSeq << typeIntroToken << type;
      }
      else if (isPrescannedTypeToken(fToken)) {
        paramSeq << fToken.children();
        nextToken();
      }
      else {
        paramSeq << Token(typeIntroToken.srcpos(), kColon)
                 << makeAnySymbol(typeIntroToken.srcpos());
      }

      if (fToken == kAssign) {
        Token assignToken = fToken;
        nextToken();

        SrcPos pos = fToken.srcpos();
        Token initExpr = parseExpr(!K(acceptComma));
        if (!initExpr.isSet())
          HR_LOG(kError, pos, E_MissingRHExpr) << "no value in initialized slot";
        else
          paramSeq << assignToken << initExpr;
      }
    }

    return Retry::kDone;
  };

  Token paramSeq;
  if (!withRetryAfterMacroExpansion([&]() { return parseSlotImpl(paramSeq); }))
    return Token::sInvalid();

  return paramSeq.unwrapSingleton();
}


struct ParseRecordSlotsParser {
  ParseRecordSlotsParser() = default;

  bool operator()(FirstPass* pass, Token& result)
  {
    Token param = pass->parseSlot();
    if (param.isSet())
      result << param;
    return true;
  }
};


bool FirstPass::parseRecordSlotsFull(TokenVector* exprlist, TokenType startToken,
                                     TokenType endToken, bool skipFirst, bool eatLast,
                                     bool firstUnexpectedEnds)
{
  Token params;
  parseSequence(ParseRecordSlotsParser(), startToken, endToken, K(hasSeparator),
                E_BadSlotList, params, "slots", skipFirst, eatLast, firstUnexpectedEnds);

  *exprlist = params.children();
  return true;
}


bool FirstPass::parseRecordSlots(TokenVector* exprlist)
{
  return parseRecordSlotsFull(exprlist, kParanOpen, kParanClose, K(skipFirst), K(eatLast),
                              !K(firstUnexpectedEnds));
}


Token FirstPass::parseParameter(ParamType* expected, bool autoCompleteTypes)
{
  auto parseParameter1stPart = [&](Token& paramSeq, ParamType& paramType,
                                   bool& doScanOn) {
    if (fToken.isSeq() && fToken.count() >= 1) {
      size_t ofs = 0;
      if (ofs + 2 < fToken.count() && fToken[ofs] == kSymbol &&
          fToken[ofs + 1] == kMapTo) {
        paramType = kNamed;
        ofs += 2;
        doScanOn = false;
      }

      if (ofs < fToken.count()) {
        if (fToken[ofs] == kSymbol) {
          ofs++;
          if (ofs + 1 < fToken.count() && fToken[ofs] == kColon) {
            ofs += 2;
          }
          if (ofs + 1 < fToken.count() && fToken[ofs] == kAssign) {
            paramSeq << fToken;
            nextToken();
            paramType = kNamed;
            ofs += 2;
            doScanOn = false;
          }
          else if (ofs < fToken.count() && fToken[ofs] == kEllipsis) {
            paramSeq << fToken;
            nextToken();
            paramType = kRest;
            ofs++;
            doScanOn = false;
          }
          else if (ofs == fToken.count()) {
            paramSeq << fToken;
            nextToken();
            paramType = kPositional;
            doScanOn = false;
          }
        }
      }
    }

    if (doScanOn) {
      if (fToken == kSymbol) {
        auto symbol = fToken;
        nextToken();

        if (fToken == kParanOpen) {
          nextToken();

          auto t = parseParamCall(symbol);
          if (t.isContinuation()) {
            return Retry::kRetry;
          }
          else
            paramSeq << t.unwrapSingleton();
        }
        else
          paramSeq << symbol;
      }
    }

    return Retry::kDone;
  };

  auto parseParamSymbol = [&](Token& paramSeq) {
    if (fToken == kSymbol) {
      auto symbol = fToken;
      nextToken();

      if (fToken == kParanOpen) {
        nextToken();

        Token t = parseParamCall(symbol);
        if (t.isContinuation()) {
          return Retry::kRetry;
        }
        else
          paramSeq << t.unwrapSingleton();
      }
      else
        paramSeq << symbol;
    }
    else {
      HR_LOG(kError, fToken.srcpos(), E_SymbolExpected)
          << "parameter name expected: " << fToken;
      scanUntilNextParameter();
      return Retry::kError;
    }
    return Retry::kDone;
  };

  Token paramSeq;
  ParamType paramType = kPositional;
  bool doScanOn = true;

  if (!withRetryAfterMacroExpansion(
          [&]() { return parseParameter1stPart(paramSeq, paramType, doScanOn); }))
    return Token::sInvalid();

  if (doScanOn) {
    if (fToken == kMapTo) {
      paramSeq << fToken;
      nextToken();
      paramType = kNamed;

      if (!withRetryAfterMacroExpansion([&]() { return parseParamSymbol(paramSeq); }))
        return Token::sInvalid();
    }

    Token typeIntroToken = fToken;
    if (fToken == kColon || fToken == kAt) {
      nextToken();

      SrcPos pos = fToken.srcpos();
      Token type = parseTypeSpec(K(onlyNestedConstr), K(needParans));
      if (!type.isSet()) {
        HR_LOG(kError, pos, E_MissingType) << "type expression expected";
        if (autoCompleteTypes)
          paramSeq << typeIntroToken << makeAnySymbol(pos);
      }
      else
        paramSeq << typeIntroToken << type;
    }
    else if (isPrescannedTypeToken(fToken)) {
      paramSeq << fToken.children();
      nextToken();
    }
    else if (autoCompleteTypes)
      paramSeq << Token(typeIntroToken.srcpos(), kColon)
               << makeAnySymbol(typeIntroToken.srcpos());

    if (fToken == kAssign) {
      Token assignToken = fToken;
      nextToken();

      SrcPos pos = fToken.srcpos();
      Token initExpr = parseExpr(!K(acceptComma));
      if (!initExpr.isSet())
        HR_LOG(kError, pos, E_MissingRHExpr) << "no value in keyed argument";
      else {
        paramSeq << assignToken << initExpr;
        paramType = kNamed;
      }
    }
    else if (fToken == kEllipsis) {
      Token restToken = fToken;
      nextToken();

      if (paramType != kPositional) {
        HR_LOG(kError, restToken.srcpos(), E_InvalidRestParam)
            << "orphaned rest parameter";
      }
      else {
        paramSeq << restToken;
        paramType = kRest;
      }
    }
  }

  if (*expected == kPositional) {
    *expected = paramType;
    return paramSeq.unwrapSingleton();
  }
  else if (*expected == kNamed) {
    if (paramType == kPositional)
      HR_LOG(kError, paramSeq.srcpos(), E_ParamOrder)
          << "out of order (positional) parameter";
    else {
      *expected = paramType;
      return paramSeq.unwrapSingleton();
    }
  }
  else if (*expected == kRest) {
    HR_LOG(kError, paramSeq.srcpos(), E_ParamOrder)
        << "no parameter after rest parameter";
  }

  return Token();
}


struct ParseFuncParamsParser {
  FirstPass::ParamType fExpected;
  bool fAutoCompleteTypes;

  ParseFuncParamsParser(bool autoCompleteTypes)
      : fExpected(FirstPass::kPositional)
      , fAutoCompleteTypes(autoCompleteTypes)
  {
  }

  bool operator()(FirstPass* pass, Token& result)
  {
    Token param = pass->parseParameter(&fExpected, fAutoCompleteTypes);
    if (param.isSet())
      result << param;
    return true;
  }
};


bool FirstPass::parseFunctionsParamsFull(TokenVector* exprlist, TokenType startToken,
                                         TokenType endToken, bool autoCompleteType,
                                         bool acceptEmptyList, bool skipFirst,
                                         bool eatLast)
{
  Token params;
  parseSequence(ParseFuncParamsParser(autoCompleteType), startToken, endToken,
                K(hasSeparator), E_BadParameterList, params, "func-params", skipFirst,
                eatLast);

  if (params.isSet() || acceptEmptyList) {
    *exprlist = params.children();
    return true;
  }

  return false;
}


bool FirstPass::parseFunctionsParams(TokenVector* exprlist, bool autoCompleteType,
                                     bool acceptEmptyList)
{
  return parseFunctionsParamsFull(exprlist, kParanOpen, kParanClose, autoCompleteType,
                                  acceptEmptyList, K(skipFirst), K(eatLast));
}


Token FirstPass::parseAnonFun()
{
  Token funcToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }

  SrcPos paranPos = fToken.srcpos();
  TokenVector params;
  if (parseFunctionsParams(&params)) {
    Token returnTyToken;
    Token returnType;

    if (fToken == kMapTo) {
      returnTyToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      returnType = parseTypeSpec(K(onlyNestedConstr), K(needParans));
      if (!returnType.isSet()) {
        HR_LOG(kError, pos, E_MissingType) << "returntype expression expected";
        returnType = makeAnySymbol(pos);
      }
    }
    else {
      returnTyToken = Token(fToken.srcpos(), kMapTo);
      returnType = makeAnySymbol(fToken.srcpos());
    }

    Token body = parseExpr(!K(acceptComma));
    if (body.isSet())
      return Token() << funcToken << (Token(paranPos, kParanOpen, kParanClose) << params)
                     << returnTyToken << returnType << body;
  }
  return Token();
}


struct FuncallArgsParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (!withRetryAfterMacroExpansion([&]() {
          if (pass->fToken == kSymbol && !pass->fToken.isKeyArg()) {
            auto symbolToken = pass->fToken;
            pass->nextToken();

            if (pass->fToken == kParanOpen) {
              auto paranOpenToken = pass->fToken;
              pass->nextToken();

              if (const auto* macro = pass->fScope->lookupMacro(symbolToken.srcpos(),
                                                                symbolToken.idValue(),
                                                                K(showAmbiguousSymDef))) {
                if (macro->type() == kMacro_Function) {
                  Token macroName =
                      Token(symbolToken.srcpos(), baseName(symbolToken.idValue()));
                  TokenVector exprs =
                      pass->parseMakeMacroCall(macroName, macro, K(isLocal));
                  if (exprs.size() == 1) {
                    if (exprs[0].isContinuation())
                      return Retry::kRetry;

                    pass->fToken = exprs[0];
                  }
                  else {
                    hr_invalid("");
                    return Retry::kError;
                  }
                }
                else {
                  HR_LOG(kError, pass->fToken.srcpos(), E_UnexpectedToken)
                      << "Unexpected macro call";
                  return Retry::kError;
                }
              }
              else {
                pass->unreadToken(pass->fToken);
                pass->unreadToken(paranOpenToken);
                pass->fToken = symbolToken;
              }
            }
            else {
              pass->unreadToken(pass->fToken);
              pass->fToken = symbolToken;
            }
          }
          return Retry::kDone;
        }))
      return false;

    if (pass->fToken.isKeyArg()) {
      Token key = pass->fToken;
      pass->nextToken();

      Token val = pass->parseExpr(!K(acceptComma));
      if (!val.isSet()) {
        HR_LOG(kError, pass->fToken.srcpos(), E_UnexpectedToken)
            << "Unexpected token while parsing function keyed argument's expr: "
            << pass->fToken;
        pass->scanUntilNextParameter();
        return true;
      }
      result << key;
      result << val;
    }
    else {
      Token val = pass->parseExpr(!K(acceptComma));
      if (!val.isSet()) {
        HR_LOG(kError, pass->fToken.srcpos(), E_UnexpectedToken)
            << "unexpected token while parsing function arguments: " << pass->fToken;
        pass->scanUntilNextParameter();
        return true;
      }
      result << val;
    }

    return true;
  }
};


void FirstPass::parseFuncallArgs(TokenVector* argsVector)
{
  Token args;
  parseSequence(FuncallArgsParser(), kParanOpen, kParanClose, K(hasSeparator),
                E_BadParameterList, args, "funcall-args", !K(skipFirst));

  if (args.isSeq())
    *argsVector = args.children();
}


Token FirstPass::parseFunctionCall(const Token& expr)
{
  TokenVector args;
  parseFuncallArgs(&args);

  return Token() << expr << (Token(expr.srcpos(), kParanOpen, kParanClose) << args);
}


Token FirstPass::parseParamCall(const Token& expr)
{
  if (expr.isSymbol()) {
    const Macro* macro =
        fScope->lookupMacro(expr.srcpos(), expr.idValue(), K(showAmbiguousSymDef));
    if (macro) {
      Token macroName = Token(expr.srcpos(), baseName(expr.idValue()));
      TokenVector exprs = parseMakeMacroCall(macroName, macro, K(isLocal));
      return (exprs.size() == 1 ? exprs[0] : Token() << exprs);
    }
  }

  return parseFunctionCall(expr);
}


Token FirstPass::parseSlice(const Token& expr)
{
  SrcPos startPos = fToken.srcpos();
  nextToken();

  if (fToken == kBracketClose) {
    nextToken();

    // the expression is an array type name
    return Token() << expr << Token(startPos, kBracketOpen, kBracketClose);
  }
  else {
    Token idx = parseExpr(!K(acceptComma));

    if (fToken != kBracketClose)
      HR_LOG(kError, fToken.srcpos(), E_MissingBracketClose) << "expected ']'";
    else
      nextToken();

    if (idx.isBinarySeq() && idx[1].tokenType() == kRange)
      return Token() << Token(startPos, "slice*")
                     << (Token(startPos, kParanOpen, kParanClose)
                         << expr << Token(idx.srcpos(), kComma) << idx[0]
                         << Token(idx.srcpos(), kComma) << idx[2]);
    else
      return Token() << Token(startPos, "slice")
                     << (Token(startPos, kParanOpen, kParanClose)
                         << expr << Token(idx.srcpos(), kComma) << idx);
  }
}


Token FirstPass::parseAccess(const Token& expr)
{
  if (fToken.isContinuation()) {
    return fToken;
  }
  else if (fToken == kParanOpen) {
    nextToken();

    return parseAccess(parseParamCall(expr));
  }
  else if (fToken == kBracketOpen) {
    return parseAccess(parseSlice(expr));
  }
  else if (fToken == kDot) {
    Token dotToken = fToken;

    nextToken();

    if (fToken != kSymbol) {
      HR_LOG(kError, fToken.srcpos(), E_SymbolExpected) << "expected SYMBOL";
      return scanUntilTopExprAndResume();
    }

    Token symToken = fToken;
    nextToken();

    if (fToken == kParanOpen) {
      Token call;
      if (!withRetryAfterMacroExpansion([&]() {
            call = parseAccess(symToken);
            return call.isContinuation() ? Retry::kRetry : Retry::kDone;
          }))
        return Token::sInvalid();

      if (call.isSeq() && call.count() == 2) {
        hr_assert(call[1].leftToken() == kParanOpen);

        auto& c = call[1].children();
        c.insert(c.begin(), expr);

        return call;
      }
      else {
        hr_invalid("unexpected call-syntax");
      }

      return Token() << expr << dotToken << call;
    }

    return parseAccess(Token() << expr << dotToken << symToken);
  }

  return expr;
}


Token FirstPass::parseGroup()
{
  Token expr = parseExpr(K(acceptComma));

  if (fToken != kParanClose) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanClose) << "expected closing ')'";
  }
  else
    nextToken();

  return expr;
}


bool FirstPass::parseExprListUntilBrace(TokenVector* result, bool endAtToplevelId,
                                        bool isLocal)
{
  for (;;) {
    if (fToken == kDefId || fToken == kExternId || fToken == kExportId ||
        fToken == kImportId || fToken == kIncludeId || fToken == kModuleId ||
        fToken == kLibraryId || fToken == kApplicationId) {
      if (!endAtToplevelId) {
        HR_LOG(kError, fToken.srcpos(), E_UnexpectedTopExpr)
            << "unexpected top level expression: " << fToken;
        return false;
      }
      else
        return true;
    }
    else if (fToken == kLetId) {
      if (!isLocal) {
        HR_LOG(kError, fToken.srcpos(), E_GlobalLet) << "'let' is not allowed here";
        parseDef(isLocal);
        continue;
      }

      TokenVector exprs = parseDef(isLocal);
      if (!exprs.empty())
        result->insert(result->end(), exprs.begin(), exprs.end());
    }
    else if (fToken == kBraceClose) {
      return true;
    }
    else if (fToken == kEOF) {
      return true;
    }
    else if (fToken == kWhenId) {
      Token expr = parseWhen(!isLocal);
      if (expr.isSet())
        result->push_back(expr);
    }
    else if (fToken == kSemicolon) {
      nextToken();
    }
    else {
      Token before = fToken;
      SrcPos startPos = fToken.srcpos();
      Token expr = parseExpr(K(acceptComma));

      if (!expr.isSet()) {
        if (expr == kInvalid) {
          HR_LOG(kError, startPos, E_UnexpectedToken)
              << "unexpected token while scanning block: " << before;
          return false;
        }
        else
          nextToken();
      }
      else
        result->push_back(expr);
    }
  }

  return false;
}


Token FirstPass::parseBlock()
{
  SrcPos startPos = fToken.srcpos();
  nextToken();

  TokenVector exprlist;
  parseExprListUntilBrace(&exprlist, !K(endAtToplevelId), K(isLocal));

  SrcPos bosp = fToken.srcpos();
  if (fToken != kBraceClose) {
    HR_LOG(kError, fToken.srcpos(), E_MissingBraceClose) << "expected '}'";

    if (startPos != fToken.srcpos())
      HR_LOG(kError, startPos, E_MissingBraceClose) << "beginning '{' was here";
  }
  else
    nextToken();

  return wrapInBlock(bosp, exprlist);
}


Token FirstPass::parseUnaryOp(const Token& inOpToken)
{
  Token opToken = inOpToken;
  nextToken();

  Token t;
  if (!withRetryAfterMacroExpansion([&]() {
        t = parseAtomicExpr();
        return t.isContinuation() ? Retry::kRetry : Retry::kDone;
      }))
    return Token::sInvalid();

  if (!t.isSet()) {
    HR_LOG(kError, opToken.srcpos(), E_UnexpectedToken) << "expected expression";
    return Token();
  }
  return Token() << opToken << (Token(opToken.srcpos(), kParanOpen, kParanClose) << t);
}


struct BasePatternParser {
protected:
  TokenVector parseConsequent(FirstPass* pass, bool mapToReq)
  {
    TokenVector result;

    Token mapToToken;
    if (mapToReq) {
      hr_assert(pass->fToken == kMapTo);
      mapToToken = pass->fToken;
      pass->nextToken();
    }

    SrcPos bodySrcpos = pass->fToken.srcpos();
    Token body = pass->parseExpr(K(acceptComma));
    if (body.isSet()) {
      if (mapToReq)
        result.push_back(mapToToken);
      result.push_back(body);
    }
    else {
      HR_LOG(kError, bodySrcpos, E_MissingExpr)
          << "Missing expression for select pattern";
    }
    return result;
  }
};


struct SelectPatternParser : public BasePatternParser {
  SelectPatternParser()
      : fElseSeen(false)
  {
  }

  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken != kPipe) {
      HR_LOG(kError, pass->fToken.srcpos(), E_ExpectedPipe) << "expect '|'";
      pass->scanUntilBrace();
      return false;
    }
    Token pipeToken = pass->fToken;
    pass->nextToken();

    if (pass->fToken == kElseId) {
      bool ignore = false;
      Token elseToken = pass->fToken;

      if (fElseSeen) {
        HR_LOG(kError, pass->fToken.srcpos(), E_RedefinedPattern)
            << "'else' pattern redefined";
        ignore = true;
      }
      fElseSeen = true;
      pass->nextToken();

      if (pass->fToken == kMapTo) {
        HR_LOG(kWarn, pass->fToken.srcpos(), E_UnexpectedMapTo)
            << "Misplaced '->' after 'else' in select";
        pass->nextToken();
      }

      TokenVector consq = parseConsequent(pass, !K(mapToReq));
      if (!ignore && !consq.empty())
        result << (Token() << pipeToken << elseToken << consq);
    }
    else {
      TokenVector pattern;
      for (;;) {
        if (pass->fToken == kEOF)
          return false;

        if (fElseSeen) {
          HR_LOG(kError, pass->fToken.srcpos(), E_ElseNotLastPattern)
              << "'else' must be last pattern";
          pass->scanUntilBrace();
          return false;
        }
        else {
          Token test = pass->parseExpr(!K(acceptComma));
          if (test.isSet()) {
            pattern.push_back(test);

            if (pass->fToken == kComma) {
              pattern.push_back(pass->fToken);
              pass->nextToken();
            }
            else if (pass->fToken == kMapTo)
              break;
            else {
              HR_LOG(kError, pass->fToken.srcpos(), E_BadPatternList)
                  << "unexpected token: " << pass->fToken;
              return false;
            }
          }
          else {
            HR_LOG(kError, pass->fToken.srcpos(), E_UnexpectedToken)
                << "unexpected token in select: " << pass->fToken;
            return false;
          }
        }
      }

      TokenVector consq = parseConsequent(pass, K(mapToReq));
      if (!pattern.empty() && !consq.empty()) {
        result << (Token() << pipeToken
                           << (pattern.size() == 1 ? pattern[0] : (Token() << pattern))
                           << consq);
      }
    }

    return true;
  }

  bool fElseSeen;
};


Token FirstPass::parseSelect()
{
  hr_assert(fToken == kSelectId);
  Token selectToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  TokenVector args;
  parseFuncallArgs(&args);

  if (fToken != kPipe) {
    HR_LOG(kError, fToken.srcpos(), E_MissingPipe) << "expected '|'";
    return scanUntilTopExprAndResume();
  }

  Token patterns = Token(fToken.srcpos(), kBraceOpen, kBraceClose);
  parseChoiceSequence(SelectPatternParser(), kPipe, patterns);

  return Token() << selectToken << (Token(paranPos, kParanOpen, kParanClose) << args)
                 << patterns;
}


struct MatchPatternParser : public BasePatternParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken != kPipe) {
      HR_LOG(kError, pass->fToken.srcpos(), E_ExpectedPipe) << "expect '|'";
      pass->scanUntilBrace();
      return false;
    }
    Token pipeToken = pass->fToken;
    pass->nextToken();

    if (pass->fToken != kSymbol && pass->fToken != kColon) {
      HR_LOG(kError, pass->fToken.srcpos(), E_SymbolExpected)
          << "variable name or ':' expected";
      pass->scanUntilBrace();
      return false;
    }

    Token varToken;
    if (pass->fToken == kSymbol) {
      varToken = pass->fToken;
      pass->nextToken();
    }

    if (pass->fToken != kColon) {
      HR_LOG(kError, pass->fToken.srcpos(), E_ColonExpected)
          << "match pattern require a type specification";
      pass->scanUntilBrace();
      return false;
    }
    Token colonToken = pass->fToken;
    pass->nextToken();

    Token matchType = pass->parseTypeSpec(K(onlyNestedConstr), K(needParans));
    if (!matchType.isSet()) {
      pass->scanUntilBrace();
      return false;
    }

    if (pass->fToken != kMapTo) {
      HR_LOG(kError, pass->fToken.srcpos(), E_BadPatternList) << "expected '->'";
      pass->scanUntilBrace();
      return false;
    }

    TokenVector consq = parseConsequent(pass, K(mapToReq));
    if (colonToken.isSet() && matchType.isSet() && !consq.empty()) {
      if (varToken.isSet())
        result << (Token() << pipeToken
                           << (Token() << varToken << colonToken << matchType) << consq);
      else
        result << (Token() << pipeToken << (Token() << colonToken << matchType) << consq);
    }
    return true;
  }
};


Token FirstPass::parseMatch()
{
  hr_assert(fToken == kMatchId);
  Token matchToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  TokenVector args;
  parseFuncallArgs(&args);

  if (fToken != kPipe) {
    HR_LOG(kError, fToken.srcpos(), E_MissingPipe) << "expected '|'";
    return scanUntilTopExprAndResume();
  }

  Token patterns = Token(fToken.srcpos(), kBraceOpen, kBraceClose);
  parseChoiceSequence(MatchPatternParser(), kPipe, patterns);

  return Token() << matchToken << (Token(paranPos, kParanOpen, kParanClose) << args)
                 << patterns;
}


Token FirstPass::parseForTestClause()
{
  if (fToken == kSymbol) {
    Token symToken = fToken;
    nextToken();

    Token type;
    Token colonToken;

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      type = parseTypeSpec(K(onlyNestedConstr), K(needParans));
      if (!type.isSet()) {
        HR_LOG(kError, pos, E_MissingType) << "type expression expected";
        type = makeAnySymbol(pos);
      }
    }
    else if (isPrescannedTypeToken(fToken)) {
      colonToken = fToken[0];
      type = fToken[1];
      nextToken();
    }

    if (fToken == kIn) {
      Token inToken = fToken;
      nextToken();

      Token collToken = parseExpr(!K(acceptComma));
      if (!collToken.isSet()) {
        HR_LOG(kError, fToken.srcpos(), E_MissingRHExpr)
            << "unexpected token: " << fToken;
        scanUntilNextParameter();
        return Token();
      }

      Token varClause;
      if (colonToken.isSet() && type.isSet())
        varClause = Token() << symToken << colonToken << type;
      else
        varClause = symToken;

      Token subexpr;
      subexpr << varClause << inToken << collToken;

      return Token(symToken.srcpos(), kParanOpen, kParanClose) << subexpr;
    }
    else {
      HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken)
          << "'in' keyword expected: " << fToken;
      scanUntilNextParameter();
    }
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken)
        << "Symbol expected in for clause: " << fToken;
    scanUntilNextParameter();
  }

  return Token();
}


Token FirstPass::parseFor()
{
  hr_assert(fToken == kForId);
  Token forToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }
  nextToken();

  Token test = parseForTestClause();
  if (fToken != kParanClose) {
    HR_LOG(kError, fToken.srcpos(), E_ParamMissParanClose) << "Syntax error, missing ')'";
  }
  else
    nextToken();

  Token body = parseExpr(K(acceptComma));

  Token elseToken;
  Token alternate;
  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();

    alternate = parseExpr(!K(acceptComma));
  }

  if (body.isSet()) {
    Token result = Token() << forToken << test << body;
    if (elseToken.isSet() && alternate.isSet())
      result << elseToken << alternate;
    return result;
  }
  return Token() << Token(forToken.srcpos(), "lang|unspecified");
}


Token FirstPass::parseWhile()
{
  hr_assert(fToken == kWhileId);
  Token whileToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  Token test = parseExpr(!K(acceptComma));
  if (fToken != kParanClose) {
    HR_LOG(kError, fToken.srcpos(), E_ParamMissParanClose) << "Syntax error, missing ')'";
  }
  else
    nextToken();

  Token cond = test.isSeq() ? Token(paranPos, kParanOpen, kParanClose) << test.children()
                            : Token(paranPos, kParanOpen, kParanClose) << test;

  Token body = parseExpr(K(acceptComma));

  Token elseToken;
  Token alternate;
  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();

    alternate = parseExpr(!K(acceptComma));
  }

  if (body.isSet()) {
    Token result = Token() << whileToken << cond << body;
    if (elseToken.isSet() && alternate.isSet()) {
      result << elseToken << alternate;
    }
    return result;
  }
  return Token() << Token(whileToken.srcpos(), "lang.unspecified");
}


Token FirstPass::parseUnitNumber(const Token& token)
{
  Token number = token;
  nextToken();
  return number;
}


Token FirstPass::parseExplicitTypedNumber(const Token& token)
{
  Token number = token;

  if (fToken == kColon) {
    Token colonToken = fToken;
    nextToken();

    SrcPos typePos = fToken.srcpos();
    Token type = parseTypeSpec(K(onlyNestedConstr), K(needParans));
    if (!type.isSet()) {
      HR_LOG(kError, typePos, E_MissingType) << "expected type specifier";
      return number;
    }

    return Token() << number << colonToken << type;
  }
  else if (isPrescannedTypeToken(fToken)) {
    auto result = Token() << number << fToken.children();
    nextToken();
    return result;
  }

  return number;
}


Token FirstPass::parseAtomicExpr0()
{
  switch (fToken.tokenType()) {
  case kBool:
  case kString:
  case kChar:
  case kKeyword:
  case kNilId:
  case kEofId:
  case kSeqExpr:
  case kNestedExpr: {
    Token t = fToken;
    nextToken();
    return t;
  }
  default:;
  }

  return Token();
}

Token FirstPass::parseAtomicExpr()
{
  switch (fToken.tokenType()) {
  case kUninitialized: {
    auto t = fToken;
    nextToken();
    return t;
  }

  case kInt:
  case kUInt:
  case kFloat:
  case kRational: return parseExplicitTypedNumber(parseUnitNumber(fToken));

  case kBool:
  case kString:
  case kChar:
  case kKeyword:
  case kNilId:
  case kEofId:
  case kSeqExpr:
  case kNestedExpr: return parseAccess(parseAtomicExpr0());

  case kIfId: return parseIf();
  case kFunctionId: return parseAnonFun();
  case kNotId:  // unary not operator
  case kMinus:  // unary negate
    return parseUnaryOp(fToken);

  case kWhenId: return parseWhen(!K(isTopLevel));
  case kSelectId: return parseSelect();
  case kMatchId: return parseMatch();
  case kForId: return parseFor();
  case kWhileId: return parseWhile();

  case kLetId:
    HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken) << "Unexpected let token";
    break;

  case kSymbol:
  case kDot:
  case kQuote: return parseAccess(parseSymbolOrSimpleType(fToken));

  case kLiteralVectorOpen: return parseAccess(parseLiteralVector());

  case kLiteralArrayOpen: return parseAccess(parseLiteralArray());

  case kParanOpen: nextToken(); return parseAccess(parseGroup());
  case kBraceOpen: return parseAccess(parseBlock());

  case kContinuationExpr: nextToken(); return parseAtomicExpr();

  default:;
  }

  return Token();
}


Token FirstPass::makeAssignToken(const Token& expr1, const Token& expr2,
                                 const SrcPos& op1Srcpos) const
{
  if (expr1.isSymFuncall()) {
    // rename the function call in expr to name! and append expr2 as last
    // parameter to expr1's parameter list
    return Token() << Token(expr1[0].srcpos(), expr1[0].idValue() + "!")
                   << (Token(op1Srcpos, kParanOpen, kParanClose)
                       << expr1[1].children() << Token(op1Srcpos, kComma) << expr2);
  }
  else {
    return Token() << expr1 << Token(op1Srcpos, kAssign) << expr2;
  }
}


Token FirstPass::makeAssignToken(const TokenVector& exprs, const Token& expr2,
                                 const SrcPos& op1Srcpos, bool hasRest) const
{
  if (exprs.size() > 1) {
    Token block = Token(op1Srcpos, kBraceOpen, kBraceClose);

    Token tempSymToken;
    if (expr2 == kSymbol) {
      tempSymToken = expr2;
    }
    else {
      tempSymToken = Token::newUniqueSymbolToken(op1Srcpos, "value");

      block << (Token() << Token(op1Srcpos, kLetId) << tempSymToken
                        << Token(op1Srcpos, kAssign) << expr2);
    }

    for (size_t i = 0; i < exprs.size(); i++) {
      const Token& expr = exprs[i];

      zstring funcName = (i == exprs.size() - 1 && hasRest ? "slice-rest" : "slice");
      Token sliceExpr = Token() << Token(op1Srcpos, funcName)
                                << (Token(op1Srcpos, kParanOpen, kParanClose)
                                    << tempSymToken << Token(op1Srcpos, kComma)
                                    << Token(op1Srcpos, kInt, (int)i));
      Token retv;
      if (expr.isSymFuncall()) {
        // rename the function call in expr to name! and append expr2 as last
        // parameter to expr1's parameter list
        retv =
            Token() << Token(expr[0].srcpos(), expr[0].idValue() + "!")
                    << (Token(op1Srcpos, kParanOpen, kParanClose)
                        << expr[1].children() << Token(op1Srcpos, kComma) << sliceExpr);
      }
      else {
        retv = Token() << expr << Token(op1Srcpos, kAssign) << sliceExpr;
      }

      block << retv;
    }

    return block;
  }

  hr_assert(exprs.size() == 1);
  return makeAssignToken(exprs[0], expr2, op1Srcpos);
}


Token FirstPass::makeBinaryToken(const Token& expr1, OperatorType op1, const Token& expr2,
                                 const SrcPos& op1Srcpos) const
{
  if (op1 == kOpAssign)
    return makeAssignToken(expr1, expr2, op1Srcpos);
  else if (op1 == kOpRange && expr2.isBinarySeq(kBy)) {
    return Token() << expr1 << Token(op1Srcpos, operatorToTokenType(op1)) << expr2[0]
                   << expr2[1] << expr2[2];
  }
  else if (op1 == kOpBy && expr1.isBinarySeq(kRange)) {
    return Token() << expr1[0] << expr1[1] << expr1[2]
                   << Token(op1Srcpos, operatorToTokenType(op1)) << expr2;
  }
  else
    return Token() << expr1 << Token(op1Srcpos, operatorToTokenType(op1)) << expr2;
}


bool FirstPass::isRightOperator(OperatorType op1) const
{
  return (op1 == kOpAssign);
}


int FirstPass::weightOperator(OperatorType op1) const
{
  switch (op1) {
  case kOpMapTo: return 10;

  case kOpLogicalAnd:
  case kOpLogicalOr: return 20;

  case kOpIsa: return 30;

  case kOpEqual:
  case kOpUnequal:
  case kOpLess:
  case kOpLessEqual:
  case kOpGreater:
  case kOpGreaterEqual: return 40;

  case kOpCompare: return 45;

  case kOpIn: return 50;

  case kOpRange:
  case kOpBy:
  case kOpConcat: return 60;

  case kOpBitAnd:
  case kOpBitOr:
  case kOpBitXor: return 70;

  case kOpShiftLeft:
  case kOpShiftRight: return 80;

  case kOpFold:
  case kOpPlus:
  case kOpMinus: return 90;

  case kOpMultiply:
  case kOpDivide:
  case kOpRem:
  case kOpMod: return 100;

  case kOpExponent: return 110;

  case kOpAs: return 200;

  case kOpAssign: return 999999;

  case kOpInvalid: hr_invalid("invalid operator");
  }

  return 999999;
}


bool FirstPass::isOpWeightAbove(OperatorType op1, OperatorType op2) const
{
  return weightOperator(op1) > weightOperator(op2);
}


Token FirstPass::parseExprRec(const TokenVector& exprs, OperatorType op1,
                              const SrcPos& op1Srcpos, bool hasRest)
{
  hr_assert(exprs.size() > 0);

  if (op1 == kOpInvalid) {
    if (exprs.size() > 1)
      HR_LOG(kError, exprs[0].srcpos(), E_OrphanedMultiValue)
          << "Multiple variables without assign";
    if (hasRest)
      HR_LOG(kError, exprs[0].srcpos(), E_OrphanedRestInd)
          << "Multiple variables (rest notation) without assign";
    return exprs[0];
  }

  nextToken();

  SrcPos before2ndPos = fToken.srcpos();
  Token expr2;

  if (op1 == kOpAs)
    expr2 = parseTypeSpec(K(onlyNestedConstr), K(needParans));
  else {
    if (!withRetryAfterMacroExpansion([&]() {
          expr2 = parseAtomicExpr();
          return expr2.isContinuation() ? Retry::kRetry : Retry::kDone;
        }))
      return Token::sInvalid();
  }
  OperatorType op2 = tokenTypeToOperator(fToken.tokenType());
  SrcPos op2Srcpos = fToken.srcpos();

  if (!expr2.isSet()) {
    HR_LOG(kError, before2ndPos, E_MissingRHExpr) << "no right hand expression";
    return exprs[0];
  }

  if (op2 == kOpInvalid) {
    if (op1 == kOpAssign) {
      return makeAssignToken(exprs, expr2, op1Srcpos, hasRest);
    }
    else {
      if (exprs.size() > 1) {
        HR_LOG(kError, exprs[0].srcpos(), E_BadLHExpr)
            << "Multiple left hand variables only allowed with assignments.";
        return Token();
      }
      return Token() << exprs[0] << Token(op1Srcpos, operatorToTokenType(op1)) << expr2;
    }
  }
  else {
    if (exprs.size() > 1) {
      HR_LOG(kError, exprs[0].srcpos(), E_BadLHExpr)
          << "Multiple left hand variables only allowed with assignments.";
      return Token();
    }

    if (!isRightOperator(op1) && isOpWeightAbove(op1, op2))
      return parseExprRec(
          makeBinaryToken(exprs[0], op1, expr2, op1Srcpos).toTokenVector(), op2,
          op2Srcpos, !K(hasRest));
    else
      return makeBinaryToken(
          exprs[0], op1, parseExprRec(expr2.toTokenVector(), op2, op2Srcpos, !K(hasRest)),
          op1Srcpos);
  }
}


Token FirstPass::parseExpr(bool acceptComma)
{
  Token expr1;
  if (!withRetryAfterMacroExpansion([&]() {
        expr1 = parseAtomicExpr();
        return expr1.isContinuation() ? Retry::kRetry : Retry::kDone;
      }))
    return Token::sInvalid();

  if (expr1.isSet()) {
    TokenVector exprs;
    bool hasRest = false;

    exprs.push_back(expr1);

    if (acceptComma) {
      while (fToken == kComma) {
        nextToken();

        Token expr2;
        if (!withRetryAfterMacroExpansion([&]() {
              expr2 = parseAtomicExpr();
              return expr2.isContinuation() ? Retry::kRetry : Retry::kDone;
            }))
          return Token::sInvalid();
        if (expr2.isSet())
          exprs.push_back(expr2);
      }

      if (fToken == kEllipsis) {
        hasRest = true;
        nextToken();
      }
    }

    OperatorType op1 = tokenTypeToOperator(fToken.tokenType());
    SrcPos op1Srcpos = fToken.srcpos();

    if (op1 != kOpInvalid)
      return parseExprRec(exprs, op1, op1Srcpos, hasRest);
  }
  return expr1;
}


void FirstPass::parseTopExprUntilBrace(TokenVector* result)
{
  while (fToken != kBraceClose) {
    if (fToken == kEOF)
      break;
    TokenVector topexprs = parseTop();
    if (isUsable(topexprs))
      result->insert(result->end(), topexprs.begin(), topexprs.end());
  }

  if (fToken == kBraceClose)
    nextToken();
}


TokenVector FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken == kBraceOpen) {
      nextToken();

      TokenVector exprs;
      parseTopExprUntilBrace(&exprs);

      return exprs;
    }

    return parseTop();
  }
  return parseExpr(!K(acceptComma)).toTokenVector();
}


Token FirstPass::multiExprsToBlock(const TokenVector& exprs)
{
  if (exprs.size() > 1)
    return wrapInBlock(exprs[0].srcpos(), exprs);
  else if (exprs.size() == 1)
    return exprs[0];
  else
    return Token();
}


Token FirstPass::wrapInBlock(const SrcPos& srcpos, const TokenVector& exprs)
{
  return Token(srcpos, kBraceOpen, kBraceClose) << exprs;
}


bool FirstPass::scanBlock(bool isTopLevel)
{
  if (fToken == kBraceOpen) {
    SrcPos startPos = fToken.srcpos();
    int braceCount = 0;
    for (;;) {
      if (fToken == kEOF) {
        HR_LOG(kError, fToken.srcpos(), E_UnexpectedEOF) << "unfinished when component";
        if (startPos != fToken.srcpos())
          HR_LOG(kError, startPos, E_MissingBraceClose) << "beginning '{' was here";
        return false;
      }

      if (fToken == kBraceOpen) {
        braceCount++;
      }
      else if (fToken == kBraceClose) {
        braceCount--;
        if (braceCount == 0) {
          nextToken();
          return true;
        }
      }

      nextToken();
    }

    return true;
  }

  if (isTopLevel)
    parseTop();
  else
    parseExpr(K(acceptComma));
  return true;
}


Token FirstPass::parseWhen(bool isTopLevel)
{
  Token result;
  result << fToken;

  nextToken();

  bool inclConsequent = true;
  bool inclAlternate = true;

  if (fToken.isSymbol()) {
    if (fToken == Compiler::ignoreToken) {
      inclConsequent = false;
    }
    else if (fToken == Compiler::includeToken) {
      inclAlternate = false;
    }
    else {
      HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken)
          << "only 'ignore' or 'include' are valid symbols here: " << fToken;
      HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken) << "assume 'ignore'";

      inclConsequent = false;
    }
    nextToken();
  }
  else if (fToken == kParanOpen) {
    SrcPos paranPos = fToken.srcpos();
    nextToken();

    Token test = parseExpr(!K(acceptComma));
    if (fToken != kParanClose) {
      HR_LOG(kError, fToken.srcpos(), E_ParamMissParanClose) << "missing ')'";
      if (fToken == kBraceOpen) {
        // try to continue with this
      }
      else
        return scanUntilTopExprAndResume();
    }
    else
      nextToken();

    if (fEvaluateExprs) {
      TokenEvalContext ctx(*fCompiler.configVarRegistry());
      Token p = ctx.evalToken(test);
      if (p.isBool()) {
        inclConsequent = p.boolValue();
        inclAlternate = !p.boolValue();
      }
      else {
        HR_LOG(kWarn, p.srcpos(), E_BadType)
            << "when-expression did not evaluate to boolean. Treat it as false";
        inclConsequent = false;
        inclAlternate = true;
      }
    }
    else
      result << (Token(paranPos, kParanOpen, kParanClose) << test);
  }
  else if (fToken == kBraceOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen)
        << "missing parameters or key for 'when' clause";
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "assume 'ignore' here";
    // try to continue with this

    inclConsequent = false;
    inclAlternate = true;
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    scanUntilTopExprAndResume();
    return Token();
  }


  Token consequent;
  Token alternate;
  Token elseToken;

  if (inclConsequent) {
    ValueSaver<bool> keep(fEvaluateExprs, inclConsequent);
    consequent = multiExprsToBlock(parseTopOrExprList(isTopLevel));
  }
  else {
    if (!scanBlock(isTopLevel))
      return Token();
  }

  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();

    if (inclAlternate) {
      ValueSaver<bool> keep(fEvaluateExprs, inclAlternate);
      alternate = multiExprsToBlock(parseTopOrExprList(isTopLevel));
    }
    else {
      if (!scanBlock(isTopLevel))
        return Token();
    }
  }

  if (inclConsequent && inclAlternate) {
    result << consequent;
    if (alternate.isSet())
      result << elseToken << alternate;

    return result;
  }
  else if (inclConsequent)
    return consequent;
  else if (inclAlternate)
    return alternate;

  hr_invalid("");
  return Token();
}


Token FirstPass::parseWith()
{
  hr_assert(fToken == kWithId);
  Token withToken = fToken;
  nextToken();

  if (fToken == kNamespaceId || fToken == kNsId) {
    Token nsToken = Token(fToken.srcpos(), kNamespaceId);
    nextToken();

    auto qSymbol = parseQualifiedName(false);
    if (qSymbol.empty()) {
      HR_LOG(kError, fToken.srcpos(), E_SymbolExpected) << "expected SYMBOL";
      return scanUntilTopExprAndResume();
    }

    Token nsNameToken = qualifyIdToken(qSymbol);

    if (fToken != kBraceOpen) {
      HR_LOG(kError, fToken.srcpos(), E_MissingBraceOpen) << "expected '{'";
      return scanUntilTopExprAndResume();
    }

    Token code;
    {
      ModuleHelper modHelper(this, nsNameToken.idValue(), K(setName));

      code = wrapInBlock(fToken.srcpos(), parseTopOrExprList(K(isTopLevel)));
    }

    if (code.isSet())
      return Token() << withToken << nsToken << nsNameToken << code;
    else
      return Token();
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken)
        << "unknown scope in 'with': " << fToken;
    return scanUntilTopExprAndResume();
  }
}


TokenVector FirstPass::parseExtern()
{
  hr_assert(fToken == kExternId);
  Token externToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '(' after extern";
    return scanUntilTopExprAndResume().toTokenVector();
  }

  nextToken();
  if (fToken != kString) {
    HR_LOG(kError, fToken.srcpos(), E_StringExpected) << "expected external linkage name";
    return scanUntilTopExprAndResume().toTokenVector();
  }
  Token linkage = fToken;
  String linkageType = fToken.stringValue();

  nextToken();
  if (fToken != kParanClose) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanClose) << "expected ')'";
    return scanUntilTopExprAndResume().toTokenVector();
  }
  nextToken();

  if (fToken != kBraceOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingBraceOpen) << "expected '{'";
    return scanUntilTopExprAndResume().toTokenVector();
  }
  nextToken();

#if 0
  if (linkageType == String("C")) {
    ExternCParser externc{*this};
    return externc.parseBlock();
  }
#endif

  HR_LOG(kError, fToken.srcpos(), E_UnknownLinkage)
      << "Unknown linkage type: " << linkageType;
  return scanUntilTopExprAndResume().toTokenVector();
}


TokenVector FirstPass::parseVarDef(const Token& defToken, const Token& vizToken,
                                   const Token& tagToken, bool isLocal, VizType vizType)
{
  Token keepTagToken = tagToken;

  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "Missing name";
    return scanUntilTopExprAndResume().toTokenVector();
  }
  Token symbolToken = qualifyIdToken(qSymbol);

  if (!isLocal)
    registerSymbolForExport(symbolToken.idValue(), vizType);

  return parseVarDef2(defToken, vizToken, keepTagToken, symbolToken, isLocal, Token());
}


Token FirstPass::evaluateConfigExpr(const Token& initExpr)
{
  TokenEvalContext ctx(*fCompiler.configVarRegistry());
  return ctx.evalToken(initExpr);
}


TokenVector FirstPass::parseVarDef2(const Token& defToken, const Token& vizToken,
                                    const Token& tagToken, const Token& symbolToken,
                                    bool isLocal, const Token& linkage)
{
  Token assignToken;
  Token sym = symbolToken;

  std::vector<Token> leftHands;
  std::vector<Token> leftHandSyms;

  Token ellipsisToken;
  bool isDone = false;
  while (!isDone) {
    Token colonToken;
    Token type;

    hr_assert(!ellipsisToken.isSet());

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      type = parseTypeSpec(K(onlyNestedConstr), K(needParans));
      if (!type.isSet()) {
        HR_LOG(kError, pos, E_MissingType) << "type expression expected";
        type = makeAnySymbol(pos);
      }
    }
    else if (isPrescannedTypeToken(fToken)) {
      colonToken = fToken[0];
      type = fToken[1];
      nextToken();
    }

    if (fToken == kEllipsis) {
      ellipsisToken = fToken;
      nextToken();
    }

    Token docString = parseOptDocString();

    Token vardefExpr;
    vardefExpr << defToken;
    if (vizToken.isSet())
      vardefExpr << vizToken;
    if (linkage.isSet())
      vardefExpr << linkage;
    if (tagToken.isSet())
      vardefExpr << tagToken;

    vardefExpr << sym;

    if (type.isSet())
      vardefExpr << colonToken << type;

    if (docString.isSet())
      vardefExpr << docString;

    leftHands.push_back(vardefExpr);
    leftHandSyms.push_back(sym);

    if (fToken == kComma) {
      if (ellipsisToken.isSet()) {
        HR_LOG(kError, fToken.srcpos(), E_InvalidRestParam)
            << "Rest var declaration must be last in sequence";
        return scanUntilTopExprAndResume().toTokenVector();
      }

      nextToken();
      if (fToken != kSymbol) {
        HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "Missing name";
        return scanUntilTopExprAndResume().toTokenVector();
      }
      sym = fToken;
      nextToken();
    }
    else
      isDone = true;
  }

  Token initExpr;
  if (fToken == kAssign) {
    assignToken = fToken;
    nextToken();

    SrcPos pos = fToken.srcpos();
    if (!withRetryAfterMacroExpansion([&]() {
          initExpr = parseExpr(!K(acceptComma));
          return initExpr.isContinuation() ? Retry::kRetry : Retry::kDone;
        }))
      return {Token::sInvalid()};

    if (!initExpr.isSet())
      HR_LOG(kError, pos, E_MissingRHExpr) << "no value in var init";
  }

  TokenVector result;

  Token initValueSym;
  if (leftHands.size() > 1) {
    if (initExpr == kSymbol) {
      initValueSym = initExpr;
    }
    else {
      initValueSym = Token::newUniqueSymbolToken(symbolToken.srcpos(), "value");

      Token multiInitValueToken;
      multiInitValueToken << defToken;
      if (vizToken.isSet())
        multiInitValueToken << vizToken;
      if (tagToken.isSet())
        multiInitValueToken << tagToken;

      multiInitValueToken << initValueSym << assignToken << initExpr;
      result.push_back(multiInitValueToken);
    }
  }


  for (size_t i = 0; i < leftHands.size(); i++) {
    Token vardefExpr = leftHands[i];
    Token vardefSym = leftHandSyms[i];

    Token effInitExpr;
    if (leftHands.size() > 1) {
      hr_assert(initValueSym.isSet());

      zstring funcName =
          (i == leftHands.size() - 1 && ellipsisToken.isSet() ? "slice-rest" : "slice");

      effInitExpr << Token(vardefSym.srcpos(), funcName)
                  << (Token(vardefSym.srcpos(), kParanOpen, kParanClose)
                      << initValueSym << Token(vardefSym.srcpos(), kComma)
                      << Token(initExpr.srcpos(), kInt, (int)i));
    }
    else
      effInitExpr = initExpr;

    if (tagToken == Compiler::configToken) {
      if (fEvaluateExprs) {
        if (!effInitExpr.isSet()) {
          HR_LOG(kError, vardefSym.srcpos(), E_DefNoInitValue)
              << "Config variable '" << symbolToken << "' without default value";

          // if no default value is given assume ''
          effInitExpr = Token(vardefSym.srcpos(), kString, "");
          if (!assignToken.isSet())
            assignToken = Token(vardefSym.srcpos(), kAssign);
        }
        fCompiler.configVarRegistry()->registerValue(vardefSym.idValue(),
                                                     evaluateConfigExpr(effInitExpr));
        // even if we have to evaluate the config var expression, we have to
        // keep the constructed expr since config-vars can be used like
        // normal const-vars by code
      }
    }

    if (effInitExpr.isSet())
      vardefExpr << assignToken << effInitExpr;

    result.push_back(vardefExpr);
  }

  return result;
}


Token FirstPass::parseCharDef(const Token& defToken, const Token& vizToken,
                              VizType vizType)
{
  Token tagToken = fToken;

  nextToken();

  if (!fToken.isCharOrUnitName()) {
    HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "missing char name";
    return scanUntilTopExprAndResume();
  }
  Token charNameToken =
      (fToken == kSymbol ? fToken : Token(fToken.srcpos(), kSymbol, fToken.toString()));

  nextToken();

  Token docString = parseOptDocString();

  Token assignToken = fToken;
  if (fToken != kAssign) {
    HR_LOG(kError, fToken.srcpos(), E_DefNoInitValue) << "expected '='";
    assignToken = Token(fToken.srcpos(), kAssign);
  }
  else
    nextToken();

  Token codePointToken = fToken;
  int codePoint = 0xffff;

  if (fToken != kInt && fToken != kUInt) {
    HR_LOG(kError, fToken.srcpos(), E_DefInitUnexpToken) << "expected INTEGER";
    codePointToken = Token(fToken.srcpos(), kUInt, 0xffff);
  }
  else {
    codePoint = fToken.intValue();
    if (codePoint < 0 || codePoint > 0x10FFFF) {
      HR_LOG(kError, fToken.srcpos(), E_BadCharValue) << "invalid expected INTEGER";

      codePointToken = Token(fToken.srcpos(), kUInt, 0xffff);
      codePoint = 0xffff;
    }
    nextToken();
  }

  if (fEvaluateExprs) {
    fCompiler.charRegistry()->registerValue(charNameToken.idValue(), codePoint);
    return Token();
  }
  else {
    registerSymbolForExport(charNameToken.idValue(), vizType, Scope::kChar);

    Token result = Token() << defToken;
    if (vizToken.isSet())
      result << vizToken;
    result << tagToken << charNameToken;
    if (docString.isSet())
      result << docString;

    result << assignToken << codePointToken;
    return result;
  }
}


Token FirstPass::parseWhereClause()
{
  hr_assert(fToken == kWhereId);

  Token whereToken = fToken;
  nextToken();

  TokenVector constraints;
  Token delayedCommaToken;

  for (;;) {
    if (fToken == kEOF) {
      HR_LOG(kError, fToken.srcpos(), E_UnexpectedEOF)
          << "unexpected eof while scanning 'where' clause";
      return Token();
    }

    if (fToken != kSymbol) {
      HR_LOG(kError, fToken.srcpos(), E_SymbolExpected) << "missing type name";
      return Token();
    }
    Token symToken = fToken;
    nextToken();

    Token opToken = fToken;
    OperatorType op1 = tokenTypeToOperator(fToken.tokenType());

    if (op1 == kOpEqual || op1 == kOpUnequal || op1 == kOpLess || op1 == kOpLessEqual ||
        op1 == kOpGreater || op1 == kOpGreaterEqual || op1 == kOpIn) {
      nextToken();

      Token constrExpr = parseExpr(!K(acceptComma));
      if (constrExpr.isSet()) {
        if (delayedCommaToken.isSet()) {
          constraints.push_back(delayedCommaToken);
          delayedCommaToken = Token();
        }
        Token constrToken = Token() << symToken << opToken << constrExpr;
        constraints.push_back(constrToken);
      }
    }
    else if (op1 == kOpIsa) {
      nextToken();

      Token typeConstraint = parseTypeSpec(!K(onlyNestedConstr), K(needParans));
      if (delayedCommaToken.isSet()) {
        constraints.push_back(delayedCommaToken);
        delayedCommaToken = Token();
      }
      Token constrToken = Token() << symToken << opToken << typeConstraint;
      constraints.push_back(constrToken);
    }
    else {
      HR_LOG(kError, fToken.srcpos(), E_SymbolExpected)
          << "unexpected operator in where clause";
      return Token();
    }

    if (fToken == kComma) {
      delayedCommaToken = fToken;
      nextToken();
    }
    else
      break;
  }

  if (!constraints.empty())
    return Token() << whereToken << constraints;
  return Token();
}


Token FirstPass::parseFunctionDef(const Token& defToken, const Token& vizToken,
                                  const Token& tagToken, const Token& symToken,
                                  const Token& linkage)
{
  hr_assert(fToken == kParanOpen);
  Token paranOpenToken = fToken;

  Token result;
  result << defToken;
  if (vizToken.isSet())
    result << vizToken;
  if (linkage.isSet())
    result << linkage;
  if (tagToken.isSet())
    result << tagToken;

  result << symToken;

  TokenVector params;
  if (parseFunctionsParams(&params)) {
    Token returnTyToken;
    Token returnType;
    Token whereClause;

    if (fToken == kMapTo) {
      returnTyToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      returnType = parseTypeSpec(K(onlyNestedConstr), !K(needParans));
      if (!returnType.isSet()) {
        HR_LOG(kError, pos, E_MissingType) << "type expression expected";
        returnType = makeAnySymbol(pos);
      }
    }
    else {
      returnTyToken = Token(fToken.srcpos(), kMapTo);
      returnType = makeAnySymbol(fToken.srcpos());
    }

    if (fToken == kWhereId)
      whereClause = parseWhereClause();

    SrcPos bodyPos = fToken.srcpos();
    Token body;
    Token docString;
    if (fToken == kEllipsis) {
      body = fToken;
      nextToken();

      docString = parseOptDocString();
    }
    else {
      docString = parseOptDocString();

      body = parseExpr(!K(acceptComma));
      if (!body.isSet()) {
        HR_LOG(kError, bodyPos, E_MissingBody) << "expected function body";
        return Token();
      }
    }

    result << (Token(paranOpenToken.srcpos(), kParanOpen, kParanClose) << params);
    if (returnTyToken.isSet())
      result << returnTyToken << returnType;

    if (whereClause.isSet())
      result << whereClause;

    if (docString.isSet())
      result << docString;

    result << body;

    return result;
  }

  return Token();
}


void FirstPass::registerSymbolForExport(const String& sym, VizType vizType,
                                        Scope::ScopeDomain domain)
{
  if (vizType != kUnset) {
    String fullId = (isQualified(sym) ? sym : qualifyId(currentModuleName(), sym));
    fScope->registerSymbolForExport(domain, fullId, vizType, !K(isFinal));
  }
}


TokenVector FirstPass::parseFunctionOrVarDef(const Token& defToken, const Token& vizToken,
                                             bool isLocal, const Token& linkage,
                                             VizType vizType)
{
  hr_assert(fToken == kSymbol || fToken == kDot);

  Token firstToken = fToken;
  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    HR_LOG(kError, firstToken.srcpos(), E_MissingDefName) << "Missing name";
    return TokenVector();
  }

  Token symToken = qualifyIdToken(qSymbol);

  const Macro* macro =
      fScope->lookupMacro(symToken.srcpos(), symToken.idValue(), K(showAmbiguousSymDef));
  if (macro) {
    Token macroName = Token(symToken.srcpos(), baseName(symToken.idValue()));

    if (macro->type() == kMacro_Def) {
      if (linkage.isSet())
        HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
            << "Unsupported linkage for macro appliance ignored";

      return parseMakeMacroCall(macroName, macro, isLocal);
    }
    // the macro is silently ignored here
  }

  if (!isLocal)
    registerSymbolForExport(symToken.idValue(), vizType);

  if (fToken == kParanOpen)
    return parseFunctionDef(defToken, vizToken, Token(), symToken, linkage)
        .toTokenVector();

  return parseVarDef2(defToken, vizToken, Token(), symToken, isLocal, linkage);
}


Token FirstPass::parseGenericFunctionDef(const Token& defToken, const Token& vizToken,
                                         bool isLocal, VizType vizType)
{
  Token tagToken;
  if (isLocal) {
    HR_LOG(kError, fToken.srcpos(), E_LocalGenericFunc)
        << "inner generic functions are not supported.  'generic' ignored";
  }
  else
    tagToken = fToken;

  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "expected function name";
    return scanUntilTopExprAndResume();
  }
  Token symToken = qualifyIdToken(qSymbol);

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '('";
    return scanUntilTopExprAndResume();
  }

  registerSymbolForExport(symToken.idValue(), vizType);

  return parseFunctionDef(defToken, vizToken, tagToken, symToken, Token());
}


Token FirstPass::parseAliasDef(const Token& defToken, const Token& vizToken, bool isLocal,
                               VizType vizType)
{
  hr_assert(fToken == Compiler::aliasToken);

  Token tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "expected alias name";
    return scanUntilTopExprAndResume();
  }
  Token symToken = qualifyIdToken(qSymbol);

  Token generics;
  if (fToken == kGenericOpen) {
    generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(kGenericClose), kGenericOpen, kGenericClose, K(hasSeparator),
                  E_GenericTypeList, generics, "alias-params");
  }

  Token whereClause;
  if (fToken == kWhereId)
    whereClause = parseWhereClause();

  Token docString = parseOptDocString();

  if (fToken != kAssign) {
    HR_LOG(kError, fToken.srcpos(), E_AssignExpected) << "expected '='";
    return scanUntilTopExprAndResume();
  }
  Token assignToken = fToken;
  nextToken();

  SrcPos pos = fToken.srcpos();
  Token type = parseTypeSpec(!K(onlyNestedConstr), !K(needParans));
  if (!type.isSet()) {
    HR_LOG(kError, pos, E_MissingType) << "type expression expected";
    return scanUntilTopExprAndResume();
  }

  if (!isLocal)
    registerSymbolForExport(symToken.idValue(), vizType);

  Token result = Token() << defToken;
  if (vizToken.isSet())
    result << vizToken;
  result << tagToken << symToken;

  if (generics.isSet())
    result << generics;

  if (whereClause.isSet())
    result << whereClause;


  if (docString.isSet())
    result << docString;

  result << assignToken << type;
  return result;
}


Token FirstPass::parseOptDocString()
{
  Token docString;
  if (fToken == kDocString) {
    if (!Properties::shouldIgnoreDocStrings())
      docString = fToken;
    nextToken();
  }
  return docString;
}


Token FirstPass::parseTypeDef(const Token& defToken, const Token& vizToken, bool isRecord,
                              bool isLocal, VizType vizType)
{
  hr_assert((isRecord && fToken == Compiler::recordToken) ||
            (!isRecord && fToken == Compiler::typeToken));

  Token tagToken;
  if (isLocal) {
    HR_LOG(kError, fToken.srcpos(), E_LocalTypeDef)
        << "inner type/class definitions are not supported.";
    return scanUntilTopExprAndResume();
  }
  else
    tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "expected alias name";
    return scanUntilTopExprAndResume();
  }
  Token symToken = qualifyIdToken(qSymbol);

  Token generics;
  if (fToken == kGenericOpen) {
    generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(kGenericClose), kGenericOpen, kGenericClose, K(hasSeparator),
                  E_GenericTypeList, generics, "typedef-params");
  }

  Token colonToken;
  Token isaType;
  if (fToken == kColon) {
    colonToken = fToken;
    nextToken();
    SrcPos pos = fToken.srcpos();
    isaType = parseTypeSpec(K(onlyNestedConstr), !K(needParans));
    if (!isaType.isSet()) {
      HR_LOG(kError, pos, E_MissingType) << "type expression expected";
      isaType = makeAnySymbol(fToken.srcpos());
    }
  }
  else if (isPrescannedTypeToken(fToken)) {
    colonToken = fToken[0];
    isaType = fToken[1];
    nextToken();
  }

  Token whereClause;
  if (fToken == kWhereId)
    whereClause = parseWhereClause();

  Token docString = parseOptDocString();

  Token slotParams;
  if (fToken == kParanOpen) {
    SrcPos paranPos = fToken.srcpos();

    if (isRecord) {
      TokenVector params;
      if (!parseRecordSlots(&params))
        return scanUntilTopExprAndResume();

      slotParams = Token(paranPos, kParanOpen, kParanClose) << params;
    }
    else {
      HR_LOG(kError, paranPos, E_NoSlotsInTypeDef) << "def type does not accept slots";
      nextToken();
      scanUntilEndOfParameters();
    }
  }


  Token result = Token() << defToken;
  if (vizToken.isSet())
    result << vizToken;
  result << tagToken << symToken;

  if (generics.isSet())
    result << generics;

  if (colonToken.isSet() && isaType.isSet())
    result << colonToken << isaType;

  if (whereClause.isSet())
    result << whereClause;

  if (docString.isSet())
    result << docString;

  if (slotParams.isSet())
    result << slotParams;

  registerSymbolForExport(symToken.idValue(), vizType);

  return result;
}


struct EnumItemParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken != kSymbol) {
      HR_LOG(kError, pass->fToken.srcpos(), E_SymbolExpected)
          << "expected enum item name";
      pass->scanUntilBrace();
      return true;
    }

    Token itemName = pass->fToken;
    pass->nextToken();

    TokenVector resultValue;
    resultValue.push_back(itemName);

    Token docString = pass->parseOptDocString();
    if (docString.isSet())
      resultValue.push_back(docString);

    if (pass->fToken == kAssign) {
      Token assignToken = pass->fToken;
      pass->nextToken();

      Token value = pass->parseExpr(!K(acceptComma));
      if (value.isSet()) {
        resultValue.push_back(assignToken);
        resultValue.push_back(value);
      }
    }

    if (resultValue.size() == 1)
      result << resultValue[0];
    else
      result << (Token() << resultValue);

    return true;
  }
};


Token FirstPass::parseEnumDef(const Token& defToken, const Token& vizToken, bool isLocal,
                              VizType vizType)
{
  hr_assert(fToken == Compiler::enumToken);
  Token tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "expected enum name";
    return scanUntilTopExprAndResume();
  }
  Token enumToken = qualifyIdToken(qSymbol);

  Token colonToken;
  Token isaType;
  if (fToken == kColon) {
    colonToken = fToken;
    nextToken();
    SrcPos pos = fToken.srcpos();
    isaType = parseTypeSpec(K(onlyNestedConstr), !K(needParans));
    if (!isaType.isSet()) {
      HR_LOG(kError, pos, E_MissingType) << "type expression expected";
      isaType = makeAnySymbol(fToken.srcpos());
    }
  }
  else if (isPrescannedTypeToken(fToken)) {
    colonToken = fToken[0];
    isaType = fToken[1];
    nextToken();
  }

  Token docString = parseOptDocString();

  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen) << "expected '{'";
    return scanUntilTopExprAndResume();
  }

  Token items = Token(fToken.srcpos(), kParanOpen, kParanClose);
  parseSequence(EnumItemParser(), kParanOpen, kParanClose, K(hasSeparator),
                E_BadEnumItemList, items, "enum-items");

  if (!isLocal)
    registerSymbolForExport(enumToken.idValue(), vizType);

  Token enumDefToken = Token() << defToken;
  if (vizToken.isSet())
    enumDefToken << vizToken;
  enumDefToken << tagToken << enumToken;
  if (colonToken.isSet() && isaType.isSet())
    enumDefToken << colonToken << isaType;
  if (docString.isSet())
    enumDefToken << docString;
  enumDefToken << items;

  return enumDefToken;
}


MacroType FirstPass::dertermineMacroPatternType(const Token& macroName,
                                                const SrcPos& patternPos,
                                                const TokenVector& pattern)
{
  if (pattern.size() == 1) {
    if (pattern[0] == macroName)
      return kMacro_Any;

    HR_LOG(kError, pattern[1].srcpos(), E_PatternNameMismatch)
        << "macro name and pattern mismatch.  Expected: " << macroName
        << " found: " << pattern[0];
    return kMacro_Invalid;
  }
  else if (pattern.size() > 1) {
    if (pattern[0] == kDefId || pattern[0] == kLetId) {
      if (pattern[1] == macroName)
        return kMacro_Def;

      HR_LOG(kError, pattern[1].srcpos(), E_PatternNameMismatch)
          << "macro name and pattern mismatch.  Expected: " << macroName
          << " found: " << pattern[1];
      return kMacro_Invalid;
    }

    TokenVector::const_iterator it = pattern.begin();
    if (*it != macroName) {
      HR_LOG(kError, pattern[1].srcpos(), E_PatternNameMismatch)
          << "macro name and pattern mismatch.  Expected: " << macroName
          << " found: " << *it;
      return kMacro_Invalid;
    }
    it++;

    if (*it == kParanOpen) {
      int paranCount = 0;
      SrcPos paranOpenPos = it->srcpos();

      for (; it != pattern.end(); it++) {
        if (*it == kParanOpen)
          paranCount++;
        else if (*it == kParanClose) {
          paranCount--;
          if (paranCount == 0) {
            it++;
            if (it == pattern.end())
              return kMacro_Function;
          }
        }
      }

      HR_LOG(kError, paranOpenPos, E_BadMacroPattern)
          << "Unbalanced paranthesis in macro pattern";
      return kMacro_Invalid;
    }
    return kMacro_Any;
  }

  HR_LOG(kError, patternPos, E_BadMacroPattern) << "empty macro pattern";
  return kMacro_Invalid;
}


MacroType FirstPass::determineMacroType(const Token& macroName,
                                        const MacroPatternVector& patterns)
{
  MacroType lastType = kMacro_Any;

  for (MacroPatternVector::const_iterator it = patterns.begin(); it != patterns.end();
       it++) {
    MacroType pType = dertermineMacroPatternType(macroName, it->fSrcPos, it->fPattern);
    if (pType == kMacro_Invalid)
      return pType;

    if (lastType == kMacro_Any || lastType == pType) {
      lastType = pType;
    }
    else {
      HR_LOG(kError, it->fSrcPos, E_MacroInconsistency)
          << "Macro has inconsistent patterns";
      return kMacro_Invalid;
    }
  }

  return lastType;
}


bool FirstPass::parseMacroComponent(TokenVector* component, TokenType beginTokenType,
                                    TokenType endTokenType)
{
  SrcPos startPos = fToken.srcpos();

  int braceCount = 1;
  for (;;) {
    if (fToken == kEOF) {
      HR_LOG(kError, fToken.srcpos(), E_UnexpectedEOF) << "unfinished macro component";
      if (startPos != fToken.srcpos())
        HR_LOG(kError, startPos, E_MissingBraceClose) << "beginning '{' was here";
      return false;
    }

    if (fToken == beginTokenType) {
      braceCount++;
      component->push_back(fToken);
      nextToken();
    }
    else if (fToken == endTokenType) {
      braceCount--;
      if (braceCount > 0) {
        component->push_back(fToken);
        nextToken();
      }
      else {
        nextToken();
        return true;
      }
    }
    else {
      component->push_back(fToken);
      nextToken();
    }
  }

  hr_invalid("");
  return true;
}


static bool isMacroOpen(const Token& token)
{
  return token == kMacroOpen || token == kMacroOpen2;
}


static TokenType macroEndToken(TokenType beginTokenType)
{
  if (beginTokenType == kMacroOpen)
    return kMacroClose;
  else if (beginTokenType == kMacroOpen2)
    return kMacroClose2;
  return beginTokenType;
}


bool FirstPass::parseMacroPatterns(MacroPatternVector* patterns)
{
  while (fToken == kPipe) {
    nextToken();

    if (!isMacroOpen(fToken)) {
      HR_LOG(kError, fToken.srcpos(), E_MissingBraceOpen)
          << "expected '\302\253' or '?(', found: " << fToken;
      scanUntilTopExprAndResume();
      return false;
    }

    TokenType beginTokenType = fToken.tokenType();
    nextToken();

    SrcPos patternPos = fToken.srcpos();
    TokenVector pattern;
    TokenVector replacement;
    if (parseMacroComponent(&pattern, beginTokenType, macroEndToken(beginTokenType))) {
      if (fToken == kMapTo) {
        nextToken();

        if (!isMacroOpen(fToken)) {
          HR_LOG(kError, fToken.srcpos(), E_MissingBraceOpen)
              << "expected '\302\253' or '?(', found: " << fToken;
          scanUntilTopExprAndResume();
          return false;
        }

        TokenType beginReplaceType = fToken.tokenType();
        nextToken();

        SrcPos pos = fToken.srcpos();
        if (parseMacroComponent(&replacement, beginReplaceType,
                                macroEndToken(beginReplaceType))) {
          patterns->push_back(MacroPattern(patternPos, pattern, replacement));
        }
        else {
          HR_LOG(kError, pos, E_BadMacroReplcment) << "bad macro replacement";
          scanUntilTopExprAndResume();
          return false;
        }
      }
      else {
        HR_LOG(kError, fToken.srcpos(), E_MapToExpected) << "expected '->'";
        scanUntilTopExprAndResume();
        return false;
      }
    }
    else {
      HR_LOG(kError, fToken.srcpos(), E_BadMacroPattern) << "bad macro pattern";
      scanUntilTopExprAndResume();
      return false;
    }
  }

  return true;
}


Token FirstPass::parseMacroDef(const Token& defToken, VizType vizType)
{
  hr_assert(fToken == Compiler::macroToken);
  Token tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    HR_LOG(kError, fToken.srcpos(), E_MissingDefName) << "expected macro name";
    return scanUntilTopExprAndResume();
  }
  Token macroNameToken = qualifyIdToken(qSymbol);

  Token docString = parseOptDocString();

  if (fToken != kPipe) {
    HR_LOG(kError, fToken.srcpos(), E_MissingPipe) << "expected '|'";
    return scanUntilTopExprAndResume();
  }

  MacroPatternVector patterns;
  if (parseMacroPatterns(&patterns)) {
    if (fEvaluateExprs) {
      MacroType mType = determineMacroType(
          Token(macroNameToken.srcpos(), macroNameToken.baseName()), patterns);

      String fullMacroName = qualifyId(currentModuleName(), macroNameToken.idValue());

      if (fScope->checkForRedefinition(defToken.srcpos(), Scope::kNormal, fullMacroName))
        return Token();


      auto synTable = SyntaxTable::compile(String(""), patterns);
      fScope->registerMacro(defToken.srcpos(), fullMacroName,
                            std::make_shared<Macro>(synTable, mType));

      registerSymbolForExport(macroNameToken.idValue(), vizType);

      if (Properties::isTraceMacro())
        HR_LOG(kInfo) << synTable->toString();
    }
  }

  // always ignore macros
  return Token();
}


Token FirstPass::parseLinkageType()
{
  hr_assert(fToken == kExternId);

  Token externToken = fToken;
  nextToken();
  if (fToken != kParanOpen) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanOpen)
        << "Expected linkage specification for extern keyword";
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();

  nextToken();
  if (fToken != kString) {
    HR_LOG(kError, fToken.srcpos(), E_StringExpected)
        << "Expected linkage specification for extern keyword";
    return scanUntilTopExprAndResume();
  }
  Token linkage = fToken;

  nextToken();
  if (fToken != kParanClose) {
    HR_LOG(kError, fToken.srcpos(), E_MissingParanClose)
        << "Expected linkage specification for extern keyword";
    return scanUntilTopExprAndResume();
  }
  nextToken();

  return Token() << externToken << (Token(paranPos, kParanOpen, kParanClose) << linkage);
}


TokenVector FirstPass::parseDef(bool isLocal)
{
  Token defToken = fToken;
  nextToken();

  Token vizToken;
  VizType vizType = kUnset;
  if (fToken == Compiler::publicToken || fToken == Compiler::pubToken) {
    vizToken = fToken;
    vizType = kPublic;
    nextToken();
  }
  else if (fToken == Compiler::internToken) {
    vizToken = fToken;
    vizType = kIntern;
    nextToken();
  }
  else if (fToken == Compiler::privateToken) {
    vizToken = fToken;
    vizType = kPrivate;
    nextToken();
  }

  Token linkage;
  if (fToken == kExternId) {
    linkage = parseLinkageType();
  }

  if (fToken == Compiler::typeToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for type definition ignored";
    return parseTypeDef(defToken, vizToken, !K(isClass), isLocal, vizType)
        .toTokenVector();
  }
  else if (fToken == Compiler::recordToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for class definition ignored";
    return parseTypeDef(defToken, vizToken, K(isRecord), isLocal, vizType)
        .toTokenVector();
  }
  else if (fToken == Compiler::aliasToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for alias definition ignored";
    return parseAliasDef(defToken, vizToken, isLocal, vizType).toTokenVector();
  }
  else if (fToken == Compiler::enumToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for enum definition ignored";
    return parseEnumDef(defToken, vizToken, isLocal, vizType).toTokenVector();
  }
  else if (fToken == Compiler::constToken || fToken == Compiler::configToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for special variable definition ignored";
    return parseVarDef(defToken, vizToken, fToken, isLocal, vizType);
  }
  else if (fToken == Compiler::genericToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for generic method ignored";
    return parseGenericFunctionDef(defToken, vizToken, isLocal, vizType).toTokenVector();
  }
  else if (fToken == Compiler::charToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for char definition ignored";
    return parseCharDef(defToken, vizToken, vizType).toTokenVector();
  }
  else if (fToken == Compiler::macroToken) {
    if (linkage.isSet())
      HR_LOG(kError, linkage.srcpos(), E_UnexpLinkage)
          << "Unsupported linkage for macro definition ignored";
    return parseMacroDef(defToken, vizType).toTokenVector();
  }
  else if (fToken == kSymbol || fToken == kDot) {
    return parseFunctionOrVarDef(defToken, vizToken, isLocal, linkage, vizType);
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_DefInitUnexpToken) << "Bad init value: " << fToken;
    return scanUntilTopExprAndResume().toTokenVector();
  }

  return TokenVector();
}


TokenVector FirstPass::parseTop()
{
  if (fToken == kModuleId) {
    return parseModule().toTokenVector();
  }
  else if (fToken == kLibraryId) {
    if (!fInLibrary) {
      return parseLibrary().toTokenVector();
    }
    else {
      HR_LOG(kError, fToken.srcpos(), E_NestedLibrary)
          << "Nested library not supported. Skipped";
      nextToken();
      return scanUntilTopExprAndResume().toTokenVector();
    }
  }
  else if (fToken == kApplicationId) {
    if (!fInApplication) {
      return parseApplication().toTokenVector();
    }
    else {
      HR_LOG(kError, fToken.srcpos(), E_NestedApp)
          << "Nested application not supported. Skipped";
      nextToken();
      return scanUntilTopExprAndResume().toTokenVector();
    }
  }
  else if (fToken == kExportId) {
    return parseExport().toTokenVector();
  }
  else if (fToken == kIncludeId) {
    if (!fInLibrary) {
      HR_LOG(kWarn, fToken.srcpos(), E_IncludeOutsideOfLibrary)
          << "'include' found outside of library";
    }

    return parseInclude();
  }
  else if (fToken == kImportId) {
    return parseImport();
  }
  else if (fToken == kDefId) {
    return parseDef(!K(isLocal));
  }
  else if (fToken == kWhenId) {
    return parseWhen(K(isTopLevel)).toTokenVector();
  }
  else if (fToken == kWithId) {
    return parseWith().toTokenVector();
  }
  else if (fToken == kExternId) {
    return parseExtern();
  }
  else {
    HR_LOG(kError, fToken.srcpos(), E_UnexpectedToken)
        << "Unexpected top expression: " << fToken;
    return scanUntilTopExprAndResume().toTokenVector();
  }

  return TokenVector();
}


Token FirstPass::parse()
{
  Token seq;

  {
    // let the complete parse run in its own scope to force an explicit export
    // run
    ScopeHelper scopeHelper(fScope, K(doExport), !K(isInnerScope), !K(doPropIntern),
                            kScopeL_CompileUnit);

    nextToken();
    parseTops(seq);
  }

  return seq;
}


//------------------------------------------------------------------------------

bool FirstPass::replaceSangHashIds(TokenVector* result, const TokenVector& source)
{
  const size_t srcSize = source.size();

  if (srcSize > 0) {
    size_t idx = 0;
    Token token = source[idx];
    bool hasFreeToken = true;

    while (idx < srcSize) {
      if (token == kSymbol) {
        if (idx + 1 < srcSize) {
          if (source[idx + 1] == kSangHash) {
            if (idx + 2 < srcSize) {
              if (source[idx + 2] == kSymbol) {
                token =
                    Token(token.srcpos(), token.idValue() + source[idx + 2].idValue());
                hasFreeToken = true;
                idx += 2;
                continue;
              }
              else if (source[idx + 2] == kKeyarg) {
                token = Token(token.srcpos(), kKeyarg,
                              token.idValue() + source[idx + 2].idValue());
                hasFreeToken = true;
                idx += 2;
                continue;
              }
              else if (source[idx + 2] == kString) {
                token = Token::newSymbolOrKeyarg(
                    token.srcpos(), token.idValue() + source[idx + 2].stringValue());
                hasFreeToken = true;
                idx += 2;
                continue;
              }
              else {
                HR_LOG(kError, source[idx + 2].srcpos(), E_OrphanedSangHash)
                    << "## requires right hand symbol";
                result->push_back(token);

                idx += 3;
                if (idx < srcSize) {
                  token = source[idx];
                  hasFreeToken = true;
                }
                continue;
              }
            }
            else {
              HR_LOG(kError, source[idx + 1].srcpos(), E_OrphanedSangHash)
                  << "Orphaned ## without following ID";
              result->push_back(token);
              idx += 2;
              if (idx < srcSize) {
                token = source[idx];
                hasFreeToken = true;
              }
              continue;
            }
          }
        }
      }
      else if (token == kSangHash) {
        HR_LOG(kError, token.srcpos(), E_OrphanedSangHash) << "Unexpected ##";
        idx++;
        if (idx < srcSize) {
          token = source[idx];
          hasFreeToken = true;
        }
        continue;
      }

      result->push_back(token);
      hasFreeToken = false;
      idx++;
      if (idx < srcSize) {
        token = source[idx];
        hasFreeToken = true;
      }
    }

    if (hasFreeToken)
      result->push_back(token);
  }

  return true;
}


const TokenVector& FirstPass::findReplaceToken(const Token& token,
                                               const NamedReplacementMap& bindings,
                                               bool& found)
{
  String paramName = token.macroParamName();
  NamedReplacementMap::const_iterator it = bindings.find(paramName);
  if (it != bindings.end()) {
    found = true;
    return it->second;
  }

  found = false;
  static TokenVector sEmpty;
  return sEmpty;
}


bool FirstPass::replaceMatchBindings(TokenVector* result, const TokenVector& templ,
                                     const NamedReplacementMap& bindings)
{
  TokenVector replacement;
  for (TokenVector::const_iterator it = templ.begin(); it != templ.end(); it++) {
    Token token = *it;

    switch (token.type()) {
    case kPunct:
    case kLit: replacement.push_back(token); break;
    case kContinuation: hr_invalid(""); break;
    case kId:
      if (token == kMacroParam || token == kMacroParamAsStr ||
          token == kMacroParamAsKeyword) {
        bool found = false;
        const TokenVector& replTokens = findReplaceToken(token, bindings, found);

        if (found) {
          if (replTokens.size() == 1) {
            if (token == kMacroParamAsStr) {
              replacement.push_back(
                  Token(replTokens[0].srcpos(), kString, replTokens[0].toString()));
            }
            else if (token == kMacroParamAsKeyword) {
              replacement.push_back(
                  Token(replTokens[0].srcpos(), kKeyword, replTokens[0].toString()));
            }
            else
              replaceMatchBindings(&replacement, replTokens, bindings);
          }
          else if (replTokens.size() > 1)
            replaceMatchBindings(&replacement, replTokens, bindings);
        }
        else
          HR_LOG(kError, token.srcpos(), E_UnknownMacroParam)
              << "Undefined macro parameter " << token;
      }
      else
        replacement.push_back(token);
      break;

    case kSeq:
      if (token.count() > 0 &&
          !replaceMatchBindings(&replacement, token.children(), bindings))
        return false;
      break;

    case kNested:
      if (token.count() > 0) {
        TokenVector temp2;
        if (!replaceMatchBindings(&temp2, token.children(), bindings)) {
          return false;
        }

        replacement.push_back(Token(token.srcpos(), token.leftToken()));
        replaceMatchBindings(&replacement, temp2, bindings);
        replacement.push_back(Token(token.srcpos(), token.rightToken()));
        break;
      }
    }
  }

  return replaceSangHashIds(result, replacement);
}


//------------------------------------------------------------------------------

MatchResult MatchResult::kOk;

namespace {
  void bindParam(NamedReplacementMap* bindings, const String& paramName,
                 const TokenVector& values)
  {
    bindings->insert(std::make_pair(paramName, values));
  }

  void bindParam(NamedReplacementMap* bindings, const String& paramName,
                 const Token& value)
  {
    if (value.isSeq() && value.count() > 0)
      bindParam(bindings, paramName, (TokenVector)makeVector(value));
    else if (value.isSet() && !value.isSeq())
      bindParam(bindings, paramName, (TokenVector)makeVector(value));
    else
      bindParam(bindings, paramName, TokenVector());
  }
}  // namespace


struct ParameterSyntaxMatcher {
  virtual ~ParameterSyntaxMatcher() {}

  virtual MatchResult match(FirstPass* pass, const String& paramName,
                            NamedReplacementMap* bindings, SyntaxTreeNode& followSet)
  {
    hr_invalid("");
    return MatchResult(pass->fToken.srcpos(), E_MacroParamMismatch,
                       String("not implemented"));
  }
};


struct ExprParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    Token expr = pass->parseExpr(!K(acceptComma));
    if (!expr.isSet()) {
      return MatchResult(pos, E_MacroParamMismatch,
                         String("Macro parameter '") + paramName +
                             "' requires expression");
    }

    bindParam(bindings, paramName, expr);
    return MatchResult::kOk;
  }
};


struct TypeParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    Token type = pass->parseTypeSpec(K(onlyNestedConstr), K(needParans), !K(acceptEmpty));
    if (!type.isSet()) {
      return MatchResult(pos, E_MacroParamMismatch,
                         String("Macro parameter '") + paramName + "' requires type");
    }

    bindParam(bindings, paramName, type);
    return MatchResult::kOk;
  }
};


struct TypeSpecParamSyntaxMatcher : public ParameterSyntaxMatcher {
  TokenType fIntroToken;
  bool fAcceptEmpty = false;

  TypeSpecParamSyntaxMatcher(TokenType introToken, bool acceptEmpty)
      : fIntroToken(introToken)
      , fAcceptEmpty(acceptEmpty)
  {
  }

  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    Token result;
    Token type;

    if (pass->fToken == fIntroToken) {
      result << pass->fToken;
      pass->nextToken();

      type = pass->parseTypeSpec(K(onlyNestedConstr), K(needParans), fAcceptEmpty);
      if (!type.isSet()) {
        return MatchResult(pos, E_MacroParamMismatch,
                           String("Macro parameter '") + paramName +
                               "' requires type-spec");
      }

      result << type;
    }
    else if (!fAcceptEmpty) {
      return MatchResult(pos, E_MacroParamMismatch,
                         String("Macro parameter '") + paramName +
                             "' requires type-spec");
    }

    bindParam(bindings, paramName, result);
    bindParam(bindings, paramName + ".type", type);

    return MatchResult::kOk;
  }
};


struct TypeListParamSyntaxMatcher : public ParameterSyntaxMatcher {
  TokenType fIntroToken;
  bool fAcceptEmpty = false;

  TypeListParamSyntaxMatcher(TokenType introToken, bool acceptEmpty)
      : fIntroToken(introToken)
      , fAcceptEmpty(acceptEmpty)
  {
  }

  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    Token result;
    Token types;

    if (pass->fToken == fIntroToken) {
      result << pass->fToken;
      pass->nextToken();

      types = pass->parseTypeSpec(K(onlyNestedConstr), !K(needParans), fAcceptEmpty);
      if (!types.isSet()) {
        return MatchResult(pos, E_MacroParamMismatch,
                           String("Macro parameter '") + paramName + "' requires types");
      }

      if (types.isSeq())
        result << types.children();
      else
        result << types;
    }
    else if (!fAcceptEmpty) {
      return MatchResult(pos, E_MacroParamMismatch,
                         String("Macro parameter '") + paramName + "' requires types");
    }

    bindParam(bindings, paramName, result);
    bindParam(bindings, paramName + ".types", types);

    return MatchResult::kOk;
  }
};


struct DocStringParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    Token docstr = pass->parseOptDocString();
    if (docstr.isSet()) {
      bindParam(bindings, paramName, docstr);
      return MatchResult::kOk;
    }

    bindParam(bindings, paramName, Token());
    return MatchResult::kOk;
  }
};


struct NameParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    if (pass->fToken == kSymbol) {
      bindParam(bindings, paramName, pass->fToken);
      pass->nextToken();
      return MatchResult::kOk;
    }

    return MatchResult(pass->fToken.srcpos(), E_MacroParamMismatch,
                       String("Macro parameter '") + paramName + "' requires identifier");
  }
};


struct OperatorParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    OperatorType op = tokenTypeToOperator(pass->fToken.tokenType());
    if (op != kOpInvalid) {
      bindParam(bindings, paramName, pass->fToken);
      pass->nextToken();
      return MatchResult::kOk;
    }

    return MatchResult(pass->fToken.srcpos(), E_MacroParamMismatch,
                       String("Macro parameter '") + paramName + "' requires operator");
  }
};


struct AnyParamParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    FirstPass::ParamType expected = FirstPass::kPositional;
    Token param = pass->parseParameter(&expected, !K(autoCompleteTypes));

    if (!param.isSet()) {
      return MatchResult(pos, E_MacroParamMismatch,
                         String("Macro parameter '") + paramName +
                             "' requires parameter");
    }

    Token key;
    Token mapTo;
    Token name;
    Token type;
    Token typeSpec;
    Token init;

    if (param.isSeq()) {
      auto ofs = 0;
      if (param.count() > ofs + 1) {
        if (param[ofs] == kSymbol && param[ofs + 1] == kMapTo) {
          key = param[ofs];
          mapTo = param[ofs + 1];
          ofs += 2;
        }
      }

      if (param.count() > ofs) {
        name = param[ofs];
        ofs++;
      }

      if (param.count() > ofs + 1 && param[ofs] == kColon) {
        type = param[ofs + 1];
        typeSpec = Token() << param[ofs] << param[ofs + 1];
        ofs += 2;
      }

      if (param.count() > ofs + 1 && param[ofs] == kEqual) {
        init = param[ofs + 1];
        ofs += 2;
      }
    }

    bindParam(bindings, paramName, param);
    bindParam(bindings, paramName + ".key", key);
    bindParam(bindings, paramName + ".mapto", mapTo);
    bindParam(bindings, paramName + ".name", name);
    bindParam(bindings, paramName + ".type", type);
    bindParam(bindings, paramName + ".typespec", typeSpec);
    bindParam(bindings, paramName + ".init", init);

    return MatchResult::kOk;
  }
};


struct SpecParamParamSyntaxMatcher : public ParameterSyntaxMatcher {
  FirstPass::ParamType fReqType;

  SpecParamParamSyntaxMatcher(FirstPass::ParamType reqType)
      : fReqType(reqType)
  {
  }

  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    FirstPass::ParamType expected = FirstPass::kPositional;
    Token param = pass->parseParameter(&expected, !K(autoCompleteTypes));

    if (expected != fReqType || !param.isSet()) {
      return MatchResult(pos, E_MacroParamMismatch,
                         String("Macro parameter '") + paramName +
                             "' requires positional parameter");
    }

    Token name;
    Token type;
    Token typeSpec;

    if (param.isSeq()) {
      auto ofs = 0;
      if (param.count() > ofs) {
        name = param[ofs];
        ofs++;
      }

      if (param.count() > ofs + 1 && param[ofs] == kColon) {
        type = param[ofs + 1];
        typeSpec = Token() << param[ofs] << param[ofs + 1];
        ofs += 2;
      }
    }

    bindParam(bindings, paramName, param);
    bindParam(bindings, paramName + ".key", Token());
    bindParam(bindings, paramName + ".mapto", Token());
    bindParam(bindings, paramName + ".name", name);
    bindParam(bindings, paramName + ".type", type);
    bindParam(bindings, paramName + ".typespec", typeSpec);
    bindParam(bindings, paramName + ".init", Token());

    return MatchResult::kOk;
  }
};


struct ParamListParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    TokenVector params;

    // TODO: extract the set of possible end token types from followSet and
    // pass it to parseFunctionsParamsFull.
    auto pos = pass->fToken.srcpos();
    TokenType endTokenType = kParanClose;
    if (!pass->parseFunctionsParamsFull(&params, kParanOpen, endTokenType,
                                        K(autoCompleteType), K(acceptEmptyList),
                                        !K(skipFirst), !K(eatLast))) {
      return MatchResult(pos, E_MacroParamMismatch, String());
    }

    bindParam(bindings, paramName, params);
    return MatchResult::kOk;
  }
};


struct SlotParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    Token slot = pass->parseSlot();

    if (!slot.isSet()) {
      return MatchResult(pos, E_MacroParamMismatch,
                         String("Macro parameter '") + paramName +
                             "' requires slot definition");
    }

    Token name;
    Token type;
    Token typeSpec;
    Token init;

    if (slot.isSeq()) {
      auto ofs = 0;
      if (slot.count() > ofs) {
        name = slot[ofs];
        ofs++;
      }

      if (slot.count() > ofs + 1 && slot[ofs] == kColon) {
        type = slot[ofs + 1];
        typeSpec = Token() << slot[ofs] << slot[ofs + 1];
        ofs += 2;
      }

      if (slot.count() > ofs + 1 && slot[ofs] == kEqual) {
        init = slot[ofs + 1];
        ofs += 2;
      }
    }

    bindParam(bindings, paramName, slot);
    bindParam(bindings, paramName + ".name", name);
    bindParam(bindings, paramName + ".type", type);
    bindParam(bindings, paramName + ".typespec", typeSpec);
    bindParam(bindings, paramName + ".init", init);

    return MatchResult::kOk;
  }
};


struct SlotListParamSyntaxMatcher : public ParameterSyntaxMatcher {
  MatchResult match(FirstPass* pass, const String& paramName,
                    NamedReplacementMap* bindings, SyntaxTreeNode& followSet) override
  {
    TokenVector slots;

    // TODO: extract the set of possible end token types from followSet and
    // pass it to parseFunctionsParamsFull.
    auto pos = pass->fToken.srcpos();
    TokenType endTokenType = kParanClose;
    if (!pass->parseRecordSlotsFull(&slots, kParanOpen, endTokenType, !K(skipFirst),
                                    !K(eatLast), K(firstUnexpectedEnds))) {
      return MatchResult(pos, E_MacroParamMismatch, String());
    }

    bindParam(bindings, paramName, slots);
    return MatchResult::kOk;
  }
};


namespace {
  template <class T, class... Args>
  auto makeMatcher(Args&&... args) -> std::unique_ptr<ParameterSyntaxMatcher>
  {
    return std::unique_ptr<ParameterSyntaxMatcher>{new T(std::forward<Args>(args)...)};
  }
}  // namespace


MatchResult FirstPass::matchParameter(const Token& macroParam,
                                      NamedReplacementMap* bindings,
                                      SyntaxTreeNode& followSet)
{
  using ParamFuncMap = std::map<MacroParamType, std::unique_ptr<ParameterSyntaxMatcher>>;
  static ParamFuncMap paramsMap;
  if (paramsMap.empty()) {
    paramsMap.insert(std::make_pair(kMacro_expr, makeMatcher<ExprParamSyntaxMatcher>()));
    paramsMap.insert(std::make_pair(kMacro_name, makeMatcher<NameParamSyntaxMatcher>()));
    paramsMap.insert(std::make_pair(kMacro_type, makeMatcher<TypeParamSyntaxMatcher>()));
    paramsMap.insert(std::make_pair(
        kMacro_typespec, makeMatcher<TypeSpecParamSyntaxMatcher>(kColon, false)));
    paramsMap.insert(std::make_pair(
        kMacro_typespec_opt, makeMatcher<TypeSpecParamSyntaxMatcher>(kColon, true)));
    paramsMap.insert(std::make_pair(
        kMacro_typelist, makeMatcher<TypeListParamSyntaxMatcher>(kColon, false)));
    paramsMap.insert(std::make_pair(
        kMacro_typelist_opt, makeMatcher<TypeListParamSyntaxMatcher>(kColon, true)));
    paramsMap.insert(
        std::make_pair(kMacro_param, makeMatcher<AnyParamParamSyntaxMatcher>()));
    paramsMap.insert(
        std::make_pair(kMacro_paramlist, makeMatcher<ParamListParamSyntaxMatcher>()));
    paramsMap.insert(std::make_pair(
        kMacro_posParam, makeMatcher<SpecParamParamSyntaxMatcher>(kPositional)));
    paramsMap.insert(std::make_pair(kMacro_namedParam,
                                    makeMatcher<SpecParamParamSyntaxMatcher>(kNamed)));
    paramsMap.insert(std::make_pair(kMacro_restParam,
                                    makeMatcher<SpecParamParamSyntaxMatcher>(kRest)));
    paramsMap.insert(
        std::make_pair(kMacro_operator, makeMatcher<OperatorParamSyntaxMatcher>()));

    paramsMap.insert(std::make_pair(kMacro_slot, makeMatcher<SlotParamSyntaxMatcher>()));
    paramsMap.insert(
        std::make_pair(kMacro_slotlist, makeMatcher<SlotListParamSyntaxMatcher>()));

    paramsMap.insert(
        std::make_pair(kMacro_docstr, makeMatcher<DocStringParamSyntaxMatcher>()));
  }

  String paramName;
  MacroParamType macroPrmType = macroParamType(macroParam, &paramName);

  auto it = paramsMap.find(macroPrmType);
  if (it != paramsMap.end()) {
    return it->second->match(this, paramName, bindings, followSet);
  }
  else {
    return MatchResult(macroParam.srcpos(), E_MacroParamType,
                       String("Unknown macro parameter type: ") + macroParam);
  }
}


void FirstPass::matchSyntax(TokenVector* result, const String& macroInAction,
                            SyntaxTable& syntaxTable)
{
  auto node = syntaxTable.rootNode();
  hr_assert(node);

  NamedReplacementMap bindings;

  std::vector<MatchResult> potentialFailures;

  fInMacroEval.insert(macroInAction);

  for (;;) {
    if (fToken.isSymbol() && fInMacroEval.count(fToken.idValue()) == 0u) {
      bool done = false;

      fCompiler.pushCheckpoint();

      const auto symbolToken = fToken;
      nextToken();

      if (fToken == kParanOpen) {
        nextToken();

        if (const auto* macro = fScope->lookupMacro(
                symbolToken.srcpos(), symbolToken.idValue(), K(showAmbiguousSymDef))) {
          if (macro->type() == kMacro_Function) {
            Token macroName =
                Token(symbolToken.srcpos(), baseName(symbolToken.idValue()));
            TokenVector exprs = parseMakeMacroCall(macroName, macro, K(isLocal));

            if (exprs.size() == 1) {
              if (!exprs[0].isContinuation())
                unreadToken(exprs[0]);
            }
            else
              hr_invalid("");

            done = true;
          }
        }
      }

      if (!done) {
        fCompiler.unrollCheckpoint();
        fToken = symbolToken;
      }
      else {
        fCompiler.popCheckpoint();
      }
    }

    auto followSet = node->findNode(fToken);
    if (followSet) {
      node = followSet;

      if (node->hasSoleEndSet()) {
        replaceMatchBindings(result, node->replacement(), bindings);
        fInMacroEval.erase(macroInAction);
        return;
      }

      nextToken();
      continue;
    }

    else if (node->hasEndSet()) {
      replaceMatchBindings(result, node->replacement(), bindings);
      fInMacroEval.erase(macroInAction);
      return;
    }

    else {
      Token macroParam;
      followSet = node->findMacroParam(&macroParam);
      if (followSet) {
        auto mpresult = matchParameter(macroParam, &bindings, *followSet);
        if (mpresult) {
          node = followSet;
          continue;
        }
        else
          potentialFailures.push_back(mpresult);
      }
      else {
        potentialFailures.push_back(MatchResult(fToken.srcpos(), E_UnexpectedToken,
                                                String("Unexpected token: ") + fToken));
      }

      break;
    }
  }

  for (auto failure : potentialFailures) {
    HR_LOG(kError, failure.fPos, failure.fErrorCode) << failure.fError;
  }

  fInMacroEval.erase(macroInAction);
}


void FirstPass::parseDoMatchSyntaxDef(TokenVector* result, const Token& expr,
                                      SyntaxTable& syntaxTable, bool isLocal)
{
  unreadToken(fToken);
  unreadToken(expr);

  if (isLocal)
    fToken = Token(expr.srcpos(), kLetId);
  else
    fToken = Token(expr.srcpos(), kDefId);

  matchSyntax(result, expr.idValue(), syntaxTable);
}


void FirstPass::parseDoMatchSyntaxFunc(TokenVector* result, const Token& expr,
                                       SyntaxTable& syntaxTable)
{
  Token oldCurrentToken = fToken;

  unreadToken(oldCurrentToken);
  unreadToken(Token(oldCurrentToken.srcpos(), kParanOpen));

  fToken = expr;
  matchSyntax(result, expr.idValue(), syntaxTable);
}


TokenVector FirstPass::parseMakeMacroCall(const Token& expr, const Macro* macro,
                                          bool isLocal)
{
  hr_assert(expr == kSymbol);
  hr_assert(!isQualified(expr.idValue()));

  auto syntaxTable = macro->syntaxTable();

  TokenVector filtered;
  switch (macro->type()) {
  case kMacro_Invalid: return TokenVector();

  case kMacro_Any: return TokenVector();

  case kMacro_Def: parseDoMatchSyntaxDef(&filtered, expr, *syntaxTable, isLocal); break;

  case kMacro_Function: parseDoMatchSyntaxFunc(&filtered, expr, *syntaxTable); break;
  }

  if (filtered.size() > 0) {
    unreadTokens(filtered);
    return {Token(fToken.srcpos(), kContinuationExpr)};
  }

  return {Token(fToken.srcpos(), kContinuationExpr)};
}


void FirstPass::parseTops(Token& seq)
{
  while (fToken != kEOF) {
    TokenVector n = parseTop();
    if (isUsable(n))
      seq << n;
  }
}

}  // namespace herschel
