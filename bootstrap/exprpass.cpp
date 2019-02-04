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
  FirstPass fp{ fCompiler, fCurrentToken, fScope };
  return fp.parse();
}


//----------------------------------------------------------------------------

static Token qualifyIdToken(const TokenVector& qSymbol)
{
  return !qSymbol.empty() ? Token(qSymbol[0].srcpos(), qualifyId(qSymbol)) : Token();
}


static Token makeAnySymbol(const SrcPos& pos)
{
  return Token(pos, kSymbol, "Any");
}


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


Token FirstPass::scanUntilTopExprAndResume()
{
  while (fToken != kEOF && fToken != kDefId && fToken != kModuleId &&
         fToken != kLibraryId && fToken != kExportId && fToken != kImportId &&
         fToken != kIncludeId && fToken != kWhenId && fToken != kExternId)
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
                              Token& result, zstring ctx, bool skipFirst, bool eatLast)
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
      else if (fToken != endToken)
        error(fToken.srcpos(), errorCode,
              (StringBuffer() << ctx << ": expected '"
                              << Token(SrcPos(), endToken).toString() << "' or ','"
                              << "; found: " << fToken.toString())
                  .toString());
    }
  }

  if (fToken == endToken) {
    if (eatLast)
      nextToken();
  }
  else {
    error(fToken.srcpos(), errorCode,
          (StringBuffer() << ctx << ": expected '" << Token(SrcPos(), endToken).toString()
                          << "'")
              .toString());

    if (startToken != kInvalid && startPos != fToken.srcpos())
      error(startPos, errorCode,
            (StringBuffer() << ctx << ": beginning '"
                            << Token(SrcPos(), startToken).toString() << "' was here")
                .toString());
    scanUntilTopExprAndResume();
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
  SrcPos startPos = fToken.srcpos();

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
        error(fToken.srcpos(), E_InconsistentGroupType,
              (StringBuffer() << ctx << ": expected wrong group type operator: "
                              << fToken.toString())
                  .toString());
        nextToken();
        continue;
      }
    }
  }

  if (fToken == kParanClose) {
    nextToken();
  }
  else {
    error(fToken.srcpos(), errorCode,
          (StringBuffer() << ctx << ": expected '"
                          << Token(SrcPos(), kParanClose).toString() << "'")
              .toString());

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
          errorf(fToken.srcpos(), E_UnexpectedRootedSymbol,
                 "Rooted qualified name not allowed in this postion");

          result.push_back(fToken);
          nextToken();
        }
        else {
          errorf(fToken.srcpos(), E_UnexpectedToken, "unexpected dot");
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
        errorf(fToken.srcpos(), E_SymbolExpected, "qualified name ends in '.'");
      }
    }
  }

  return result;
}


//----------------------------------------------------------------------------

struct LibraryParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    TokenVector n = pass->parseTop();
    if (!n.empty())
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
      ScopeHelper scopeHelper(fScope, K(doExport), !K(isInnerScope), !K(doPropOuter),
                              kScopeL_Library);
      ModuleHelper moduleScope(this, libName.idValue());

      fInLibrary = true;

      if (fToken == kBraceOpen) {
        parseSequence(LibraryParser(), kBraceOpen, kBraceClose, !K(hasSeparator),
                      E_MissingBraceClose, defines, "library-body");
      }
      else {
        while (fToken != kEOF) {
          TokenVector n = parseTop();
          if (!n.empty())
            defines << n;
        }
      }

      fInLibrary = false;
    }

    libExpr << defines;
  }

  return libExpr;
}


struct ModuleParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    TokenVector n = pass->parseTop();
    if (!n.empty())
      result << n;

    return true;
  }
};


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
      ScopeHelper scopeHelper(fScope, K(doExport), K(isInnerScope), K(doPropOuter),
                              kScopeL_Module);

      ModuleHelper moduleScope(this, modName.idValue());
      if (fToken == kBraceOpen) {
        parseSequence(ModuleParser(), kBraceOpen, kBraceClose, !K(hasSeparator),
                      E_MissingBraceClose, defines, "module-body");
      }
      else {
        while (fToken != kEOF) {
          TokenVector n = parseTop();
          if (!n.empty())
            defines << n;
        }
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
          errorf(pass->fToken.srcpos(), E_SymbolExpected,
                 "expected symbol domain identifier");
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
      errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected SYMBOL or '*'");
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

  VizType vizType = kOuter;
  if (fToken == kSymbol) {
    if (fToken == Compiler::publicToken) {
      vizType = kPublic;
      expr << fToken;
    }
    else
      errorf(fToken.srcpos(), E_ExportVisibility, "unknown visibility type '%s'",
             (zstring)StrHelper(fToken.toString()));

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
      errorf(fToken.srcpos(), E_EmptyExportList, "empty export list");
      ignore = true;
    }
    else
      expr << symbols;
  }
  else {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  bool isFinal = false;
  if (fToken == kAs) {
    Token asToken = fToken;
    nextToken();

    if (fToken != Compiler::finalToken) {
      errorf(fToken.srcpos(), E_UnexpectedToken, "expected 'final'");
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
      errorf(pass->fToken.srcpos(), E_StringExpected, "expected STRING");
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
    if (fToken == Compiler::publicToken) {
      isPublic = true;
    }
    else
      errorf(fToken.srcpos(), E_UnknownIncludeScope, "unknown include scope '%s'",
             (zstring)StrHelper(fToken.toString()));

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
    error(fToken.srcpos(), E_StringExpected,
          String("expected STRING or '('") + fToken.toString());
    return scanUntilTopExprAndResume().toTokenVector();
  }


  TokenVector result;

  if (isPublic || !fCompiler.isParsingInterface()) {
    for (const auto& srcp : srcNames) {
      try {
        auto incl = fCompiler.includeFile(srcp.first, srcp.second, [&]() {
          nextToken();

          Token seq;
          while (fToken != kEOF) {
            TokenVector n = parseTop();
            if (!n.empty())
              seq << n;
          }

          return seq.children();
        });
        result.insert(result.end(), incl.begin(), incl.end());

        nextToken();
      }
      catch (const Exception& e) {
        error(srcp.first, E_UnknownInputFile, e.message());
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
      errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
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
    errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
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
      error(libNameSymb.srcpos(), E_UnknownLibrary, e.message());
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
      errorf(pos, E_UnexpectedToken, "returntype expression expected, but found: %s",
             (zstring)StrHelper(pass->fToken.toString()));
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
      errorf(baseToken.srcpos(), E_UnexpectedQuote, "Unexpected quote");
      return Token();
    }

    typeName = qualifyIdToken(qSymbol);
  }
  else if (baseToken == kQuote) {
    Token t = baseToken;
    nextToken();

    auto qSymbol = parseQualifiedName(true);
    if (qSymbol.empty()) {
      errorf(t.srcpos(), E_UnexpectedQuote, "Unexpected quote");
      return Token();
    }

    typeName = Token() << t << qualifyIdToken(qSymbol);
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
        errorf(idxPos, E_UnexpectedToken, "expected index expression");
      else
        arrayType << idxExpr;
    }

    if (fToken != kBracketClose) {
      errorf(fToken.srcpos(), E_MissingBracketClose, "expected ']'");
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
      error(constExpr.srcpos(), E_ConstExprExpected,
            String("constraint types only accept constant expressions") +
                constExpr.toString());
      return baseType;
    }

    return Token() << baseType << op << constExpr;
  }

  return baseType;
}


Token FirstPass::parseFunctionSignature()
{
  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
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
        errorf(pos, E_MissingType, "returntype expression expected");
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
    errorf(fToken.srcpos(), E_SymbolExpected, "missing type name");
    return Token();
  }

  Token result = Token() << quote << fToken;
  nextToken();

  return result;
}


Token FirstPass::parseTypeSpec(bool onlyNestedConstraints, bool needParans)
{
  Token isRefToken;
  if (fToken == kReference) {
    isRefToken = fToken;
    nextToken();
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
    else {
      errorf(fToken.srcpos(), E_UnexpectedToken,
             "Unexpect token, type expression expected");
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
          errorf(pass->fToken.srcpos(), E_InconsistentArgs,
                 "For literal dictionaries all elements must be '->' pairs");
          pass->scanUntilNextParameter();
          return true;
        }

        Token mapToken = pass->fToken;
        pass->nextToken();

        Token toValue = pass->parseExpr(!K(acceptComma));
        if (!toValue.isSet()) {
          errorf(mapToken.srcpos(), E_MissingRHExpr, "'->' requires a second expression");
          pass->scanUntilNextParameter();
        }
        else
          result << (Token() << expr << mapToken << toValue);
      }
      else {
        if (fIsDict)
          errorf(expr.srcpos(), E_InconsistentArgs,
                 "For literal dictionaries all elements must be '->' pairs");
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
      errorf(pass->fToken.srcpos(), E_UnexpectedToken,
             "Unexpected token while parsing array: %s",
             (zstring)StrHelper(pass->fToken.toString()));
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
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  Token test = parseExpr(!K(acceptComma));
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_ParamMissParanClose, "Syntax error, missing ')'");
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


Token FirstPass::parseParameter(ParamType* expected, bool autoCompleteTypes)
{
  Token paramSeq;
  ParamType paramType = kPositional;
  bool doScanOn = true;

  if (fToken.isSeq() && fToken.count() >= 1) {
    size_t ofs = 0;
    if (ofs + 2 < fToken.count() && fToken[ofs] == kSymbol && fToken[ofs + 1] == kMapTo) {
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
      paramSeq << fToken;
      nextToken();
    }

    if (fToken == kMapTo) {
      paramSeq << fToken;
      nextToken();
      paramType = kNamed;

      if (fToken == kSymbol) {
        paramSeq << fToken;
        nextToken();
      }
      else {
        error(fToken.srcpos(), E_SymbolExpected,
              String("parameter name expected: ") + fToken);
        scanUntilNextParameter();
        return Token();
      }
    }

    Token typeIntroToken = fToken;
    if (fToken == kColon || fToken == kAt) {
      nextToken();

      SrcPos pos = fToken.srcpos();
      Token type = parseTypeSpec(K(onlyNestedConstr), K(needParans));
      if (!type.isSet()) {
        errorf(pos, E_MissingType, "type expression expected");
        if (autoCompleteTypes)
          paramSeq << typeIntroToken << makeAnySymbol(pos);
      }
      else
        paramSeq << typeIntroToken << type;
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
        errorf(pos, E_MissingRHExpr, "no value in keyed argument");
      else {
        paramSeq << assignToken << initExpr;
        paramType = kNamed;
      }
    }
    else if (fToken == kEllipsis) {
      Token restToken = fToken;
      nextToken();

      if (paramType != kPositional) {
        errorf(restToken.srcpos(), E_InvalidRestParam, "orphaned rest parameter");
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
      errorf(paramSeq.srcpos(), E_ParamOrder, "out of order (positional) parameter");
    else {
      *expected = paramType;
      return paramSeq.unwrapSingleton();
    }
  }
  else if (*expected == kRest) {
    errorf(paramSeq.srcpos(), E_ParamOrder, "no parameter after rest parameter");
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
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
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
        errorf(pos, E_MissingType, "returntype expression expected");
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
    if (pass->fToken.isKeyArg()) {
      Token key = pass->fToken;
      pass->nextToken();

      Token val = pass->parseExpr(!K(acceptComma));
      if (!val.isSet()) {
        errorf(pass->fToken.srcpos(), E_UnexpectedToken,
               "Unexpected token while parsing function keyed argument's expr:",
               (zstring)StrHelper(pass->fToken.toString()));
        pass->scanUntilNextParameter();
        return true;
      }
      result << key;
      result << val;
    }
    else {
      Token val = pass->parseExpr(!K(acceptComma));
      if (!val.isSet()) {
        errorf(pass->fToken.srcpos(), E_UnexpectedToken,
               "unexpected token while parsing function arguments: ",
               (zstring)StrHelper(pass->fToken.toString()));
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


Token FirstPass::parseFunctionCall(const Token& expr, const TokenVector& preScannedArgs,
                                   bool parseParams)
{
  TokenVector effArgs;
  if (!preScannedArgs.empty())
    effArgs.assign(preScannedArgs.begin(), preScannedArgs.end());

  if (parseParams) {
    TokenVector args;
    parseFuncallArgs(&args);

    if (!args.empty()) {
      if (!effArgs.empty())
        effArgs.push_back(Token(args.front().srcpos(), kComma));

      effArgs.insert(effArgs.end(), args.begin(), args.end());
    }
  }

  return Token() << expr << (Token(expr.srcpos(), kParanOpen, kParanClose) << effArgs);
}


Token FirstPass::parseParamCall(const Token& expr, const TokenVector& preScannedArgs,
                                bool shouldParseParams)
{
  if (expr.isSymbol()) {
    const Macro* macro =
        fScope->lookupMacro(expr.srcpos(), expr.idValue(), K(showAmbiguousSymDef));
    Token macroName = Token(expr.srcpos(), baseName(expr.idValue()));

    if (macro) {
      TokenVector exprs = parseMakeMacroCall(macroName, preScannedArgs, macro,
                                             shouldParseParams, K(isLocal));
      return (exprs.size() == 1 ? exprs[0] : Token() << exprs);
    }
  }

  return parseFunctionCall(expr, preScannedArgs, shouldParseParams);
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
      errorf(fToken.srcpos(), E_MissingBracketClose, "expected ']'");
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
  if (fToken == kParanOpen) {
    nextToken();

    TokenVector args;
    return parseAccess(parseParamCall(expr, args, K(shouldParseParams)));
  }
  else if (fToken == kBracketOpen) {
    return parseAccess(parseSlice(expr));
  }
  else if (fToken == kDot) {
    Token dotToken = fToken;

    nextToken();

    if (fToken != kSymbol) {
      errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
      return scanUntilTopExprAndResume();
    }

    Token symToken = fToken;
    nextToken();

    if (fToken == kParanOpen) {
      auto call = parseAccess(symToken);
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
    errorf(fToken.srcpos(), E_MissingParanClose, "expected closing ')'");
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
        fToken == kLibraryId) {
      if (!endAtToplevelId) {
        error(fToken.srcpos(), E_UnexpectedTopExpr,
              String("unexpected top level expression: ") + fToken.toString());
        return false;
      }
      else
        return true;
    }
    else if (fToken == kLetId) {
      if (!isLocal) {
        errorf(fToken.srcpos(), E_GlobalLet, "'let' is not allowed here");
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
        error(startPos, E_UnexpectedToken,
              String("unexpected token while scanning block: ") + before);
        return false;
      }
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
    errorf(fToken.srcpos(), E_MissingBraceClose, "expected '}'");

    if (startPos != fToken.srcpos())
      error(startPos, E_MissingBraceClose, String("beginning '{' was here"));
  }
  else
    nextToken();

  return wrapInBlock(bosp, exprlist);
}


Token FirstPass::parseUnaryOp(const Token& inOpToken)
{
  Token opToken = inOpToken;
  nextToken();

  Token t = parseAtomicExpr();
  if (!t.isSet()) {
    errorf(opToken.srcpos(), E_UnexpectedToken, "expected expression");
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
      errorf(bodySrcpos, E_MissingExpr, "Missing expression for select pattern");
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
      errorf(pass->fToken.srcpos(), E_ExpectedPipe, "expect '|'");
      pass->scanUntilBrace();
      return false;
    }
    Token pipeToken = pass->fToken;
    pass->nextToken();

    if (pass->fToken == kElseId) {
      bool ignore = false;
      Token elseToken = pass->fToken;

      if (fElseSeen) {
        errorf(pass->fToken.srcpos(), E_RedefinedPattern, "'else' pattern redefined");
        ignore = true;
      }
      fElseSeen = true;
      pass->nextToken();

      if (pass->fToken == kMapTo) {
        warningf(pass->fToken.srcpos(), E_UnexpectedMapTo,
                 "Misplaced '->' after 'else' in select");
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
          errorf(pass->fToken.srcpos(), E_ElseNotLastPattern,
                 "'else' must be last pattern");
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
              error(pass->fToken.srcpos(), E_BadPatternList,
                    String("unexpected token: ") + pass->fToken.toString());
              return false;
            }
          }
          else {
            error(pass->fToken.srcpos(), E_UnexpectedToken,
                  String("unexpected token in select: ") + pass->fToken.toString());
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
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  TokenVector args;
  parseFuncallArgs(&args);

  if (fToken != kPipe) {
    errorf(fToken.srcpos(), E_MissingPipe, "expected '|'");
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
      errorf(pass->fToken.srcpos(), E_ExpectedPipe, "expect '|'");
      pass->scanUntilBrace();
      return false;
    }
    Token pipeToken = pass->fToken;
    pass->nextToken();

    if (pass->fToken != kSymbol && pass->fToken != kColon) {
      errorf(pass->fToken.srcpos(), E_SymbolExpected, "variable name or ':' expected");
      pass->scanUntilBrace();
      return false;
    }

    Token varToken;
    if (pass->fToken == kSymbol) {
      varToken = pass->fToken;
      pass->nextToken();
    }

    if (pass->fToken != kColon) {
      errorf(pass->fToken.srcpos(), E_ColonExpected,
             "match pattern require a type specification");
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
      errorf(pass->fToken.srcpos(), E_BadPatternList, "expected '->'");
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
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  TokenVector args;
  parseFuncallArgs(&args);

  if (fToken != kPipe) {
    errorf(fToken.srcpos(), E_MissingPipe, "expected '|'");
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
        errorf(pos, E_MissingType, "type expression expected");
        type = makeAnySymbol(pos);
      }
    }

    if (fToken == kIn) {
      Token inToken = fToken;
      nextToken();

      Token collToken = parseExpr(!K(acceptComma));
      if (!collToken.isSet()) {
        error(fToken.srcpos(), E_MissingRHExpr,
              String("unexpected token: ") + fToken.toString());
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
      error(fToken.srcpos(), E_UnexpectedToken,
            String("'in' keyword expected: ") + fToken.toString());
      scanUntilNextParameter();
    }
  }
  else {
    error(fToken.srcpos(), E_UnexpectedToken,
          String("Symbol expected in for clause: ") + fToken.toString());
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
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  Token test = parseForTestClause();
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_ParamMissParanClose, "Syntax error, missing ')'");
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
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  Token test = parseExpr(!K(acceptComma));
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_ParamMissParanClose, "Syntax error, missing ')'");
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
      errorf(typePos, E_MissingType, "expected type specifier");
      return number;
    }

    return Token() << number << colonToken << type;
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

  case kLetId: errorf(fToken.srcpos(), E_UnexpectedToken, "Unexpected let token"); break;

  case kSymbol:
  case kDot:
  case kQuote: return parseAccess(parseSymbolOrSimpleType(fToken));

  case kLiteralVectorOpen: return parseAccess(parseLiteralVector());

  case kLiteralArrayOpen: return parseAccess(parseLiteralArray());

  case kParanOpen: nextToken(); return parseAccess(parseGroup());
  case kBraceOpen: return parseAccess(parseBlock());

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
      errorf(exprs[0].srcpos(), E_OrphanedMultiValue,
             "Multiple variables without assign");
    if (hasRest)
      errorf(exprs[0].srcpos(), E_OrphanedRestInd,
             "Multiple variables (rest notation) without assign");
    return exprs[0];
  }

  nextToken();

  SrcPos before2ndPos = fToken.srcpos();
  Token expr2;

  if (op1 == kOpAs)
    expr2 = parseTypeSpec(K(onlyNestedConstr), K(needParans));
  else
    expr2 = parseAtomicExpr();
  OperatorType op2 = tokenTypeToOperator(fToken.tokenType());
  SrcPos op2Srcpos = fToken.srcpos();

  if (!expr2.isSet()) {
    errorf(before2ndPos, E_MissingRHExpr, "no right hand expression");
    return exprs[0];
  }

  if (op2 == kOpInvalid) {
    if (op1 == kOpAssign) {
      return makeAssignToken(exprs, expr2, op1Srcpos, hasRest);
    }
    else {
      if (exprs.size() > 1) {
        errorf(exprs[0].srcpos(), E_BadLHExpr,
               "Multiple left hand variables only allowed with assignments.");
        return Token();
      }
      return Token() << exprs[0] << Token(op1Srcpos, operatorToTokenType(op1)) << expr2;
    }
  }
  else {
    if (exprs.size() > 1) {
      errorf(exprs[0].srcpos(), E_BadLHExpr,
             "Multiple left hand variables only allowed with assignments.");
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
  Token expr1 = parseAtomicExpr();
  if (expr1.isSet()) {
    TokenVector exprs;
    bool hasRest = false;

    exprs.push_back(expr1);

    if (acceptComma) {
      while (fToken == kComma) {
        nextToken();

        Token expr2 = parseAtomicExpr();
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
    if (!topexprs.empty())
      result->insert(result->end(), topexprs.begin(), topexprs.end());
  }

  if (fToken == kBraceClose)
    nextToken();
}


TokenVector FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken == kBraceOpen) {
      SrcPos bracePos = fToken.srcpos();
      nextToken();

      TokenVector exprs;
      parseTopExprUntilBrace(&exprs);

      return exprs;
    }

    TokenVector exprs = parseTop();
    return exprs;
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
        errorf(fToken.srcpos(), E_UnexpectedEOF, "unfinished when component");
        if (startPos != fToken.srcpos())
          errorf(startPos, E_MissingBraceClose, "beginning '{' was here");
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
      error(fToken.srcpos(), E_UnexpectedToken,
            String("only 'ignore' or 'include' are valid symbols here: ") + fToken);
      errorf(fToken.srcpos(), E_UnexpectedToken, "assume 'ignore'");

      inclConsequent = false;
    }
    nextToken();
  }
  else if (fToken == kParanOpen) {
    SrcPos paranPos = fToken.srcpos();
    nextToken();

    Token test = parseExpr(!K(acceptComma));
    if (fToken != kParanClose) {
      errorf(fToken.srcpos(), E_ParamMissParanClose, "missing ')'");
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
        warningf(p.srcpos(), E_BadType,
                 "when-expression did not evaluate to boolean. "
                 "Treat it as false");
        inclConsequent = false;
        inclAlternate = true;
      }
    }
    else
      result << (Token(paranPos, kParanOpen, kParanClose) << test);
  }
  else if (fToken == kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen,
           "missing parameters or key for 'when' clause");
    errorf(fToken.srcpos(), E_MissingParanOpen, "assume 'ignore' here");
    // try to continue with this

    inclConsequent = false;
    inclAlternate = true;
  }
  else {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
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
      errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
      return scanUntilTopExprAndResume();
    }

    Token nsNameToken = qualifyIdToken(qSymbol);

    if (fToken != kBraceOpen) {
      errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
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
    error(fToken.srcpos(), E_UnexpectedToken,
          String("unknown scope in 'with': ") + fToken);
    return scanUntilTopExprAndResume();
  }
}


TokenVector FirstPass::parseExtern()
{
  hr_assert(fToken == kExternId);
  Token externToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '(' after extern");
    return scanUntilTopExprAndResume().toTokenVector();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();
  if (fToken != kString) {
    errorf(fToken.srcpos(), E_StringExpected, "expected external linkage name");
    return scanUntilTopExprAndResume().toTokenVector();
  }
  Token linkage = fToken;
  String linkageType = fToken.stringValue();

  nextToken();
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_MissingParanClose, "expected ')'");
    return scanUntilTopExprAndResume().toTokenVector();
  }
  nextToken();

  if (fToken != kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
    return scanUntilTopExprAndResume().toTokenVector();
  }
  SrcPos bracePos = fToken.srcpos();
  nextToken();

#if 0
  if (linkageType == String("C")) {
    ExternCParser externc{*this};
    return externc.parseBlock();
  }
#endif

  errorf(fToken.srcpos(), E_UnknownLinkage, "Unknown linkage type: '%s'",
         (zstring)StrHelper(linkageType));
  return scanUntilTopExprAndResume().toTokenVector();
}


TokenVector FirstPass::parseVarDef(const Token& defToken, const Token& tagToken,
                                   bool isLocal)
{
  Token keepTagToken = tagToken;

  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    errorf(fToken.srcpos(), E_MissingDefName, "Missing name");
    return scanUntilTopExprAndResume().toTokenVector();
  }
  Token symbolToken = qualifyIdToken(qSymbol);

  return parseVarDef2(defToken, keepTagToken, symbolToken, isLocal, Token());
}


Token FirstPass::evaluateConfigExpr(const Token& initExpr)
{
  TokenEvalContext ctx(*fCompiler.configVarRegistry());
  return ctx.evalToken(initExpr);
}


TokenVector FirstPass::parseVarDef2(const Token& defToken, const Token& tagToken,
                                    const Token& symbolToken, bool isLocal,
                                    const Token& linkage)
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
        errorf(pos, E_MissingType, "type expression expected");
        type = makeAnySymbol(pos);
      }
    }

    if (fToken == kEllipsis) {
      ellipsisToken = fToken;
      nextToken();
    }

    Token docString = parseOptDocString();

    Token vardefExpr;
    vardefExpr << defToken;
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
        errorf(fToken.srcpos(), E_InvalidRestParam,
               "Rest var declaration must be last in sequence");
        return scanUntilTopExprAndResume().toTokenVector();
      }

      nextToken();
      if (fToken != kSymbol) {
        errorf(fToken.srcpos(), E_MissingDefName, "Missing name");
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
    initExpr = parseExpr(!K(acceptComma));
    if (!initExpr.isSet())
      errorf(pos, E_MissingRHExpr, "no value in var init");
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
          error(vardefSym.srcpos(), E_DefNoInitValue,
                (String("Config variable '") + symbolToken + "' without default value"));

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


Token FirstPass::parseCharDef(const Token& defToken)
{
  Token tagToken = fToken;

  nextToken();

  if (!fToken.isCharOrUnitName()) {
    errorf(fToken.srcpos(), E_MissingDefName, "missing char name");
    return scanUntilTopExprAndResume();
  }
  Token charNameToken =
      (fToken == kSymbol ? fToken : Token(fToken.srcpos(), kSymbol, fToken.toString()));

  nextToken();

  Token docString = parseOptDocString();

  Token assignToken = fToken;
  if (fToken != kAssign) {
    errorf(fToken.srcpos(), E_DefNoInitValue, "expected '='");
    assignToken = Token(fToken.srcpos(), kAssign);
  }
  else
    nextToken();

  Token codePointToken = fToken;
  int codePoint = 0xffff;

  if (fToken != kInt && fToken != kUInt) {
    errorf(fToken.srcpos(), E_DefInitUnexpToken, "expected INTEGER");
    codePointToken = Token(fToken.srcpos(), kUInt, 0xffff);
  }
  else {
    codePoint = fToken.intValue();
    if (codePoint < 0 || codePoint > 0x10FFFF) {
      errorf(fToken.srcpos(), E_BadCharValue, "invalid expected INTEGER");

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
    Token result = Token() << defToken << tagToken << charNameToken;
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
      errorf(fToken.srcpos(), E_UnexpectedEOF,
             "unexpected eof while scanning 'where' clause");
      return Token();
    }

    if (fToken != kSymbol) {
      errorf(fToken.srcpos(), E_SymbolExpected, "missing type name");
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
      errorf(fToken.srcpos(), E_SymbolExpected, "unexpected operator in where clause");
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


Token FirstPass::parseFunctionDef(const Token& defToken, const Token& tagToken,
                                  const Token& symToken, const Token& linkage)
{
  hr_assert(fToken == kParanOpen);
  Token paranOpenToken = fToken;

  Token result;
  result << defToken;
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
        errorf(pos, E_MissingType, "type expression expected");
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
        errorf(bodyPos, E_MissingBody, "expected function body");
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


TokenVector FirstPass::parseFunctionOrVarDef(const Token& defToken, bool isLocal,
                                             const Token& linkage)
{
  hr_assert(fToken == kSymbol || fToken == kDot);

  Token firstToken = fToken;
  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    errorf(firstToken.srcpos(), E_MissingDefName, "Missing name");
    return TokenVector();
  }

  Token symToken = qualifyIdToken(qSymbol);

  const Macro* macro =
      fScope->lookupMacro(symToken.srcpos(), symToken.idValue(), K(showAmbiguousSymDef));
  if (macro) {
    Token macroName = Token(symToken.srcpos(), baseName(symToken.idValue()));

    if (macro->type() == kMacro_Def) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for macro appliance ignored");

      TokenVector dummyArgs;
      TokenVector exprs =
          parseMakeMacroCall(macroName, dummyArgs, macro, K(shouldParseParams), isLocal);
      return exprs;
    }
    // the macro is silently ignored here
  }

  if (fToken == kParanOpen)
    return parseFunctionDef(defToken, Token(), symToken, linkage).toTokenVector();

  return parseVarDef2(defToken, Token(), symToken, isLocal, linkage);
}


Token FirstPass::parseGenericFunctionDef(const Token& defToken, bool isLocal)
{
  Token tagToken;
  if (isLocal) {
    errorf(fToken.srcpos(), E_LocalGenericFunc,
           "inner generic functions are not supported.  'generic' ignored");
  }
  else
    tagToken = fToken;

  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected function name");
    return scanUntilTopExprAndResume();
  }
  Token symToken = qualifyIdToken(qSymbol);

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  return parseFunctionDef(defToken, tagToken, symToken, Token());
}


Token FirstPass::parseAliasDef(const Token& defToken, bool isLocal)
{
  hr_assert(fToken == Compiler::aliasToken);

  Token tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected alias name");
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
    errorf(fToken.srcpos(), E_AssignExpected, "expected '='");
    return scanUntilTopExprAndResume();
  }
  Token assignToken = fToken;
  nextToken();

  SrcPos pos = fToken.srcpos();
  Token type = parseTypeSpec(!K(onlyNestedConstr), !K(needParans));
  if (!type.isSet()) {
    errorf(pos, E_MissingType, "type expression expected");
    return scanUntilTopExprAndResume();
  }

  Token result = Token() << defToken << tagToken << symToken;
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


Token FirstPass::parseTypeDef(const Token& defToken, bool isRecord, bool isLocal)
{
  hr_assert((isRecord && fToken == Compiler::recordToken) ||
            (!isRecord && fToken == Compiler::typeToken));

  Token tagToken;
  if (isLocal) {
    errorf(fToken.srcpos(), E_LocalTypeDef,
           "inner type/class definitions are not supported.");
    return scanUntilTopExprAndResume();
  }
  else
    tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected alias name");
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
      errorf(pos, E_MissingType, "type expression expected");
      isaType = makeAnySymbol(fToken.srcpos());
    }
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
      if (!parseFunctionsParams(&params))
        return scanUntilTopExprAndResume();

      slotParams = Token(paranPos, kParanOpen, kParanClose) << params;
    }
    else {
      errorf(paranPos, E_NoSlotsInTypeDef, "def type does not accept slots");
      nextToken();
      scanUntilEndOfParameters();
    }
  }


  Token result = Token() << defToken << tagToken << symToken;
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

  return result;
}


struct EnumItemParser {
  bool operator()(FirstPass* pass, Token& result)
  {
    if (pass->fToken != kSymbol) {
      errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected enum item name");
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


Token FirstPass::parseEnumDef(const Token& defToken, bool isLocal)
{
  hr_assert(fToken == Compiler::enumToken);
  Token tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected enum name");
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
      errorf(pos, E_MissingType, "type expression expected");
      isaType = makeAnySymbol(fToken.srcpos());
    }
  }

  Token docString = parseOptDocString();

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '{'");
    return scanUntilTopExprAndResume();
  }

  Token items = Token(fToken.srcpos(), kParanOpen, kParanClose);
  parseSequence(EnumItemParser(), kParanOpen, kParanClose, K(hasSeparator),
                E_BadEnumItemList, items, "enum-items");

  Token enumDefToken = Token() << defToken << tagToken << enumToken;
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

    error(pattern[1].srcpos(), E_PatternNameMismatch,
          String("macro name and pattern mismatch.  Expected: ") + macroName +
              " found: " + pattern[0]);
    return kMacro_Invalid;
  }
  else if (pattern.size() > 1) {
    if (pattern[0] == kDefId || pattern[0] == kLetId) {
      if (pattern[1] == macroName)
        return kMacro_Def;

      error(pattern[1].srcpos(), E_PatternNameMismatch,
            String("macro name and pattern mismatch.  Expected: ") + macroName +
                " found: " + pattern[1]);
      return kMacro_Invalid;
    }

    TokenVector::const_iterator it = pattern.begin();
    if (*it != macroName) {
      error(pattern[1].srcpos(), E_PatternNameMismatch,
            String("macro name and pattern mismatch.  Expected: ") + macroName +
                " found: " + *it);
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
            else
              return kMacro_Stmt;
          }
        }
      }

      errorf(paranOpenPos, E_BadMacroPattern, "Unbalanced paranthesis in macro pattern");
      return kMacro_Invalid;
    }
    return kMacro_Any;
  }

  errorf(patternPos, E_BadMacroPattern, "empty macro pattern");
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
      errorf(it->fSrcPos, E_MacroInconsistency, "Macro has inconsistent patterns");
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
      errorf(fToken.srcpos(), E_UnexpectedEOF, "unfinished macro component");
      if (startPos != fToken.srcpos())
        errorf(startPos, E_MissingBraceClose, "beginning '{' was here");
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
      error(fToken.srcpos(), E_MissingBraceOpen,
            String("expected '\302\253' or '?(', found: ") + fToken);
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
          error(fToken.srcpos(), E_MissingBraceOpen,
                String("expected '\302\253' or '?(', found: ") + fToken);
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
          errorf(pos, E_BadMacroReplcment, "bad macro replacement");
          scanUntilTopExprAndResume();
          return false;
        }
      }
      else {
        errorf(fToken.srcpos(), E_MapToExpected, "expected '->'");
        scanUntilTopExprAndResume();
        return false;
      }
    }
    else {
      errorf(fToken.srcpos(), E_BadMacroPattern, "bad macro pattern");
      scanUntilTopExprAndResume();
      return false;
    }
  }

  return true;
}


Token FirstPass::parseMacroDef(const Token& defToken)
{
  hr_assert(fToken == Compiler::macroToken);
  Token tagToken = fToken;
  nextToken();

  auto qSymbol = parseQualifiedName(true);
  if (qSymbol.empty()) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected macro name");
    return scanUntilTopExprAndResume();
  }
  Token macroNameToken = qualifyIdToken(qSymbol);

  Token docString = parseOptDocString();

  if (fToken != kPipe) {
    errorf(fToken.srcpos(), E_MissingPipe, "expected '|'");
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

      if (Properties::isTraceMacro()) {
        fprintf(stderr, "%s\n", (zstring)StrHelper(synTable->toString()));
      }
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
    errorf(fToken.srcpos(), E_MissingParanOpen,
           "Expected linkage specification for extern keyword");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();

  nextToken();
  if (fToken != kString) {
    errorf(fToken.srcpos(), E_StringExpected,
           "Expected linkage specification for extern keyword");
    return scanUntilTopExprAndResume();
  }
  Token linkage = fToken;

  nextToken();
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_MissingParanClose,
           "Expected linkage specification for extern keyword");
    return scanUntilTopExprAndResume();
  }
  nextToken();

  return Token() << externToken << (Token(paranPos, kParanOpen, kParanClose) << linkage);
}


TokenVector FirstPass::parseDef(bool isLocal)
{
  Token defToken = fToken;
  nextToken();

  Token linkage;
  if (fToken == kExternId) {
    linkage = parseLinkageType();
  }

  if (fToken == Compiler::typeToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for type definition ignored");
    return parseTypeDef(defToken, !K(isClass), isLocal).toTokenVector();
  }
  else if (fToken == Compiler::recordToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for class definition ignored");
    return parseTypeDef(defToken, K(isRecord), isLocal).toTokenVector();
  }
  else if (fToken == Compiler::aliasToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for alias definition ignored");
    return parseAliasDef(defToken, isLocal).toTokenVector();
  }
  else if (fToken == Compiler::enumToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for enum definition ignored");
    return parseEnumDef(defToken, isLocal).toTokenVector();
  }
  else if (fToken == Compiler::constToken || fToken == Compiler::configToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for special variable definition ignored");
    return parseVarDef(defToken, fToken, isLocal);
  }
  else if (fToken == Compiler::genericToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for generic method ignored");
    return parseGenericFunctionDef(defToken, isLocal).toTokenVector();
  }
  else if (fToken == Compiler::charToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for char definition ignored");
    return parseCharDef(defToken).toTokenVector();
  }
  else if (fToken == Compiler::macroToken) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for macro definition ignored");
    return parseMacroDef(defToken).toTokenVector();
  }
  else if (fToken == kSymbol || fToken == kDot) {
    return parseFunctionOrVarDef(defToken, isLocal, linkage);
  }
  else {
    errorf(fToken.srcpos(), E_DefInitUnexpToken, "Bad init value: %s",
           (zstring)StrHelper(fToken.toString()));
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
      errorf(fToken.srcpos(), E_NestedLibrary, "Nested library not supported. Skipped");
      nextToken();
      return scanUntilTopExprAndResume().toTokenVector();
    }
  }
  else if (fToken == kExportId) {
    return parseExport().toTokenVector();
  }
  else if (fToken == kIncludeId) {
    if (!fInLibrary) {
      warningf(fToken.srcpos(), E_IncludeOutsideOfLibrary,
               "'include' found outside of library");
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
    errorf(fToken.srcpos(), E_UnexpectedToken, "I: Unexpected top expression: %s",
           (zstring)StrHelper(fToken.toString()));
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
    ScopeHelper scopeHelper(fScope, K(doExport), !K(isInnerScope), !K(doPropOuter),
                            kScopeL_CompileUnit);

    nextToken();
    while (fToken != kEOF) {
      TokenVector n = parseTop();
      if (!n.empty())
        seq << n;
    }
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
              else if (source[idx + 2] == kString) {
                token = Token(token.srcpos(),
                              token.idValue() + source[idx + 2].stringValue());
                hasFreeToken = true;
                idx += 2;
                continue;
              }
              else {
                errorf(source[idx + 2].srcpos(), E_OrphanedSangHash,
                       "## requires right hand symbol");
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
              errorf(source[idx + 1].srcpos(), E_OrphanedSangHash,
                     "Orphaned ## without following ID");
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
        errorf(token.srcpos(), E_OrphanedSangHash, "Unexpected ##");
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

    case kId:
      if (token == kMacroParam || token == kMacroParamAsStr) {
        bool found = false;
        const TokenVector& replTokens = findReplaceToken(token, bindings, found);

        if (found) {
          if (replTokens.size() == 1) {
            if (token == kMacroParamAsStr) {
              replacement.push_back(
                  Token(replTokens[0].srcpos(), kString, replTokens[0].toString()));
            }
            else
              replacement.push_back(replTokens[0]);
          }
          else if (replTokens.size() > 1) {
            replacement.insert(replacement.end(), replTokens.begin(), replTokens.end());
          }
        }
        else
          errorf(token.srcpos(), E_UnknownMacroParam, "Undefined macro parameter %s",
                 (zstring)StrHelper(token.toString()));
      }
      else
        replacement.push_back(token);
      break;

    case kSeq: {
      TokenVector temp2;
      if (!replaceMatchBindings(&temp2, token.children(), bindings))
        return false;

      replacement.push_back(Token() << temp2);
    } break;

    case kNested: {
      TokenVector temp2;
      if (!replaceMatchBindings(&temp2, token.children(), bindings)) {
        return false;
      }

      replacement.push_back(Token(token.srcpos(), token.leftToken(), token.rightToken())
                            << temp2);
    } break;
    }
  }

  return replaceSangHashIds(result, replacement);
}


//------------------------------------------------------------------------------

struct ParameterSyntaxMatcher {
  virtual ~ParameterSyntaxMatcher() {}

  virtual bool match(FirstPass* pass, const String& paramName,
                     NamedReplacementMap* bindings, SyntaxTreeNode& followSet)
  {
    hr_invalid("");
    return false;
  }
};


struct ExprParamSyntaxMatcher : public ParameterSyntaxMatcher {
  bool match(FirstPass* pass, const String& paramName, NamedReplacementMap* bindings,
             SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    Token expr = pass->parseExpr(!K(acceptComma));
    if (!expr.isSet()) {
      errorf(pos, E_MacroParamMismatch, "Macro parameter %s requires expression",
             (zstring)StrHelper(paramName));
      return false;
    }

    bindings->insert(std::make_pair(paramName, (TokenVector)makeVector(expr)));
    return true;
  }
};


struct NameParamSyntaxMatcher : public ParameterSyntaxMatcher {
  bool match(FirstPass* pass, const String& paramName, NamedReplacementMap* bindings,
             SyntaxTreeNode& followSet) override
  {
    if (pass->fToken == kSymbol) {
      bindings->insert(std::make_pair(paramName, (TokenVector)makeVector(pass->fToken)));
      pass->nextToken();
      return true;
    }

    errorf(pass->fToken.srcpos(), E_MacroParamMismatch,
           "Macro parameter %s requires identifier", (zstring)StrHelper(paramName));
    return false;
  }
};


struct OperatorParamSyntaxMatcher : public ParameterSyntaxMatcher {
  bool match(FirstPass* pass, const String& paramName, NamedReplacementMap* bindings,
             SyntaxTreeNode& followSet) override
  {
    OperatorType op = tokenTypeToOperator(pass->fToken.tokenType());
    if (op != kOpInvalid) {
      bindings->insert(std::make_pair(paramName, (TokenVector)makeVector(pass->fToken)));
      pass->nextToken();
      return true;
    }

    errorf(pass->fToken.srcpos(), E_MacroParamMismatch,
           "Macro parameter %s requires operator", (zstring)StrHelper(paramName));
    return false;
  }
};


struct AnyParamParamSyntaxMatcher : public ParameterSyntaxMatcher {
  bool match(FirstPass* pass, const String& paramName, NamedReplacementMap* bindings,
             SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    FirstPass::ParamType expected = FirstPass::kPositional;
    Token param = pass->parseParameter(&expected, !K(autoCompleteTypes));

    if (!param.isSet()) {
      errorf(pos, E_MacroParamMismatch, "Macro parameter %s requires parameter",
             (zstring)StrHelper(paramName));
      return false;
    }

    bindings->insert(std::make_pair(paramName, (TokenVector)makeVector(param)));
    return true;
  }
};


struct SpecParamParamSyntaxMatcher : public ParameterSyntaxMatcher {
  FirstPass::ParamType fReqType;

  SpecParamParamSyntaxMatcher(FirstPass::ParamType reqType)
      : fReqType(reqType)
  {
  }

  bool match(FirstPass* pass, const String& paramName, NamedReplacementMap* bindings,
             SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    FirstPass::ParamType expected = FirstPass::kPositional;
    Token param = pass->parseParameter(&expected, !K(autoCompleteTypes));

    if (expected != fReqType || !param.isSet()) {
      errorf(pos, E_MacroParamMismatch,
             "Macro parameter %s requires positional parameter",
             (zstring)StrHelper(paramName));
      return false;
    }

    bindings->insert(std::make_pair(paramName, (TokenVector)makeVector(param)));
    return true;
  }
};


struct ParamListParamSyntax : public ParameterSyntaxMatcher {
  bool match(FirstPass* pass, const String& paramName, NamedReplacementMap* bindings,
             SyntaxTreeNode& followSet) override
  {
    SrcPos pos = pass->fToken.srcpos();
    TokenVector params;

    // TODO: extract the set of possible end token types from followSet and
    // pass it to parseFunctionsParamsFull.
    TokenType endTokenType = kParanClose;
    if (!pass->parseFunctionsParamsFull(&params, kParanOpen, endTokenType,
                                        K(autoCompleteType), K(acceptEmptyList),
                                        !K(skipFirst), !K(eatLast))) {
      return false;
    }

    bindings->insert(std::make_pair(paramName, params));
    return true;
  }
};


namespace {
  template <class T, class... Args>
  auto makeMatcher(Args&&... args) -> std::unique_ptr<ParameterSyntaxMatcher>
  {
    return std::unique_ptr<ParameterSyntaxMatcher>{ new T(std::forward<Args>(args)...) };
  }
}  // namespace

bool FirstPass::matchParameter(const Token& macroParam, NamedReplacementMap* bindings,
                               SyntaxTreeNode& followSet)
{
  using ParamFuncMap = std::map<MacroParamType, std::unique_ptr<ParameterSyntaxMatcher>>;
  static ParamFuncMap paramsMap;
  if (paramsMap.empty()) {
    paramsMap.insert(std::make_pair(kMacro_expr, makeMatcher<ExprParamSyntaxMatcher>()));
    paramsMap.insert(std::make_pair(kMacro_name, makeMatcher<NameParamSyntaxMatcher>()));
    paramsMap.insert(
        std::make_pair(kMacro_param, makeMatcher<AnyParamParamSyntaxMatcher>()));
    paramsMap.insert(
        std::make_pair(kMacro_paramlist, makeMatcher<ParamListParamSyntax>()));
    paramsMap.insert(std::make_pair(
        kMacro_posParam, makeMatcher<SpecParamParamSyntaxMatcher>(kPositional)));
    paramsMap.insert(std::make_pair(kMacro_namedParam,
                                    makeMatcher<SpecParamParamSyntaxMatcher>(kNamed)));
    paramsMap.insert(std::make_pair(kMacro_restParam,
                                    makeMatcher<SpecParamParamSyntaxMatcher>(kRest)));
    paramsMap.insert(
        std::make_pair(kMacro_operator, makeMatcher<OperatorParamSyntaxMatcher>()));
  }

  String paramName;
  MacroParamType macroPrmType = macroParamType(macroParam, &paramName);

  auto it = paramsMap.find(macroPrmType);
  if (it != paramsMap.end()) {
    return it->second->match(this, paramName, bindings, followSet);
  }
  else {
    errorf(macroParam.srcpos(), E_MacroParamType, "Unknown macro parameter type: %s",
           (zstring)StrHelper(macroParam.toString()));
    return false;
  }
}


bool FirstPass::matchSyntax(TokenVector* result, SyntaxTable& syntaxTable)
{

  auto node = syntaxTable.rootNode();
  hr_assert(node);

  NamedReplacementMap bindings;

  for (;;) {
    auto followSet = node->findNode(fToken);
    if (followSet) {
      node = followSet;
      nextToken();
      continue;
    }

    else if (node->hasEndSet()) {
      return replaceMatchBindings(result, node->replacement(), bindings);
    }

    else {
      Token macroParam;
      followSet = node->findMacroParam(&macroParam);
      if (followSet) {
        if (matchParameter(macroParam, &bindings, *followSet)) {
          node = followSet;
          continue;
        }
      }

      return false;
    }
  }

  return false;
}


bool FirstPass::parseDoMatchSyntaxDef(TokenVector* result, const Token& expr,
                                      SyntaxTable& syntaxTable, bool isLocal)
{
  unreadToken(expr);

  if (isLocal)
    fToken = Token(expr.srcpos(), kLetId);
  else
    fToken = Token(expr.srcpos(), kDefId);

  return matchSyntax(result, syntaxTable);
}


bool FirstPass::parseDoMatchSyntaxFunc(TokenVector* result, const Token& expr,
                                       const TokenVector& args, SyntaxTable& syntaxTable,
                                       bool shouldParseParams)
{
  Token oldCurrentToken = fToken;

  if (shouldParseParams) {
    unreadToken(oldCurrentToken);

    if (!args.empty()) {
      Token nextTk = nextToken();
      unreadToken(nextTk);
      if (nextTk != kParanClose)
        unreadToken(Token(nextTk.srcpos(), kComma));
      hr_assert(args.size() == 1);
      unreadToken(args[0]);
    }

    unreadToken(Token(oldCurrentToken.srcpos(), kParanOpen));
  }
  else {
    unreadToken(oldCurrentToken);
    unreadToken(Token(oldCurrentToken.srcpos(), kParanClose));

    std::list<Token> res;
    for (size_t i = 0; i < args.size(); i++) {
      Token arg = args[i];
      if (i + 1 < args.size()) {
        res.push_front(arg);
        res.push_front(Token(arg.srcpos(), kComma));
      }
      else
        res.push_front(arg);
    }
    for (std::list<Token>::iterator it = res.begin(); it != res.end(); it++) {
      unreadToken(*it);
    }

    unreadToken(Token(oldCurrentToken.srcpos(), kParanOpen));
  }

  fToken = expr;
  return matchSyntax(result, syntaxTable);
}


bool FirstPass::parseExprStream(TokenVector* result, bool isTopLevel)
{
  for (;;) {
    if (fToken == kEOF)
      return true;
    else if (fToken == kBraceClose)
      return true;
    else if (fToken == kParanClose)
      return true;
    else if (fToken == kBracketClose)
      return true;

    SrcPos pos = fToken.srcpos();
    TokenVector exprs;
    if (isTopLevel)
      exprs = parseTop();
    else
      exprs = parseExpr(K(acceptComma)).toTokenVector();

    if (!exprs.empty())
      result->insert(result->end(), exprs.begin(), exprs.end());
    else {
      errorf(pos, E_UnexpectedToken,
             "unexpected token while scanning macro replacement: %s",
             (zstring)StrHelper(fToken.toString()));
      return false;
    }
  }

  return true;
}


TokenVector FirstPass::parseMakeMacroCall(const Token& expr, const TokenVector& args,
                                          const Macro* macro, bool shouldParseParams,
                                          bool isLocal)
{
  hr_assert(expr == kSymbol);
  hr_assert(!isQualified(expr.idValue()));

  auto syntaxTable = macro->syntaxTable();

  TokenVector filtered;
  switch (macro->type()) {
  case kMacro_Invalid:
    // hr_invalid("");
    return TokenVector();

  case kMacro_Any: return TokenVector();

  case kMacro_Def: parseDoMatchSyntaxDef(&filtered, expr, *syntaxTable, isLocal); break;

  case kMacro_Stmt:
  case kMacro_Function:
    parseDoMatchSyntaxFunc(&filtered, expr, args, *syntaxTable, shouldParseParams);
    break;
  }

  if (filtered.size() > 0) {
    Token lastCurrentToken = fToken;

    std::list<Token> follows;

    // skip the first item here.
    TokenVector::iterator it = filtered.begin();
    it++;
    follows.assign(it, filtered.end());

    // ... and store it in the current fToken
    fToken = filtered[0];

    TokenVector retval;
    auto tempPort = std::make_shared<InternalTokenPort>(follows);

    {
      Compiler::PortStackHelper portStack(fCompiler, tempPort);

      TokenVector result;
      if (parseExprStream(&result, !isLocal))
        retval = result;
      else
        return TokenVector();
    }

    fToken = lastCurrentToken;

    return retval;
  }

  return filtered;
}

}  // namespace herschel
