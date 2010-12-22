/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "errcodes.h"
#include "externc.h"
#include "log.h"
#include "macro.h"
#include "compiler.h"
#include "pass1.h"
#include "properties.h"
#include "scope.h"
#include "strbuf.h"
#include "symbol.h"
#include "tokeneval.h"
#include "tokenizer.h"
#include "valuesaver.h"


//----------------------------------------------------------------------------

using namespace heather;


//----------------------------------------------------------------------------

ExprPass::ExprPass(int level, Compiler* compiler,
                   const Token& currentToken, Scope* scope)
  : TokenCompilePass(level),
    fCurrentToken(currentToken),
    fScope(scope),
    fCompiler(compiler)
{ }


Token
ExprPass::doApply(const Token& src)
{
  Ptr<FirstPass> fp = new FirstPass(fCompiler, fCurrentToken, fScope);
  return fp->parse();
}


//----------------------------------------------------------------------------

FirstPass::FirstPass(Compiler* compiler, const Token& currentToken, Scope* scope)
  : AbstractPass(compiler, scope),
    fToken(currentToken),
    fEvaluateExprs(true)
{ }


//----------------------------------------------------------------------------

Token
FirstPass::nextToken()
{
  fToken = fCompiler->nextToken();
  return fToken;
}


Token
FirstPass::currentToken()
{
  return fToken;
}


void
FirstPass::unreadToken(const Token& token)
{
  fCompiler->unreadToken(token);
}


Token
FirstPass::scanUntilTopExprAndResume()
{
  while (fToken != kEOF &&
         fToken != kDefId &&
         fToken != kModuleId &&
         fToken != kExportId &&
         fToken != kImportId &&
         fToken != kWhenId &&
         fToken != kExtendId &&
         fToken != kExternId)
    nextToken();

  return Token();
}


Token
FirstPass::scanUntilNextParameter(TokenType endTokenType)
{
  while (fToken != kEOF &&
         fToken != kComma &&
         fToken != endTokenType)
    nextToken();

  return Token();
}


Token
FirstPass::scanUntilBrace()
{
  while (fToken != kEOF &&
         fToken != kBraceClose)
    nextToken();
  return Token();
}


Token
FirstPass::scanUntilEndOfParameters()
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


template<typename ParseFunctor>
void
FirstPass::parseSequence(ParseFunctor functor,
                         TokenType startToken, TokenType endToken,
                         bool hasSeparator,
                         ErrCodes errorCode,
                         Token& result,
                         const char* ctx,
                         bool skipFirst,
                         bool eatLast)
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
               << "; found: " << fToken.toString()).toString());
    }
  }

  if (fToken == endToken) {
    if (eatLast)
      nextToken();
  }
  else {
    error(fToken.srcpos(), errorCode,
          (StringBuffer() << ctx << ": expected '"
           << Token(SrcPos(), endToken).toString() << "'").toString());

    if (startToken != kInvalid && startPos != fToken.srcpos())
      error(startPos, errorCode,
            (StringBuffer() << ctx << ": beginning '"
             << Token(SrcPos(), startToken).toString()
             << "' was here").toString());
    scanUntilTopExprAndResume();
  }
}


//----------------------------------------------------------------------------

namespace heather
{
  struct ModuleParser
  {
    bool operator() (FirstPass* pass, Token& result)
    {
      TokenVector n = pass->parseTop(FirstPass::kNonScopedDef);
      if (!n.empty())
        result << n;

      return true;
    }
  };
};


Token
FirstPass::parseModule()
{
  Token tagToken = fToken;
  nextToken();

  Token modExpr;

  if (fToken == kSymbol) {
    Token modName = fToken;

    nextToken();
    if (fToken == kParanOpen) {
      SrcPos begSp = fToken.srcpos();
      nextToken();

      if (fToken == kString) {
        Token publicId = fToken;

        nextToken();
        if (fToken != kParanClose) {
          errorf(fToken.srcpos(), E_ParamMissParanClose,
                 "Syntax error, missing ')'");
        }
        else
          nextToken();

        modExpr = Token()
          << tagToken
          << modName
          << ( Token(begSp, kParanOpen, kParanClose)
               << publicId );
      }
      else {
        errorf(fToken.srcpos(), E_MissingModName,
               "Syntax error, missing module name");
        return scanUntilTopExprAndResume();
      }
    }
    else
      modExpr = Token() << tagToken
                        << modName;

    Token docString = parseOptDocString();
    if (docString.isSet())
      modExpr << docString;

    if (fToken == kBraceOpen) {
      Token defines = Token(fToken.srcpos(), kBraceOpen, kBraceClose);

      {
        ScopeHelper scopeHelper(fScope, true, true, kScopeL_Module);

        ModuleHelper moduleScope(this, modName.idValue());
        parseSequence(ModuleParser(),
                      kBraceOpen, kBraceClose, false, E_MissingBraceClose,
                      defines,
                      "module-body");
      }

      modExpr << defines;
    }
    else {
      fScope = new Scope(kScopeL_Module, fScope);
      fCurrentModuleName = qualifyId(fCurrentModuleName, modName.idValue());
    }
  }

  return modExpr;
}


namespace heather
{
  struct ExportParser
  {
    bool operator() (FirstPass* pass, Token& result)
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
          else
          {
            result << ( Token() << symbol << colon << pass->fToken );
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
};


Token
FirstPass::parseExport()
{
  Token expr;
  expr << fToken;

  nextToken();

  VizType vizType = kPrivate;
  if (fToken == kSymbol) {
    if (fToken == Compiler::publicToken) {
      vizType = kPublic;
      expr << fToken;
    }
    else if (fToken == Compiler::innerToken) {
      vizType = kInner;
      expr << fToken;
    }
    else if (fToken == Compiler::outerToken) {
      vizType = kOuter;
      expr << fToken;
    }
    else
      errorf(fToken.srcpos(), E_ExportVisibility,
             "unknown visibility type '%s'", (const char*)StrHelper(fToken.toString()));

    nextToken();
  }

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  bool ignore = false;
  Token symbols = Token(fToken.srcpos(), kParanOpen, kParanClose);
  parseSequence(ExportParser(),
                kParanOpen, kParanClose, true, E_BadParameterList,
                symbols,
                "export-symbols");
  if (symbols.isEmpty()) {
    errorf(fToken.srcpos(), E_EmptyExportList,
           "empty export list");
    ignore = true;
  }
  else
    expr << symbols;

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
    for (TokenVector::const_iterator it = children.begin();
         it != children.end();
         it++)
    {
      if (*it == kSymbol) {
        String fullId = ( isQualified(it->idValue())
                          ? it->idValue()
                          : qualifyId(currentModuleName(), it->idValue()) );
        fScope->registerSymbolForExport(Scope::kNormal, fullId, vizType, isFinal);
      }
    }

    return expr;
  }

  return Token();
}


namespace heather
{
  struct ImportRenameParser
  {
    bool operator() (FirstPass* pass, Token& result)
    {
      if (pass->fToken != kSymbol) {
        errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
        pass->scanUntilNextParameter();
      }
      else {
        Token first = pass->fToken;

        pass->nextToken();
        if (pass->fToken != kMapTo) {
          errorf(pass->fToken.srcpos(), E_MapToExpected, "expected '->'");
          pass->scanUntilNextParameter();
        }
        else {
          Token maptoToken = pass->fToken;

          pass->nextToken();
          if (pass->fToken != kSymbol) {
            errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
            pass->scanUntilNextParameter();
          }
          else {
            Token second = pass->fToken;

            result << ( Token() << first << maptoToken << second );

            pass->nextToken();
          }
        }
      }

      return true;
    }
  };
};


Token
FirstPass::parseImport()
{
  Token expr;
  expr << fToken;
  nextToken();

  if (fToken != kString) {
    errorf(fToken.srcpos(), E_StringExpected, "expected STRING");
    return scanUntilTopExprAndResume();
  }

  Token importFile = fToken;
  expr << importFile;

  nextToken();
  if (fToken == kParanOpen) {
    Token renames = Token(fToken.srcpos(), kParanOpen, kParanClose);

    parseSequence(ImportRenameParser(),
                  kParanOpen, kParanClose, true, E_BadParameterList,
                  renames,
                  "import-renames");

    expr << renames;
  }

  bool canImport = true;
#if defined(UNITTESTS)
  canImport = !Properties::test_dontImport();
#endif

  if (fEvaluateExprs && canImport) {
    try
    {
      String srcName = importFile.stringValue();
      if (!fCompiler->importFile(importFile.srcpos(), srcName, false, fScope))
        return Token();
    }
    catch (const Exception& e) {
      error(importFile.srcpos(), E_UnknownInputFile, e.message());
      return Token();
    }
  }

  return expr;
}


namespace heather
{
  struct TypeParser
  {
    TokenType fEndToken;

    TypeParser(TokenType endToken)
      : fEndToken(endToken)
    { }

    bool operator() (FirstPass* pass, Token& result)
    {
      SrcPos pos = pass->fToken.srcpos();
      Token type = pass->parseTypeSpec(false);
      if (!type.isSet()) {
        errorf(pos, E_UnexpectedToken,
               "returntype expression expected, but found: %s",
               (const char*)StrHelper(pass->fToken.toString()));
        pass->scanUntilNextParameter(fEndToken);
        return true;
      }
      else
        result << type;

      return true;
    }
  };
};


Token
FirstPass::parseSimpleType(const Token& baseToken, bool nextIsParsedYet)
{
  assert(baseToken == kSymbol || baseToken == kQuote);

  Token typeName = baseToken;
  if (baseToken == kQuote) {
    Token t = baseToken;
    nextToken();

    if (fToken != kSymbol) {
      errorf(t.srcpos(), E_UnexpectedQuote, "Unexpected quote");
      return Token();
    }

    typeName = Token() << t << fToken;
  }

  if (!nextIsParsedYet)
    nextToken();

  if (fToken == kGenericOpen) {
    Token generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(kGenericClose),
                  kGenericOpen, kGenericClose, true, E_GenericTypeList,
                  generics,
                  "type-params");
    return Token() << typeName << generics;
  }

  return typeName;
}


Token
FirstPass::parseGroupType()
{
  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "missing '('");
    return scanUntilTopExprAndResume();
  }

  Token nested = Token(fToken.srcpos(), kParanOpen, kParanClose);
  parseSequence(TypeParser(kParanClose),
                kParanOpen, kParanClose, true, E_BadParameterList,
                nested,
                "group-type");

  return nested;
}


Token
FirstPass::parseUnionType()
{
  Token nested = Token(fToken.srcpos(), kUnionOpen, kParanClose);
  parseSequence(TypeParser(kParanClose),
                kUnionOpen, kParanClose, true, E_BadParameterList,
                nested,
                "union-type");
  return nested;
}


Token
FirstPass::parseArrayExtend(const Token& baseType)
{
  Token result = baseType;

  while (fToken == kBracketOpen) {
    Token arrayType = Token(fToken.srcpos(), kBracketOpen, kBracketClose);
    nextToken();

    Token idxExpr;
    if (fToken != kBracketClose) {
      SrcPos idxPos = fToken.srcpos();
      idxExpr = parseExpr(false);
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


bool
FirstPass::isConstraintOperator(const Token& token) const
{
  return  (token == kEqual   || token == kUnequal   || token == kLess ||
           token == kGreater || token == kLessEqual || token == kGreaterEqual ||
           token == kIn      || token == kIsa);
}


Token
FirstPass::parseConstraintExtend(const Token& baseType)
{
  if (isConstraintOperator(fToken))
  {
    Token op = fToken;
    nextToken();

    Token constExpr = parseExpr(false);
    if ( !(constExpr.isLit() || constExpr.isSymbol() ||
           constExpr.isConstRange()))
    {
      error(constExpr.srcpos(), E_ConstExprExpected,
             String("constraint types only accept constant expressions") + constExpr.toString());
      return baseType;
    }

    return Token() << baseType << op << constExpr;
  }

  return baseType;
}


Token
FirstPass::parseFunctionSignature()
{
  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  SrcPos paranPos = fToken.srcpos();
  TokenVector params;
  if (parseFunctionsParams(&params)) {
    Token colonToken;
    Token returnType;

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      returnType = parseTypeSpec(true);
      if (!returnType.isSet()) {
        errorf(pos, E_MissingType, "returntype expression expected");
        returnType = Token(pos, kSymbol, "Any");
      }
    }
    else {
      colonToken = Token(fToken.srcpos(), kColon);
      returnType = Token(fToken.srcpos(), kSymbol, "Any");
    }

    return Token() << ( Token(paranPos, kParanOpen, kParanClose)
                        << params )
                   << colonToken << returnType;
  }
  return Token();
}


Token
FirstPass::parseFunctionType()
{
  Token funcToken = fToken;
  nextToken();

  Token signature = parseFunctionSignature();
  if (signature.isSet())
    return Token() << funcToken
                   << signature.children();
  return Token();
}


Token
FirstPass::parseQuotedType()
{
  assert(fToken == kQuote);
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


Token
FirstPass::parseTypeSpec(bool onlyNestedConstraints)
{
  Token isRefToken;
  if (fToken == kReference) {
    isRefToken = fToken;
    nextToken();
  }

  // TODO: pass the isRefType into the following functions

  Token retval;
  if (fToken == kSymbol) {
    retval = ( onlyNestedConstraints
               ? parseArrayExtend(parseSimpleType(fToken))
               : parseConstraintExtend(parseArrayExtend(parseSimpleType(fToken))) );
  }
  else if (fToken == kFUNCTIONId) {
    retval = parseArrayExtend(parseFunctionType());
  }
  else if (fToken == kQuote) {
    // no constraints for generics
    retval = parseArrayExtend(parseQuotedType());
  }
  else if (fToken == kUnionOpen) {
    // no constraints for union types
    retval = parseArrayExtend(parseUnionType());
  }
  else if (fToken == kParanOpen) {
    // no constraints for sequence types
    retval = parseArrayExtend(parseGroupType());
  }

  if (isRefToken.isSet())
    return Token() << isRefToken << retval;

  return retval;
}


namespace heather
{
  struct LiteralVectorParser
  {
    LiteralVectorParser()
      : fIsDict(false),
        fIsFirst(true)
    { }

    bool operator() (FirstPass* pass, Token& result)
    {
      Token expr = pass->parseExpr(false);

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

          Token toValue = pass->parseExpr(false);
          if (!toValue.isSet()) {
            errorf(mapToken.srcpos(), E_MissingRHExpr,
                   "'->' requires a second expression");
            pass->scanUntilNextParameter();
          }
          else
            result << ( Token() << expr << mapToken << toValue );
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
}


Token
FirstPass::parseLiteralVector()
{
  Token nested = Token(fToken.srcpos(), kLiteralVectorOpen, kParanClose);
  parseSequence(LiteralVectorParser(),
                kLiteralVectorOpen, kParanClose, true, E_BadParameterList,
                nested,
                "literal-vector");
  return nested;
}


namespace heather
{
  struct LiteralArrayParser
  {
    bool operator() (FirstPass* pass, Token& result)
    {
      Token n = pass->parseExpr(false);
      if (n.isSet())
        result << n;
      else {
        errorf(pass->fToken.srcpos(), E_UnexpectedToken,
               "Unexpected token while parsing array: %s",
               (const char*)StrHelper(pass->fToken.toString()));
        return false;
      }

      return true;
    }
  };
}


Token
FirstPass::parseLiteralArray()
{
  Token array = Token(fToken.srcpos(), kLiteralArrayOpen, kBracketClose);
  parseSequence(LiteralArrayParser(),
                kLiteralArrayOpen, kBracketClose, true, E_BadParameterList,
                array,
                "literal-array");
  return array;
}


Token
FirstPass::parseIf()
{
  Token ifToken = fToken;

  nextToken();

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  Token test = parseExpr(false);
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_ParamMissParanClose,
           "Syntax error, missing ')'");
  }
  else
    nextToken();

  Token consequent = parseExpr(false);

  Token result;
  result << ifToken;

  if (test.isSeq())
    result << ( Token(paranPos, kParanOpen, kParanClose)
                << test.children() );
  else
    result << ( Token(paranPos, kParanOpen, kParanClose)
                << test );
  result << consequent;

  if (fToken == kElseId) {
    Token elseToken = fToken;
    nextToken();

    Token alternate = parseExpr(false);

    result << elseToken << alternate;
  }

  return result;
}


Token
FirstPass::parseParameter(ParamType* expected, bool autoCompleteTypes)
{
  Token paramSeq;
  ParamType paramType = kPositional;
  bool doScanOn = true;

  if (fToken.isSeq() && fToken.count() >= 1) {
    size_t ofs = 0;
    if (ofs < fToken.count()) {
      if (fToken[ofs] == kKeyarg) {
        paramType = kNamed;
        ofs++;
        doScanOn = false;
      }
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
    if (fToken == kKeyarg) {
      paramSeq << fToken;
      nextToken();
      paramType = kNamed;
    }

    if (fToken != kSymbol) {
      error(fToken.srcpos(), E_SymbolExpected,
            String("parameter name expected: ") + fToken);
      scanUntilNextParameter();
      return Token();
    }
    else {
      paramSeq << fToken;
      nextToken();

      Token typeIntroToken = fToken;
      if (fToken == kColon ||
          fToken == kAt)
      {
        nextToken();

        SrcPos pos = fToken.srcpos();
        Token type = parseTypeSpec(true);
        if (!type.isSet()) {
          errorf(pos, E_MissingType,
                 "type expression expected");
          if (autoCompleteTypes)
            paramSeq << typeIntroToken << Token(pos, kSymbol, "Any");
        }
        else
          paramSeq << typeIntroToken << type;
      }
      else if (autoCompleteTypes)
        paramSeq << Token(typeIntroToken.srcpos(), kColon)
                 << Token(typeIntroToken.srcpos(), kSymbol, "Any");

      if (fToken == kAssign) {
        Token assignToken = fToken;
        nextToken();

        SrcPos pos = fToken.srcpos();
        Token initExpr = parseExpr(false);
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
          errorf(restToken.srcpos(), E_InvalidRestParam,
                 "orphaned rest parameter");
        }
        else {
          paramSeq << restToken;
          paramType = kRest;
        }
      }
    }
  }

  if (*expected == kPositional) {
    *expected = paramType;
    return paramSeq.unwrapSingleton();
  }
  else if (*expected == kNamed) {
    if (paramType == kPositional)
      errorf(paramSeq.srcpos(), E_ParamOrder,
             "out of order (positional) parameter");
    else {
      *expected = paramType;
      return paramSeq.unwrapSingleton();
    }
  }
  else if (*expected == kRest) {
    errorf(paramSeq.srcpos(), E_ParamOrder,
           "no parameter after rest parameter");
  }

  return Token();
}


namespace heather
{
  struct ParseFuncParamsParser
  {
    FirstPass::ParamType fExpected;
    bool                 fAutoCompleteTypes;

    ParseFuncParamsParser(bool autoCompleteTypes)
      : fExpected(FirstPass::kPositional),
        fAutoCompleteTypes(autoCompleteTypes)
    { }

    bool operator() (FirstPass* pass, Token& result)
    {
      Token param = pass->parseParameter(&fExpected, fAutoCompleteTypes);
      if (param.isSet())
        result << param;
      return true;
    }
  };
};


bool
FirstPass::parseFunctionsParamsFull(TokenVector* exprlist,
                                    TokenType startToken, TokenType endToken,
                                    bool autoCompleteType,
                                    bool acceptEmptyList,
                                    bool skipFirst, bool eatLast)
{
  Token params;
  parseSequence(ParseFuncParamsParser(autoCompleteType),
                startToken, endToken, true, E_BadParameterList,
                params,
                "func-params",
                skipFirst, eatLast);

  if (params.isSet() || acceptEmptyList) {
    *exprlist = params.children();
    return true;
  }

  return false;
}


bool
FirstPass::parseFunctionsParams(TokenVector* exprlist, bool autoCompleteType,
                                bool acceptEmptyList)
{
  return parseFunctionsParamsFull(exprlist,
                                  kParanOpen, kParanClose,
                                  autoCompleteType,
                                  acceptEmptyList,
                                  true, true);
}


Token
FirstPass::parseOn(ScopeType scopeType)
{
  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
    return scanUntilTopExprAndResume();
  }

  Token keyToken = fToken;

  const Macro* macro = fScope->lookupMacro(keyToken.srcpos(),
                                           keyToken.idValue(), true);
  Token macroName = Token(keyToken.srcpos(), baseName(keyToken.idValue()));

  if (macro != NULL) {
    TokenVector dummyArgs;
    Token expr= parseMakeMacroCall(macroName, dummyArgs, macro,
                                   true, /* parseParams */
                                   true, /* is local*/
                                   scopeType);
    return expr;
  }
  else
  {
    bool ignoreStmt = false;

    if (keyToken != Compiler::syncToken &&
        keyToken != Compiler::initToken &&
        keyToken != Compiler::deleteToken &&
        keyToken != Compiler::exitToken &&
        keyToken != Compiler::signalToken) {
      errorf(keyToken.srcpos(), E_UnknownOnKey, "unknown 'on'-keyword.");
      ignoreStmt = true;
    }

    nextToken();
    if (fToken != kParanOpen) {
      errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
      return scanUntilTopExprAndResume();
    }
    SrcPos paranPos = fToken.srcpos();

    TokenVector params;
    if (parseFunctionsParams(&params)) {
      Token body = parseExpr(false);

      if (!ignoreStmt && body.isSet())
        return Token() << tagToken << keyToken
                       << ( Token(paranPos, kParanOpen, kParanClose)
                            << params )
                       << body;
      else
        return Token() << Token(tagToken.srcpos(), "lang|unspecified");
    }

    return Token();
  }
}


Token
FirstPass::parseAnonFun()
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
    Token colonToken;
    Token returnType;

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      returnType = parseTypeSpec(true);
      if (!returnType.isSet()) {
        errorf(pos, E_MissingType, "returntype expression expected");
        returnType = Token(pos, kSymbol, "Any");
      }
    }
    else {
      colonToken = Token(fToken.srcpos(), kColon);
      returnType = Token(fToken.srcpos(), kSymbol, "Any");
    }

    Token body = parseExpr(false);
    if (body.isSet())
      return Token() << funcToken
                     << ( Token(paranPos, kParanOpen, kParanClose)
                          << params )
                     << colonToken << returnType
                     << body;
  }
  return Token();
}


namespace heather
{
  struct FuncallArgsParser
  {
    bool operator() (FirstPass* pass, Token& result)
    {
      if (pass->fToken.isKeyArg()) {
        Token key = pass->fToken;
        pass->nextToken();

        Token val = pass->parseExpr(false);
        if (!val.isSet()) {
          errorf(pass->fToken.srcpos(), E_UnexpectedToken,
                 "Unexpected token while parsing function keyed argument's expr:",
                 (const char*)StrHelper(pass->fToken.toString()));
          pass->scanUntilNextParameter();
          return true;
        }
        result << key;
        result << val;
      }
      else {
        Token val = pass->parseExpr(false);
        if (!val.isSet()) {
          errorf(pass->fToken.srcpos(), E_UnexpectedToken,
                 "unexpected token while parsing function arguments: ",
                 (const char*)StrHelper(pass->fToken.toString()));
          pass->scanUntilNextParameter();
          return true;
        }
        result << val;
      }

      return true;
    }
  };
};


void
FirstPass::parseFuncallArgs(TokenVector* argsVector)
{
  Token args;
  parseSequence(FuncallArgsParser(),
                kParanOpen, kParanClose, true, E_BadParameterList,
                args, "funcall-args", false);

  if (args.isSeq())
    *argsVector = args.children();
}


Token
FirstPass::parseFunctionCall(const Token& expr,
                             const TokenVector& preScannedArgs,
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

  return Token() << expr
                 << ( Token(expr.srcpos(), kParanOpen, kParanClose)
                      << effArgs );
}


Token
FirstPass::parseParamCall(const Token& expr,
                          const TokenVector& preScannedArgs,
                          bool shouldParseParams)
{
  if (expr.isSymbol()) {
    const Macro* macro = fScope->lookupMacro(expr.srcpos(),
                                             expr.idValue(), true);
    Token macroName = Token(expr.srcpos(), baseName(expr.idValue()));

    if (macro != NULL) {
      Token expr= parseMakeMacroCall(macroName, preScannedArgs,
                                     macro,
                                     shouldParseParams, true,
                                     kNonScopedDef);
      return expr;
    }
  }

  return parseFunctionCall(expr, preScannedArgs, shouldParseParams);
}


Token
FirstPass::parseSlice(const Token& expr)
{
  SrcPos startPos = fToken.srcpos();
  nextToken();

  if (fToken == kBracketClose) {
    nextToken();

    // the expression is an array type name
    return Token() << expr << Token(startPos, kBracketOpen, kBracketClose);
  }
  else {
    Token idx = parseExpr(false);

    if (fToken != kBracketClose)
      errorf(fToken.srcpos(), E_MissingBracketClose, "expected ']'");
    else
      nextToken();

    return Token() << Token(startPos, "slice")
                   << ( Token(startPos, kParanOpen, kParanClose)
                        << expr
                        << Token(idx.srcpos(), kComma)
                        << idx );
  }
}


Token
FirstPass::parseAccess(const Token& expr)
{
  TokenVector args;

  if (fToken == kParanOpen) {
    nextToken();
    return parseAccess(parseParamCall(expr, args, true));
  }
  else if (fToken == kBracketOpen) {
    return parseAccess(parseSlice(expr));
  }
  else if (fToken == kDot) {
    nextToken();
    if (fToken != kSymbol) {
      errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
      return scanUntilTopExprAndResume();
    }
    Token symToken = fToken;

    nextToken();
    args.push_back(expr);
    if (fToken == kParanOpen) {
      nextToken();
      return parseAccess(parseParamCall(symToken, args, true));
    }
    else if (fToken == kBracketOpen || fToken == kDot)
      return parseAccess(parseParamCall(symToken, args, false));
    else
      return parseParamCall(symToken, args, false);
  }

  return expr;
}


Token
FirstPass::parseGroup()
{
  Token expr = parseExpr(true);
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_MissingParanClose, "expected closing ')'");
  }
  else
    nextToken();

  return expr;
}


bool
FirstPass::parseExprListUntilBrace(TokenVector* result,
                                   bool endAtToplevelId,
                                   bool isLocal)
{
  for ( ; ; ) {
    if (fToken == kDefId ||
        fToken == kExtendId ||
        fToken == kExternId ||
        fToken == kExportId ||
        fToken == kImportId ||
        fToken == kModuleId)
    {
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
        parseDef(isLocal, kNonScopedDef);
        continue;
      }

      TokenVector exprs = parseDef(isLocal, kNonScopedDef);
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
      Token expr = parseWhen(!isLocal, kNonScopedDef);
      if (expr.isSet())
        result->push_back(expr);
    }
    else {
      Token before = fToken;
      SrcPos startPos = fToken.srcpos();
      Token expr = parseExpr(true);

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


Token
FirstPass::parseBlock()
{
  SrcPos startPos = fToken.srcpos();
  nextToken();

  TokenVector exprlist;
  parseExprListUntilBrace(&exprlist, false, true);

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


Token
FirstPass::parseUnaryOp(const Token& inOpToken)
{
  Token opToken = inOpToken;
  nextToken();

  Token t = parseAtomicExpr();
  if (!t.isSet()) {
    errorf(opToken.srcpos(), E_UnexpectedToken, "expected expression");
    return Token();
  }
  return Token() << opToken
                 << ( Token(opToken.srcpos(), kParanOpen, kParanClose)
                      << t );
}


namespace heather
{
  struct BasePatternParser
  {
  protected:
    TokenVector parseConsequent(FirstPass* pass, bool mapToReq)
    {
      TokenVector result;

      Token mapToToken;
      if (mapToReq) {
        assert(pass->fToken == kMapTo);
        mapToToken = pass->fToken;
        pass->nextToken();
      }

      Token body = pass->parseExpr(true);
      if (body.isSet()) {
        if (mapToReq)
          result.push_back(mapToToken);
        result.push_back(body);
      }
      return result;
    }
  };


  struct SelectPatternParser : public BasePatternParser
  {
    SelectPatternParser()
      : fElseSeen(false)
    {}

    bool operator() (FirstPass* pass, Token& result)
    {
      if (pass->fToken != kPipe) {
        errorf(pass->fToken.srcpos(), E_ExpectedPipe,
               "expect '|'");
        pass->scanUntilBrace();
        return false;
      }
      Token pipeToken = pass->fToken;
      pass->nextToken();

      if (pass->fToken == kElseId) {
        bool ignore = false;
        Token elseToken = pass->fToken;

        if (fElseSeen) {
          errorf(pass->fToken.srcpos(), E_RedefinedPattern,
                 "'else' pattern redefined");
          ignore = true;
        }
        fElseSeen = true;
        pass->nextToken();

        TokenVector consq = parseConsequent(pass, false);
        if (!ignore && !consq.empty())
          result << ( Token() << pipeToken << elseToken << consq );
      }
      else {
        TokenVector pattern;
        for ( ; ; ) {
          if (pass->fToken == kEOF)
            return false;

          if (fElseSeen) {
            errorf(pass->fToken.srcpos(), E_ElseNotLastPattern,
                   "'else' must be last pattern");
            pass->scanUntilBrace();
            return false;
          }
          else {
            Token test = pass->parseExpr(false);
            if (test.isSet()) {
              pattern.push_back(test);

              if (pass->fToken == kComma) {
                pattern.push_back(pass->fToken);
                pass->nextToken();
              }
              else if (pass->fToken == kMapTo)
                break;
              else {
                errorf(pass->fToken.srcpos(), E_BadPatternList,
                       "unexpected token");
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

        TokenVector consq = parseConsequent(pass, true);
        if (!pattern.empty() && !consq.empty()) {
          result << ( Token() << pipeToken << ( pattern.size() == 1
                                                ? pattern[0]
                                                : ( Token() << pattern ) )
                      << consq );
        }
      }

      return true;
    }

    bool fElseSeen;
  };
};


Token
FirstPass::parseSelect()
{
  assert(fToken == kSelectId);
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

  if (fToken != kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
    return scanUntilTopExprAndResume();
  }

  Token patterns = Token(fToken.srcpos(), kBraceOpen, kBraceClose);
  parseSequence(SelectPatternParser(),
                kBraceOpen, kBraceClose, false, E_BadPatternList,
                patterns, "select-pattern");

  return Token() << selectToken << ( Token(paranPos, kParanOpen, kParanClose)
                                     << args )
                 << patterns;
}


namespace heather
{
  struct MatchPatternParser : public BasePatternParser
  {
    bool operator() (FirstPass* pass, Token& result)
    {
      if (pass->fToken != kPipe) {
        errorf(pass->fToken.srcpos(), E_ExpectedPipe,
               "expect '|'");
        pass->scanUntilBrace();
        return false;
      }
      Token pipeToken = pass->fToken;
      pass->nextToken();

      if (pass->fToken != kSymbol && pass->fToken != kColon) {
        errorf(pass->fToken.srcpos(), E_SymbolExpected,
               "variable name or ':' expected");
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

      Token matchType = pass->parseTypeSpec(true);
      if (!matchType.isSet()) {
        pass->scanUntilBrace();
        return false;
      }

      if (pass->fToken != kMapTo) {
        errorf(pass->fToken.srcpos(), E_BadPatternList,
               "expected '->'");
        pass->scanUntilBrace();
        return false;
      }

      TokenVector consq = parseConsequent(pass, true);
      if (colonToken.isSet() && matchType.isSet() && !consq.empty()) {
        if (varToken.isSet())
          result << ( Token()
                      << pipeToken
                      << ( Token() << varToken << colonToken << matchType )
                      << consq );
        else
          result << ( Token()
                      << pipeToken
                      << ( Token() << colonToken << matchType )
                      << consq );
      }
      return true;
    }
  };
};


Token
FirstPass::parseMatch()
{
  assert(fToken == kMatchId);
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

  if (fToken != kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
    return scanUntilTopExprAndResume();
  }

  Token patterns = Token(fToken.srcpos(), kBraceOpen, kBraceClose);
  parseSequence(MatchPatternParser(),
                kBraceOpen, kBraceClose, false, E_BadPatternList,
                patterns, "match-pattern");

  return Token() << matchToken << ( Token(paranPos, kParanOpen, kParanClose)
                                    << args )
                 << patterns;
}


namespace heather
{
  struct ForClauseParser
  {
    bool parseInCollClause(FirstPass* pass, Token& result,
                           const Token& symToken,
                           const Token& colonToken,
                           const Token& type)
    {
      assert(pass->fToken == kIn);
      Token inToken = pass->fToken;
      pass->nextToken();

      Token collToken = pass->parseExpr(false);
      if (!collToken.isSet()) {
        error(pass->fToken.srcpos(), E_MissingRHExpr,
              String("unexpected token: ") + pass->fToken.toString());
        pass->scanUntilNextParameter();
        return true;
      }

      Token varClause;
      if (colonToken.isSet() && type.isSet())
        varClause = Token() << symToken << colonToken << type;
      else
        varClause = symToken;

      Token subexpr;
      subexpr << varClause << inToken << collToken;

      result << subexpr;
      return true;
    }


    bool parseExplicitClause(FirstPass* pass, Token& result,
                             const Token& symToken,
                             const Token& colonToken,
                             const Token& type)
    {
      assert(pass->fToken == kAssign);
      Token assignToken = pass->fToken;
      pass->nextToken();

      Token iterator = pass->parseExpr(false);
      if (!iterator.isSet()) {
        error(pass->fToken.srcpos(), E_MissingRHExpr,
              String("unexpected token: ") + pass->fToken.toString());
        pass->scanUntilNextParameter();
        return true;
      }

      Token varClause;
      if (colonToken.isSet() && type.isSet())
        varClause = Token() << symToken << colonToken << type;
      else
        varClause = symToken;

      Token subexpr;
      subexpr << varClause << assignToken << iterator;

      result << subexpr;
      return true;
    }


    bool operator() (FirstPass* pass, Token& result)
    {
      if (pass->fToken == kSymbol) {
        bool allowNormalExpr = true;

        Token symToken = pass->fToken;
        pass->nextToken();

        Token type;
        Token colonToken;

        if (pass->fToken == kColon) {
          colonToken = pass->fToken;
          pass->nextToken();
          SrcPos pos = pass->fToken.srcpos();
          type = pass->parseTypeSpec(true);
          if (!type.isSet()) {
            errorf(pos, E_MissingType, "type expression expected");
            type = Token(pos, kSymbol, "Any");
          }
          allowNormalExpr = false;
        }

        if (pass->fToken == kIn) {
          return parseInCollClause(pass, result, symToken, colonToken, type);
        }
        else if (pass->fToken == kAssign) {
          return parseExplicitClause(pass, result,
                                     symToken, colonToken, type);
        }
        else {
          Token first = pass->parseAccess(pass->parseSimpleType(symToken, true));
          if (allowNormalExpr) {
            OperatorType op1 = tokenTypeToOperator(pass->fToken.tokenType());
            SrcPos op1Srcpos = pass->fToken.srcpos();

            if (op1 != kOpInvalid) {
              Token expr = pass->parseExprRec(first.toTokenVector(), op1,
                                              op1Srcpos, false);
              if (expr.isSet()) {
                result << expr;
                return true;
              }
            }
            else {
              result << first;
              return true;
            }
          }

          error(pass->fToken.srcpos(), E_UnexpectedToken,
                String("unexpected token in for clause (1): ") +
                pass->fToken.toString());
          pass->scanUntilNextParameter();
        }
      }
      else {
        Token expr = pass->parseExpr(false);
        if (expr.isSet()) {
          result << expr;
          return true;
        }

        error(pass->fToken.srcpos(), E_UnexpectedToken,
              String("unexpected token in for clause (2): ") + pass->fToken.toString());
        pass->scanUntilNextParameter();
      }

      return true;
    }
  };
};


Token
FirstPass::parseFor()
{
  assert(fToken == kForId);
  Token forToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  Token args = Token(paranPos, kParanOpen, kParanClose);
  parseSequence(ForClauseParser(),
                kParanOpen, kParanClose, true, E_BadParameterList,
                args, "for-clauses", false);

  Token body = parseExpr(true);

  Token elseToken;
  Token alternate;
  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();

    alternate = parseExpr(false);
  }

  if (body.isSet()) {
    Token result = Token() << forToken
                           << args
                           << body;
    if (elseToken.isSet() && alternate.isSet())
      result << elseToken << alternate;
    return result;
  }
  return Token() << Token(forToken.srcpos(), "lang|unspecified");
}


Token
FirstPass::parseUnitNumber(const Token& token)
{
  Token number = token;
  nextToken();
  if (fToken == kQuote) {
    Token bqToken = fToken;
    nextToken();

    if (!fToken.isCharOrUnitName()) {
      errorf(fToken.srcpos(), E_UnitExpected, "unit name expected");
      return number;
    }
    Token unitToken = ( fToken == kSymbol
                        ? fToken
                        : Token(fToken.srcpos(), kSymbol, fToken.toString()) );
    nextToken();

    return Token() << number << bqToken << unitToken;
  }
  return number;
}


Token
FirstPass::parseExplicitTypedNumber(const Token& token)
{
  Token number = token;

  if (fToken == kColon) {
    Token colonToken = fToken;
    nextToken();

    SrcPos typePos = fToken.srcpos();
    Token type = parseTypeSpec(true);
    if (!type.isSet()) {
      errorf(typePos, E_MissingType, "expected type specifier");
      return number;
    }

    return Token() << number << colonToken << type;
  }
  return number;
}


Token
FirstPass::parseAtomicExpr()
{
  switch (fToken.tokenType()) {
  case kInt:
  case kReal:
  case kRational:
    return parseExplicitTypedNumber(parseUnitNumber(fToken));

  case kBool:
  case kString:
  case kChar:
  case kKeyword:
  case kNilId:
  case kEofId:
  case kSeqExpr:
  case kNestedExpr:
    {
      Token t = fToken;
      nextToken();
      return t;
    }

  case kIfId:
    return parseIf();
  case kOnId:
    return parseOn(kNonScopedDef);
  case kFunctionId:
    return parseAnonFun();
  case kNotId:                  // unary not operator
  case kMinus:                  // unary negate
    return parseUnaryOp(fToken);

  case kWhenId:
    return parseWhen(false, kNonScopedDef);
  case kSelectId:
    return parseSelect();
  case kMatchId:
    return parseMatch();
  case kForId:
    return parseFor();

  case kLetId:
    assert(0);
    break;

  case kSymbol:
  case kQuote:
    return parseAccess(parseSimpleType(fToken));

  case kLiteralVectorOpen:
    return parseAccess(parseLiteralVector());

  case kLiteralArrayOpen:
    return parseAccess(parseLiteralArray());

  case kParanOpen:
    nextToken();
    return parseAccess(parseGroup());
  case kBraceOpen:
    return parseAccess(parseBlock());

  default:
    ;
  }

  return Token();
}


Token
FirstPass::makeAssignToken(const Token& expr1, const Token& expr2,
                           const SrcPos& op1Srcpos) const
{
  if (expr1.isSymFuncall()) {
    // rename the function call in expr to name! and append expr2 as last
    // parameter to expr1's parameter list
    return Token() << Token(expr1[0].srcpos(), expr1[0].idValue() + "!")
                   << ( Token(op1Srcpos, kParanOpen, kParanClose)
                        << expr1[1].children()
                        << Token(op1Srcpos, kComma)
                        << expr2 );
  }
  else {
    return Token() << expr1
                   << Token(op1Srcpos, kAssign)
                   << expr2;
  }
}


Token
FirstPass::makeAssignToken(const TokenVector& exprs, const Token& expr2,
                           const SrcPos& op1Srcpos,
                           bool hasRest) const
{
  if (exprs.size() > 1) {
    Token block = Token(op1Srcpos, kBraceOpen, kBraceClose);

    Token tempSymToken = Token::newUniqueSymbolToken(op1Srcpos, "value");
    block << ( Token() << Token(op1Srcpos, kLetId)
               << tempSymToken
               << Token(op1Srcpos, kAssign)
               << expr2 );

    for (size_t i = 0; i < exprs.size(); i++) {
      const Token& expr = exprs[i];

      const char* funcName = ( i == exprs.size() - 1 && hasRest
                               ? "slice*"
                               : "slice" );
      Token sliceExpr = Token() << Token(op1Srcpos, funcName)
                                << ( Token(op1Srcpos, kParanOpen, kParanClose)
                                     << tempSymToken
                                     << Token(op1Srcpos, kComma)
                                     << Token(op1Srcpos, kInt, (int)i) );
      Token retv;
      if (expr.isSymFuncall()) {
        // rename the function call in expr to name! and append expr2 as last
        // parameter to expr1's parameter list
        retv = Token() << Token(expr[0].srcpos(), expr[0].idValue() + "!")
                       << ( Token(op1Srcpos, kParanOpen, kParanClose)
                            << expr[1].children()
                            << Token(op1Srcpos, kComma)
                            << sliceExpr );
      }
      else {
        retv = Token() << expr
                       << Token(op1Srcpos, kAssign)
                       << sliceExpr;
      }

      block << retv;
    }

    return block;
  }

  assert(exprs.size() == 1);
  return makeAssignToken(exprs[0], expr2, op1Srcpos);
}


Token
FirstPass::makeBinaryToken(const Token& expr1, OperatorType op1,
                           const Token& expr2,
                           const SrcPos& op1Srcpos) const
{
  if (op1 == kOpAssign)
    return makeAssignToken(expr1, expr2, op1Srcpos);
  else if ( op1 == kOpRange && expr2.isBinarySeq(kBy) ) {
    return Token() << expr1
                   << Token(op1Srcpos, operatorToTokenType(op1))
                   << expr2[0]
                   << expr2[1]
                   << expr2[2];
  }
  else if (op1 == kOpBy && expr1.isBinarySeq(kRange)) {
    return Token() << expr1[0]
                   << expr1[1]
                   << expr1[2]
                   << Token(op1Srcpos, operatorToTokenType(op1))
                   << expr2;
  }
  else if (op1 == kOpThen && expr2.isBinarySeq(kWhileId)) {
    return Token() << expr1
                   << Token(op1Srcpos, operatorToTokenType(op1))
                   << expr2[0]
                   << expr2[1]
                   << expr2[2];
  }
  else if (op1 == kOpWhile && expr1.isBinarySeq(kThenId)) {
    return Token() << expr1[0]
                   << expr1[1]
                   << expr1[2]
                   << Token(op1Srcpos, operatorToTokenType(op1))
                   << expr2;
  }
  else
    return Token() << expr1
                   << Token(op1Srcpos, operatorToTokenType(op1))
                   << expr2;
}


bool
FirstPass::isRightOperator(OperatorType op1) const
{
  return (op1 == kOpAssign);
}


int
FirstPass::weightOperator(OperatorType op1) const
{
  switch (op1) {
  case kOpMapTo:
  case kOpThen:
  case kOpWhile:          return  10;

  case kOpLogicalAnd:
  case kOpLogicalOr:      return  20;

  case kOpIsa:            return  30;

  case kOpEqual:
  case kOpUnequal:
  case kOpLess:
  case kOpLessEqual:
  case kOpGreater:
  case kOpGreaterEqual:
  case kOpCompare:        return  40;

  case kOpIn:             return  50;

  case kOpRange:
  case kOpBy:
  case kOpAppend:         return  60;

  case kOpBitAnd:
  case kOpBitOr:
  case kOpBitXor:         return  70;

  case kOpShiftLeft:
  case kOpShiftRight:     return  80;

  case kOpFold:
  case kOpPlus:
  case kOpMinus:          return  90;

  case kOpMultiply:
  case kOpDivide:
  case kOpMod:            return 100;

  case kOpExponent:       return 110;

  case kOpAs:             return 200;

  case kOpAssign:         return 999999;

  case kOpInvalid:
    assert(0);
  }

  return 999999;
}


bool
FirstPass::isOpWeightAbove(OperatorType op1, OperatorType op2) const
{
  return weightOperator(op1) > weightOperator(op2);
}


Token
FirstPass::parseExprRec(const TokenVector& exprs,
                        OperatorType op1, const SrcPos& op1Srcpos,
                        bool hasRest)
{
  assert(exprs.size() > 0);

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
    expr2 = parseTypeSpec(true);
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
      return Token() << exprs[0]
                     << Token(op1Srcpos, operatorToTokenType(op1))
                     << expr2;
    }
  }
  else {
    if (exprs.size() > 1) {
      errorf(exprs[0].srcpos(), E_BadLHExpr,
             "Multiple left hand variables only allowed with assignments.");
      return Token();
    }

    if (!isRightOperator(op1) && isOpWeightAbove(op1, op2))
      return parseExprRec(makeBinaryToken(exprs[0], op1,
                                          expr2, op1Srcpos).toTokenVector(),
                          op2,
                          op2Srcpos, false);
    else
      return makeBinaryToken(exprs[0], op1,
                             parseExprRec(expr2.toTokenVector(),
                                          op2, op2Srcpos, false),
                             op1Srcpos);
  }
}


Token
FirstPass::parseExpr(bool acceptComma)
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


void
FirstPass::parseTopExprUntilBrace(TokenVector* result, ScopeType scope)
{
  while (fToken != kBraceClose) {
    if (fToken == kEOF)
      break;
    TokenVector topexprs = parseTop(scope);
    if (!topexprs.empty())
      result->insert(result->end(), topexprs.begin(), topexprs.end());
  }

  if (fToken == kBraceClose)
    nextToken();
}


TokenVector
FirstPass::parseTopOrExprList(bool isTopLevel, ScopeType scope)
{
  if (isTopLevel) {
    if (fToken == kBraceOpen) {
      SrcPos bracePos = fToken.srcpos();
      nextToken();

      TokenVector exprs;
      parseTopExprUntilBrace(&exprs, scope);

      return exprs;
    }

    TokenVector exprs = parseTop(scope);
    return exprs;
  }
  return parseExpr(false).toTokenVector();
}


Token
FirstPass::multiExprsToBlock(const TokenVector& exprs)
{
  if (exprs.size() > 1)
    return wrapInBlock(exprs[0].srcpos(), exprs);
  else if (exprs.size() == 1)
    return exprs[0];
  else
    return Token();
}


Token
FirstPass::wrapInBlock(const SrcPos& srcpos, const TokenVector& exprs)
{
  return Token(srcpos, kBraceOpen, kBraceClose) << exprs;
}


bool
FirstPass::scanBlock(bool isTopLevel, ScopeType scope)
{
  if (fToken == kBraceOpen) {
    SrcPos startPos = fToken.srcpos();
    int braceCount = 0;
    for ( ; ; ) {
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
    parseTop(scope);
  else
    parseExpr(true);
  return true;
}


Token
FirstPass::parseWhen(bool isTopLevel, ScopeType scope)
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
      errorf(fToken.srcpos(), E_UnexpectedToken,
             "only 'ignore' or 'include' are valid symbols here");
      errorf(fToken.srcpos(), E_UnexpectedToken, "assume 'ignore'");

      inclConsequent = false;
    }
    nextToken();
  }
  else if (fToken == kParanOpen) {
    SrcPos paranPos = fToken.srcpos();
    nextToken();

    Token test = parseExpr(false);
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
      TokenEvalContext ctx(fCompiler->configVarRegistry());
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
      result << ( Token(paranPos, kParanOpen, kParanClose) << test );
  }
  else if (fToken == kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "missing parameters or key for 'when' clause");
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
    consequent = multiExprsToBlock(parseTopOrExprList(isTopLevel, scope));
  }
  else {
    if (!scanBlock(isTopLevel, scope))
      return Token();
  }

  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();

    if (inclAlternate) {
      ValueSaver<bool> keep(fEvaluateExprs, inclAlternate);
      alternate = multiExprsToBlock(parseTopOrExprList(isTopLevel, scope));
    }
    else {
      if (!scanBlock(isTopLevel, scope))
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

  assert(0);
  return Token();
}


Token
FirstPass::parseExtend(ScopeType scope)
{
  assert(fToken == kExtendId);
  Token extendToken = fToken;
  nextToken();

  if (fToken != kModuleId) {
    errorf(fToken.srcpos(), E_UnexpectedToken, "expected 'module'");
    return scanUntilTopExprAndResume();
  }

  Token moduleToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
    return scanUntilTopExprAndResume();
  }

  Token modNameToken = fToken;
  nextToken();

  if (fToken != kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
    return scanUntilTopExprAndResume();
  }

  Token code;
  {
    ModuleHelper modHelper(this, modNameToken.idValue(), true);

    code = wrapInBlock(fToken.srcpos(), parseTopOrExprList(true, scope));
  }

  if (code.isSet())
    return Token() << extendToken << moduleToken
                   << modNameToken << code;
  else
    return Token();
}


TokenVector
FirstPass::parseExtern()
{
  assert(fToken == kExternId);
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

  if (linkageType == String("C")) {
    Ptr<ExternCParser> externc = new ExternCParser(this);
    return externc->parseBlock();
  }

  errorf(fToken.srcpos(), E_UnknownLinkage, "Unknown linkage type: '%s'",
         (const char*)StrHelper(linkageType));
  return scanUntilTopExprAndResume().toTokenVector();
}


TokenVector
FirstPass::parseVarDef(const Token& defToken, const Token& tagToken, bool isLocal)
{
  Token keepTagToken = tagToken;

  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "Missing name");
    return scanUntilTopExprAndResume().toTokenVector();
  }
  Token symbolToken = fToken;

  nextToken();

  return parseVarDef2(defToken, keepTagToken, symbolToken, isLocal, Token());
}


Token
FirstPass::evaluateConfigExpr(const Token& initExpr)
{
  TokenEvalContext ctx(fCompiler->configVarRegistry());
  return ctx.evalToken(initExpr);
}


TokenVector
FirstPass::parseVarDef2(const Token& defToken, const Token& tagToken,
                        const Token& symbolToken,
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

    assert(!ellipsisToken.isSet());

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      type = parseTypeSpec(true);
      if (!type.isSet()) {
        errorf(pos, E_MissingType, "type expression expected");
        type = Token(pos, kSymbol, "Any");
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
    initExpr = parseExpr(false);
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
      assert(initValueSym.isSet());

      const char* funcName = ( i == leftHands.size() - 1 && ellipsisToken.isSet()
                               ? "slice*"
                               : "slice" );

      effInitExpr << Token(vardefSym.srcpos(), funcName)
                  << ( Token(vardefSym.srcpos(), kParanOpen, kParanClose)
                       << initValueSym
                       << Token(vardefSym.srcpos(), kComma)
                       << Token(initExpr.srcpos(), kInt, (int)i) );
    }
    else
      effInitExpr = initExpr;

    if (tagToken == Compiler::configToken) {
      if (fEvaluateExprs) {
        if (!effInitExpr.isSet()) {
          error(vardefSym.srcpos(), E_DefNoInitValue,
                ( String("Config variable '") + symbolToken +
                  "' without default value") );

          // if no default value is given assume ''
          effInitExpr = Token(vardefSym.srcpos(), kString, "");
          if (!assignToken.isSet())
            assignToken = Token(vardefSym.srcpos(), kAssign);
        }
        fCompiler->configVarRegistry()->registerValue(vardefSym.idValue(),
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


Token
FirstPass::parseCharDef(const Token& defToken)
{
  Token tagToken = fToken;

  nextToken();

  if (!fToken.isCharOrUnitName()) {
    errorf(fToken.srcpos(), E_MissingDefName, "missing char name");
    return scanUntilTopExprAndResume();
  }
  Token charNameToken = ( fToken == kSymbol
                          ? fToken
                          : Token(fToken.srcpos(), kSymbol, fToken.toString()) );

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

  if (fToken != kInt) {
    errorf(fToken.srcpos(), E_DefInitUnexpToken,
           "expected INTEGER");
    codePointToken = Token(fToken.srcpos(), kInt, 0xffff);
  }
  else {
    codePoint = fToken.intValue();
    if (codePoint < 0 || codePoint > 0x10FFFF) {
      errorf(fToken.srcpos(), E_BadCharValue, "invalid expected INTEGER");

      codePointToken = Token(fToken.srcpos(), kInt, 0xffff);
      codePoint = 0xffff;
    }
    nextToken();
  }

  if (fEvaluateExprs) {
    fCompiler->charRegistry()->registerValue(charNameToken.idValue(),
                                           codePoint);
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


Token
FirstPass::parseWhereClause()
{
  assert(fToken == kWhereId);

  Token whereToken = fToken;
  nextToken();

  TokenVector constraints;
  Token delayedCommaToken;

  for ( ; ; ) {
    if (fToken == kEOF) {
      errorf(fToken.srcpos(), E_UnexpectedEOF, "unexpected eof while scanning 'where' clause");
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

    if (op1 == kOpEqual || op1 == kOpUnequal ||
        op1 == kOpLess  || op1 == kOpLessEqual ||
        op1 == kOpGreater || op1 == kOpGreaterEqual ||
        op1 == kOpIn)
    {
      nextToken();

      Token constrExpr = parseExpr(false);
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

      Token typeConstraint = parseTypeSpec(false);
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


Token
FirstPass::parseReifyClause()
{
  assert(fToken == kReifyId);
  Token reifyToken = fToken;

  nextToken();

  TokenVector funcDefs;
  Token delayedCommaToken;

  for ( ; ; ) {
    if (fToken == kEOF) {
      errorf(fToken.srcpos(), E_UnexpectedEOF, "unexpected eof while scanning 'reify' clause");
      return Token();
    }

    Token funcDefExpr = parseFunctionSignature();
    if (funcDefExpr.isSet()) {
      if (delayedCommaToken.isSet()) {
        funcDefs.push_back(delayedCommaToken);
        delayedCommaToken = Token();
      }
      funcDefs.push_back(funcDefExpr);
    }

    if (fToken == kComma) {
      delayedCommaToken = fToken;
      nextToken();
    }
    else
      break;
  }

  if (!funcDefs.empty())
    return Token() << reifyToken << funcDefs;
  return Token();
}


Token
FirstPass::parseFunctionDef(const Token& defToken, const Token& tagToken,
                            const Token& symToken, bool isLocal,
                            const Token& linkage)
{
  assert(fToken == kParanOpen);
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
    Token colonToken;
    Token returnType;
    Token reifyClause;
    Token whereClause;

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      SrcPos pos = fToken.srcpos();
      returnType = parseTypeSpec(true);
      if (!returnType.isSet()) {
        errorf(pos, E_MissingType, "type expression expected");
        returnType = Token(pos, kSymbol, "Any");
      }
    }
    else {
      colonToken = Token(fToken.srcpos(), kColon);
      returnType = Token(fToken.srcpos(), kSymbol, "Any");
    }

    if (fToken == kReifyId)
      reifyClause = parseReifyClause();

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

      if (isLocal) {
        body = parseExpr(false);
        if (!body.isSet()) {
          errorf(bodyPos, E_MissingBody, "expected function body");
          return Token();
        }
      }
      else {
        TokenVector bodyExprs;
        parseExprListUntilBrace(&bodyExprs, !isLocal, true);

        body = wrapInBlock(bodyPos, bodyExprs);
      }
    }

    result << ( Token(paranOpenToken.srcpos(), kParanOpen, kParanClose)
                << params );
    if (colonToken.isSet())
      result << colonToken << returnType;

    if (reifyClause.isSet())
      result << reifyClause;

    if (whereClause.isSet())
      result << whereClause;

    if (docString.isSet())
      result << docString;

    result << body;

    return result;
  }

  return Token();
}


TokenVector
FirstPass::parseFunctionOrVarDef(const Token& defToken, bool isLocal,
                                 const Token& linkage)
{
  assert(fToken == kSymbol);

  Token symToken = fToken;

  const Macro* macro = fScope->lookupMacro(symToken.srcpos(),
                                           symToken.idValue(), true);
  Token macroName = Token(symToken.srcpos(), baseName(symToken.idValue()));

  if (macro != NULL) {
    if (linkage.isSet())
      errorf(linkage.srcpos(), E_UnexpLinkage,
             "Unsupported linkage for macro appliance ignored");

    TokenVector dummyArgs;
    Token expr= parseMakeMacroCall(macroName, dummyArgs, macro,
                                   true /* parseParams */, isLocal,
                                   kNonScopedDef);
    return expr.toTokenVector();
  }
  else
  {
    nextToken();
    if (fToken == kParanOpen)
      return parseFunctionDef(defToken, Token(), symToken, isLocal,
                              linkage).toTokenVector();

    return parseVarDef2(defToken, Token(), symToken, isLocal,
                        linkage);
  }

  return TokenVector();
}


Token
FirstPass::parseGenericFunctionDef(const Token& defToken, bool isLocal)
{
  Token tagToken;
  if (isLocal) {
    errorf(fToken.srcpos(), E_LocalGenericFunc,
           "inner generic functions are not supported.  'generic' ignored");
  }
  else
    tagToken = fToken;

  nextToken();
  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected function name");
    return scanUntilTopExprAndResume();
  }
  Token symToken = fToken;

  nextToken();
  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  return parseFunctionDef(defToken, tagToken, symToken, isLocal, Token());
}


Token
FirstPass::parseAliasDef(const Token& defToken, bool isLocal)
{
  assert(fToken == Compiler::aliasToken);

  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected alias name");
    return scanUntilTopExprAndResume();
  }
  Token symToken = fToken;
  nextToken();

  Token generics;
  if (fToken == kGenericOpen) {
    generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(kGenericClose),
                  kGenericOpen, kGenericClose, true, E_GenericTypeList,
                  generics,
                  "alias-params");
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
  Token type = parseTypeSpec(false);
  if (!type.isSet()) {
    errorf(pos, E_MissingType, "type expression expected");
    return scanUntilTopExprAndResume();
  }

  Token result = Token() << defToken << tagToken
                         << symToken;
  if (generics.isSet())
    result << generics;

  if (whereClause.isSet())
    result << whereClause;


  if (docString.isSet())
    result << docString;

  result << assignToken << type;
  return result;
}


Token
FirstPass::parseOptDocString()
{
  Token docString;
  if (fToken == kDocString) {
    if (!Properties::shouldIgnoreDocStrings())
      docString = fToken;
    nextToken();
  }
  return docString;
}


Token
FirstPass::parseTypeDef(const Token& defToken, bool isClass, bool isLocal)
{
  assert((isClass && fToken == Compiler::classToken) ||
         (!isClass && fToken == Compiler::typeToken) );

  Token tagToken;
  if (isLocal) {
    errorf(fToken.srcpos(), E_LocalTypeDef,
           "inner type/class definitions are not supported.");
    return scanUntilTopExprAndResume();
  }
  else
    tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected alias name");
    return scanUntilTopExprAndResume();
  }
  Token symToken = fToken;
  nextToken();

  Token generics;
  if (fToken == kGenericOpen) {
    generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(kGenericClose),
                  kGenericOpen, kGenericClose, true, E_GenericTypeList,
                  generics,
                  "typedef-params");
  }

  Token ctorParams;
  if (fToken == kParanOpen) {
    SrcPos paranPos = fToken.srcpos();
    if (isClass) {
      TokenVector params;
      if (!parseFunctionsParams(&params))
        return scanUntilTopExprAndResume();

      ctorParams = Token(paranPos, kParanOpen, kParanClose) << params;
    }
    else {
      errorf(paranPos, E_CtorNotInTypes,
             "ctor parameters are not allowed in 'type' def");
      nextToken();
      scanUntilEndOfParameters();
    }
  }

  Token colonToken;
  Token isaType;
  if (fToken == kColon) {
    colonToken = fToken;
    nextToken();
    SrcPos pos = fToken.srcpos();
    isaType = parseTypeSpec(true);
    if (!isaType.isSet()) {
      errorf(pos, E_MissingType, "type expression expected");
      isaType = Token(fToken.srcpos(), kSymbol, "Any");
    }
  }

  Token whereClause;
  if (fToken == kWhereId)
    whereClause = parseWhereClause();

  Token docString = parseOptDocString();

  TokenVector requiredProtocols;
  SrcPos bracePos;
  if (fToken == kBraceOpen) {
    bracePos = fToken.srcpos();
    requiredProtocols = parseTopOrExprList(true, (isClass
                                                  ? kInClassDef
                                                  : kInTypeDef) );
  }

  Token result = Token() << defToken << tagToken << symToken;
  if (generics.isSet())
    result << generics;
  if (ctorParams.isSet())
    result << ctorParams;

  if (colonToken.isSet() && isaType.isSet())
    result << colonToken << isaType;

  if (whereClause.isSet())
    result << whereClause;

  if (docString.isSet())
    result << docString;

  if (!requiredProtocols.empty())
    result << wrapInBlock(bracePos, requiredProtocols);

  return result;
}


Token
FirstPass::parseSlotDef(const Token& defToken)
{
  assert(fToken == Compiler::slotToken);
  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected slot name");
    return scanUntilTopExprAndResume();
  }
  Token symToken = fToken;
  nextToken();

  Token colonToken;
  Token isaType;
  if (fToken == kColon) {
    colonToken = fToken;
    nextToken();
    SrcPos pos = fToken.srcpos();
    isaType = parseTypeSpec(true);
    if (!isaType.isSet()) {
      errorf(pos, E_MissingType, "type expression expected");
      isaType = Token(fToken.srcpos(), kSymbol, "Any");
    }
  }

  Token docString = parseOptDocString();

  Token assignToken;
  Token initExpr;
  if (fToken == kAssign) {
    assignToken = fToken;
    nextToken();

    SrcPos pos = fToken.srcpos();
    initExpr = parseExpr(false);
    if (!initExpr.isSet())
      errorf(pos, E_MissingRHExpr, "no value in var init");
  }

  Token semiToken;
  TokenVector annotations;
  if (fToken == kSemicolon) {
    semiToken = fToken;
    nextToken();

    Token delayedComma;
    while (fToken != kEOF) {
      if (fToken == kSymbol) {
        if (delayedComma.isSet())
          annotations.push_back(delayedComma);
        annotations.push_back(fToken);
        nextToken();

        if (fToken == kComma) {
          delayedComma = fToken;
          nextToken();
        }
        else
          break;
      }
      else if (!annotations.empty())
        break;
      else {
        errorf(fToken.srcpos(), E_UnexpectedToken, "expected SYMBOL");
        scanUntilTopExprAndResume();
        break;
      }
    }
  }


  Token slotDefToken = Token() << defToken << tagToken << symToken;
  if (colonToken.isSet() && isaType.isSet())
    slotDefToken << colonToken << isaType;
  if (docString.isSet())
    slotDefToken << docString;
  if (assignToken.isSet() && initExpr.isSet())
    slotDefToken << assignToken << initExpr;
  if (semiToken.isSet() && !annotations.empty())
    slotDefToken << semiToken << annotations;

  return slotDefToken;
}


Token
FirstPass::parseMeasure(const Token& defToken, bool isLocal)
{
  assert(fToken == Compiler::measureToken);
  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected measure name");
    return scanUntilTopExprAndResume();
  }
  Token symToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingUnitTag, "expected unit tag definition");
    return scanUntilTopExprAndResume();
  }
  SrcPos paranPos = fToken.srcpos();
  nextToken();

  if (!fToken.isCharOrUnitName()) {
    errorf(paranPos, E_MissingUnitTag, "empty unit parameter definition");
    return scanUntilTopExprAndResume();
  }
  Token unitToken = ( fToken == kSymbol
                      ? fToken
                      : Token(fToken.srcpos(), kSymbol, fToken.toString()) );
  nextToken();

  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_MissingParanClose, "expected ')'");
  }
  else
    nextToken();


  if (fToken != kColon) {
    errorf(fToken.srcpos(), E_MissingBaseType,
           "expected base type for measure");
    return scanUntilTopExprAndResume();
  }

  Token colonToken = fToken;
  nextToken();

  SrcPos pos = fToken.srcpos();
  Token isaType = parseTypeSpec(true);
  if (!isaType.isSet()) {
    errorf(pos, E_MissingType, "type expression expected");
    isaType = Token(fToken.srcpos(), kSymbol, "Any");
  }

  Token docString = parseOptDocString();

  Token result = Token() << defToken << tagToken << symToken
                         << (Token(paranPos, kParanOpen, kParanClose) << unitToken)
                         << colonToken << isaType;
  if (docString.isSet())
    result << docString;

  return result;
}


Token
FirstPass::parseUnit(const Token& defToken, bool isLocal)
{
  assert(fToken == Compiler::unitToken);
  Token tagToken = fToken;
  nextToken();

  if (!fToken.isCharOrUnitName()) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected unit name");
    return scanUntilTopExprAndResume();
  }
  Token newUnitToken = ( fToken == kSymbol
                         ? fToken
                         : Token(fToken.srcpos(), kSymbol, fToken.toString()) );
  nextToken();

  if (fToken != kMapTo) {
  }
  Token mapToToken = fToken;
  nextToken();

  if (!fToken.isCharOrUnitName()) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected reference unit name");
    return scanUntilTopExprAndResume();
  }
  Token refUnitToken = ( fToken == kSymbol
                         ? fToken
                         : Token(fToken.srcpos(), kSymbol, fToken.toString()) );
  nextToken();

  SrcPos signPos = fToken.srcpos();
  Token signature = parseFunctionSignature();
  if (!signature.isSet()) {
    errorf(signPos, E_MissingUnitSign, "expected function signature");
    return scanUntilTopExprAndResume();
  }

  Token docString = parseOptDocString();

  SrcPos bodyPos = fToken.srcpos();
  Token body = parseExpr(false);
  if (!body.isSet()) {
    errorf(bodyPos, E_MissingBody, "expected unit def function body");
    return Token();
  }

  Token result = Token() << defToken << tagToken
                         << newUnitToken << mapToToken << refUnitToken;
  if (docString.isSet())
    result << docString;

  result << signature.children()
         << body;

  return result;
}


namespace heather
{
  struct EnumItemParser
  {
    bool operator() (FirstPass* pass, Token& result)
    {
      if (pass->fToken != kSymbol) {
        errorf(pass->fToken.srcpos(), E_SymbolExpected,
               "expected enum item name");
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

        Token value = pass->parseExpr(false);
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
};


Token
FirstPass::parseEnumDef(const Token& defToken, bool isLocal)
{
  assert(fToken == Compiler::enumToken);
  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected enum name");
    return scanUntilTopExprAndResume();
  }
  Token enumToken = fToken;
  nextToken();

  Token colonToken;
  Token isaType;
  if (fToken == kColon) {
    colonToken = fToken;
    nextToken();
    SrcPos pos = fToken.srcpos();
    isaType = parseTypeSpec(true);
    if (!isaType.isSet()) {
      errorf(pos, E_MissingType, "type expression expected");
      isaType = Token(fToken.srcpos(), kSymbol, "Any");
    }
  }

  Token docString = parseOptDocString();

  if (fToken != kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
    return scanUntilTopExprAndResume();
  }

  Token items = Token(fToken.srcpos(), kBraceOpen, kBraceClose);
  parseSequence(EnumItemParser(),
                kBraceOpen, kBraceClose, false, E_BadEnumItemList,
                items, "enum-items");

  Token enumDefToken = Token() << defToken << tagToken << enumToken;
  if (colonToken.isSet() && isaType.isSet())
    enumDefToken << colonToken << isaType;
  if (docString.isSet())
    enumDefToken << docString;
  enumDefToken << items;

  return enumDefToken;
}


MacroType
FirstPass::dertermineMacroPatternType(const Token& macroName,
                                      const SrcPos& patternPos,
                                      const TokenVector& pattern)
{
  if (pattern.size() == 1) {
    if (pattern[0] == macroName)
      return kMacro_Any;

    errorf(pattern[1].srcpos(), E_PatternNameMismatch,
           "macro name and pattern mismatch");
    return kMacro_Invalid;
  }
  else if (pattern.size() > 1) {
    if (pattern[0] == kDefId ||
        pattern[0] == kLetId)
    {
      if (pattern[1] == macroName)
        return kMacro_Def;

      errorf(pattern[1].srcpos(), E_PatternNameMismatch,
             "macro name and pattern mismatch");
      return kMacro_Invalid;
    }
    else if (pattern[0] == kOnId) {
      if (pattern[1] == macroName)
        return kMacro_On;

      errorf(pattern[1].srcpos(), E_PatternNameMismatch,
             "macro name and pattern mismatch");
      return kMacro_Invalid;
    }

    TokenVector::const_iterator it = pattern.begin();
    if (*it != macroName) {
      errorf(pattern[1].srcpos(), E_PatternNameMismatch,
             "macro name and pattern mismatch");
      return kMacro_Invalid;
    }
    it++;

    if (*it == kParanOpen) {
      int paranCount = 0;
      SrcPos paranOpenPos = it->srcpos();

      for ( ; it != pattern.end(); it++) {
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

      errorf(paranOpenPos, E_BadMacroPattern,
             "Unbalanced paranthesis in macro pattern");
      return kMacro_Invalid;
    }
    return kMacro_Any;
  }

  errorf(patternPos, E_BadMacroPattern, "empty macro pattern");
  return kMacro_Invalid;
}


MacroType
FirstPass::determineMacroType(const Token& macroName,
                              const MacroPatternVector& patterns)
{
  MacroType lastType = kMacro_Any;

  for (MacroPatternVector::const_iterator it = patterns.begin();
       it != patterns.end();
       it++)
  {
    MacroType pType = dertermineMacroPatternType(macroName,
                                                 it->fSrcPos, it->fPattern);
    if (pType == kMacro_Invalid)
      return pType;

    if (lastType == kMacro_Any ||
        lastType == pType)
    {
      lastType = pType;
    }
    else {
      errorf(it->fSrcPos, E_MacroInconsistency,
             "Macro has inconsistent patterns");
      return kMacro_Invalid;
    }
  }

  return lastType;
}


bool
FirstPass::parseMacroComponent(TokenVector* component,
                               TokenType beginTokenType, TokenType endTokenType)
{
  SrcPos startPos = fToken.srcpos();

  int braceCount = 1;
  for ( ; ; ) {
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

  assert(0);
  return true;
}


bool
FirstPass::parseMacroPatterns(MacroPatternVector* patterns)
{
  SrcPos startPos = fToken.srcpos();

  while (true) {
    TokenType beginTokenType = kBraceOpen;
    TokenType endTokenType = kBraceClose;

    if (fToken == kEOF)
      break;

    if (fToken == kBraceOpen) {
      beginTokenType = kBraceOpen;
      endTokenType = kBraceClose;
    }
    else if (fToken == kMacroOpen) {
      beginTokenType = kMacroOpen;
      endTokenType = kMacroClose;
    }
    else {
      errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{' or '\343\200\214'");
      scanUntilTopExprAndResume();
      return false;
    }
    nextToken();

    SrcPos patternPos = fToken.srcpos();
    TokenVector pattern;
    TokenVector replacement;
    if (parseMacroComponent(&pattern, beginTokenType, endTokenType)) {
      if (fToken == kMapTo) {
        nextToken();

        if (fToken == kBraceOpen || fToken == kMacroOpen) {
          TokenType beginTT = fToken.tokenType();
          TokenType endTT = beginTT == kBraceOpen ? kBraceClose : kMacroClose;
          nextToken();
          SrcPos pos = fToken.srcpos();
          if (parseMacroComponent(&replacement, beginTT, endTT)) {
            patterns->push_back(MacroPattern(patternPos,
                                             pattern, replacement));
          }
          else {
            errorf(pos, E_BadMacroReplcment, "bad macro replacement");
            scanUntilTopExprAndResume();
            return false;
          }
        }
        else {
          errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
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

    if (fToken == kBraceOpen || fToken == kMacroOpen)
      continue;
    else if (fToken != kBraceClose) {
      errorf(fToken.srcpos(), E_UnexpectedToken, "expected '{', '\343\200\214' or '}'");
      scanUntilTopExprAndResume();
      return false;
    }
    else if (fToken == kBraceClose)
      break;
  }

  if (fToken == kBraceClose) {
    nextToken();
  }
  else {
    errorf(fToken.srcpos(), E_UnexpectedToken, "expected '}'");

    if (startPos != fToken.srcpos())
      errorf(startPos, E_MissingBraceClose, "beginning '{' was here");
    scanUntilTopExprAndResume();
    return false;
  }

  return true;
}


Token
FirstPass::parseMacroDef(const Token& defToken)
{
  assert(fToken == Compiler::macroToken);
  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected macro name");
    return scanUntilTopExprAndResume();
  }
  Token macroNameToken = fToken;
  nextToken();

  Token docString = parseOptDocString();

  if (fToken != kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
    return scanUntilTopExprAndResume();
  }

  SrcPos bracePos = fToken.srcpos();
  nextToken();

  MacroPatternVector patterns;
  if (parseMacroPatterns(&patterns)) {
    if (fEvaluateExprs) {
      MacroType mType = determineMacroType(macroNameToken, patterns);

      String fullMacroName = qualifyId(currentModuleName(),
                                       macroNameToken.idValue());

      if (fScope->checkForRedefinition(defToken.srcpos(),
                                       Scope::kNormal, fullMacroName))
        return Token();


      Ptr<SyntaxTable> synTable = SyntaxTable::compile(String(""), patterns);
      fScope->registerMacro(defToken.srcpos(),
                            fullMacroName,
                            new Macro(synTable, mType));

      if (Properties::isTraceMacro()) {
        fprintf(stderr, "%s\n", (const char*)StrHelper(synTable->toString()));
      }
    }
  }

  // always ignore macros
  return Token();
}


Token
FirstPass::parseLinkageType()
{
  assert(fToken == kExternId);

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

  return Token() << externToken
                 << ( Token(paranPos, kParanOpen, kParanClose)
                      << linkage );
}


TokenVector
FirstPass::parseDef(bool isLocal, ScopeType scope)
{
  Token defToken = fToken;
  nextToken();

  Token linkage;
  if (fToken == kExternId) {
    linkage = parseLinkageType();
  }

  switch (scope) {
  case kInTypeDef:
    if (fToken == Compiler::genericToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for generic method ignored");

      return parseGenericFunctionDef(defToken, isLocal).toTokenVector();
    }
    else {
      error(fToken.srcpos(), E_UnexpDefInClass,
            ( String("unexpected definition type '") + fToken.toString()
              + "'in class") );
      return scanUntilTopExprAndResume().toTokenVector();
    }
    break;

  case kInClassDef:
    if (fToken == Compiler::slotToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for slot definition ignored");
      return parseSlotDef(defToken).toTokenVector();
    }
    else if (fToken == Compiler::genericToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for generic method ignored");
      return parseGenericFunctionDef(defToken, isLocal).toTokenVector();
    }
    else {
      error(fToken.srcpos(), E_UnexpDefInClass,
            ( String("unexpected definition type '") + fToken.toString()
              + "'in class") );
      return scanUntilTopExprAndResume().toTokenVector();
    }
    break;

  case kNonScopedDef:
    if (fToken == Compiler::typeToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for type definition ignored");
      return parseTypeDef(defToken, false, isLocal).toTokenVector();
    }
    else if (fToken == Compiler::classToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for class definition ignored");
      return parseTypeDef(defToken, true, isLocal).toTokenVector();
    }
    else if (fToken == Compiler::aliasToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for alias definition ignored");
      return parseAliasDef(defToken, isLocal).toTokenVector();
    }
    else if (fToken == Compiler::slotToken) {
      errorf(fToken.srcpos(), E_SlotNotInClassDef,
             "slot definitions only allowed in class defs.");
      return scanUntilTopExprAndResume().toTokenVector();
    }
    else if (fToken == Compiler::enumToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for enum definition ignored");
      return parseEnumDef(defToken, isLocal).toTokenVector();
    }
    else if (fToken == Compiler::measureToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for measure definition ignored");
      return parseMeasure(defToken, isLocal).toTokenVector();
    }
    else if (fToken == Compiler::unitToken) {
      if (linkage.isSet())
        errorf(linkage.srcpos(), E_UnexpLinkage,
               "Unsupported linkage for unit definition ignored");
      return parseUnit(defToken, isLocal).toTokenVector();
    }
    else if (fToken == Compiler::constToken ||
             fToken == Compiler::fluidToken ||
             fToken == Compiler::configToken) {
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
    else if (fToken == kSymbol) {
      return parseFunctionOrVarDef(defToken, isLocal, linkage);
    }
    else {
      errorf(fToken.srcpos(), E_DefInitUnexpToken,
             "Bad init value: %s", (const char*)StrHelper(fToken.toString()));
      return scanUntilTopExprAndResume().toTokenVector();
    }
    break;
  }

  return TokenVector();
}


TokenVector
FirstPass::parseTop(ScopeType scope)
{
  if (fToken == kModuleId) {
    return parseModule().toTokenVector();
  }
  else if (fToken == kExportId) {
    return parseExport().toTokenVector();
  }
  else if (fToken == kImportId) {
    return parseImport().toTokenVector();
  }
  else if (fToken == kDefId) {
    return parseDef(false, scope);
  }
  else if (fToken == kWhenId) {
    return parseWhen(true, scope).toTokenVector();
  }
  else if (fToken == kExtendId) {
    return parseExtend(scope).toTokenVector();
  }
  else if (fToken == kExternId) {
    return parseExtern();
  }
  else if (fToken == kOnId) {
    if (scope == kInClassDef)
      return parseOn(kInClassDef).toTokenVector();
    else {
      errorf(fToken.srcpos(), E_UnexpectedToken, "Unexpected 'on' expr");
      return scanUntilTopExprAndResume().toTokenVector();
    }
  }
  else {
    errorf(fToken.srcpos(), E_UnexpectedToken,
           "Unexpected top expression: %s",
           (const char*)StrHelper(fToken.toString()));
    return scanUntilTopExprAndResume().toTokenVector();
  }

  return TokenVector();
}


Token
FirstPass::parse()
{
  Token seq;

  {
    // let the complete parse run in its own scope to force an explicit export
    // run
    ScopeHelper scopeHelper(fScope, true, false, kScopeL_CompileUnit);

    nextToken();
    while (fToken != kEOF) {
      TokenVector n = parseTop(kNonScopedDef);
      if (!n.empty())
        seq << n;
    }
  }

  return seq;
}


//------------------------------------------------------------------------------

bool
FirstPass::replaceSangHashIds(TokenVector* result, const TokenVector& source)
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
                token = Token(token.srcpos(),
                              token.idValue() + source[idx + 2].idValue());
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


const TokenVector&
FirstPass::findReplaceToken(const Token& token,
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


bool
FirstPass::replaceMatchBindings(TokenVector* result,
                                const TokenVector& templ,
                                const NamedReplacementMap& bindings)
{
  TokenVector replacement;
  for (TokenVector::const_iterator it = templ.begin();
       it != templ.end();
       it++)
  {
    Token token = *it;

    switch (token.type()) {
    case kPunct:
    case kLit:
      replacement.push_back(token);
      break;

    case kId:
      if (token == kMacroParam ||
          token == kMacroParamAsStr) {
        bool found = false;
        const TokenVector& replTokens = findReplaceToken(token, bindings, found);

        if (found) {
          if (replTokens.size() == 1) {
            if (token == kMacroParamAsStr) {
              replacement.push_back(Token(replTokens[0].srcpos(),
                                          kString,
                                          replTokens[0].toString()));
            }
            else
              replacement.push_back(replTokens[0]);
          }
          else if (replTokens.size() > 1) {
            replacement.insert(replacement.end(),
                               replTokens.begin(), replTokens.end());
          }
        }
        else
          errorf(token.srcpos(), E_UnknownMacroParam,
                 "Undefined macro parameter %s",
                 (const char*)StrHelper(token.toString()));
      }
      else
        replacement.push_back(token);
      break;

    case kSeq:
      {
        TokenVector temp2;
        if (!replaceMatchBindings(&temp2, token.children(), bindings))
          return false;

        replacement.push_back(Token() << temp2);
      }
      break;

    case kNested:
      {
        TokenVector temp2;
        if (!replaceMatchBindings(&temp2, token.children(), bindings)) {
          return false;
        }

        replacement.push_back(
          Token(token.srcpos(), token.leftToken(), token.rightToken()) << temp2);
      }
      break;
    }
  }

  return replaceSangHashIds(result, replacement);
}


//------------------------------------------------------------------------------

namespace heather {

  struct ParameterSyntaxMatcher : public RefCountable
  {
    virtual ~ParameterSyntaxMatcher() { }

    virtual bool match(FirstPass* pass,
                       const String& paramName,
                       NamedReplacementMap* bindings,
                       SyntaxTreeNode* followSet)
    {
      assert(0);
      return false;
    }
  };


  struct ExprParamSyntaxMatcher : public ParameterSyntaxMatcher
  {
    virtual bool match(FirstPass* pass,
                       const String& paramName,
                       NamedReplacementMap* bindings,
                       SyntaxTreeNode* followSet)
    {
      SrcPos pos = pass->fToken.srcpos();
      Token expr = pass->parseExpr(false);
      if (!expr.isSet()) {
        errorf(pos, E_MacroParamMismatch,
               "Macro parameter %s requires expression",
               (const char*)StrHelper(paramName));
        return false;
      }

      TokenVector tokens;
      tokens.push_back(expr);
      bindings->insert(std::make_pair(paramName, tokens));
      return true;
    }
  };


  struct NameParamSyntaxMatcher : public ParameterSyntaxMatcher
  {
    virtual bool match(FirstPass* pass,
                       const String& paramName,
                       NamedReplacementMap* bindings,
                       SyntaxTreeNode* followSet)
    {
      if (pass->fToken == kSymbol) {
        TokenVector tokens;
        tokens.push_back(pass->fToken);
        bindings->insert(std::make_pair(paramName, tokens));
        pass->nextToken();
        return true;
      }

      errorf(pass->fToken.srcpos(), E_MacroParamMismatch,
             "Macro parameter %s requires identifier",
             (const char*)StrHelper(paramName));
      return false;
    }
  };


  struct AnyParamParamSyntaxMatcher : public ParameterSyntaxMatcher
  {
    virtual bool match(FirstPass* pass,
                       const String& paramName,
                       NamedReplacementMap* bindings,
                       SyntaxTreeNode* followSet)
    {
      SrcPos pos = pass->fToken.srcpos();
      FirstPass::ParamType expected = FirstPass::kPositional;
      Token param = pass->parseParameter(&expected, false);

      if (!param.isSet()) {
        errorf(pos, E_MacroParamMismatch,
               "Macro parameter %s requires parameter",
               (const char*)StrHelper(paramName));
        return false;
      }

      TokenVector tokens;
      tokens.push_back(param);
      bindings->insert(std::make_pair(paramName, tokens));
      return true;
    }
  };


  struct SpecParamParamSyntaxMatcher : public ParameterSyntaxMatcher
  {
    FirstPass::ParamType fReqType;

    SpecParamParamSyntaxMatcher(FirstPass::ParamType reqType)
      : fReqType(reqType)
    { }

    virtual bool match(FirstPass* pass,
                       const String& paramName,
                       NamedReplacementMap* bindings,
                       SyntaxTreeNode* followSet)
    {
      SrcPos pos = pass->fToken.srcpos();
      FirstPass::ParamType expected = FirstPass::kPositional;
      Token param = pass->parseParameter(&expected, false);

      if (expected != fReqType || !param.isSet()) {
        errorf(pos, E_MacroParamMismatch,
               "Macro parameter %s requires positional parameter",
               (const char*)StrHelper(paramName));
        return false;
      }

      TokenVector tokens;
      tokens.push_back(param);
      bindings->insert(std::make_pair(paramName, tokens));
      return true;
    }
  };


  struct ParamListParamSyntax : public ParameterSyntaxMatcher
  {
    virtual bool match(FirstPass* pass,
                       const String& paramName,
                       NamedReplacementMap* bindings,
                       SyntaxTreeNode* followSet)
    {
      SrcPos pos = pass->fToken.srcpos();
      TokenVector params;

      // TODO: extract the set of possible end token types from followSet and
      // pass it to parseFunctionsParamsFull.
      TokenType endTokenType = kParanClose;
      if (!pass->parseFunctionsParamsFull(&params,
                                          kParanOpen, endTokenType,
                                          true /* autoCompleteType */,
                                          true /* acceptEmptyList */,
                                          false /* skipFirst */,
                                          false /* don't eat last */)) {
        return false;
      }

      bindings->insert(std::make_pair(paramName, params));
      return true;
    }
  };
};


typedef std::map<MacroParamType, Ptr<ParameterSyntaxMatcher> > ParamFuncMap;

bool
FirstPass::matchParameter(const Token& macroParam,
                          NamedReplacementMap* bindings,
                          SyntaxTreeNode* followSet)
{
  static ParamFuncMap paramsMap;
  if (paramsMap.empty()) {
    paramsMap.insert(std::make_pair(kMacro_expr,
                                    new ExprParamSyntaxMatcher));
    paramsMap.insert(std::make_pair(kMacro_name,
                                    new NameParamSyntaxMatcher));
    paramsMap.insert(std::make_pair(kMacro_param,
                                    new AnyParamParamSyntaxMatcher));
    paramsMap.insert(std::make_pair(kMacro_paramlist,
                                    new ParamListParamSyntax));
    paramsMap.insert(std::make_pair(kMacro_posParam,
                                    new SpecParamParamSyntaxMatcher(kPositional)));
    paramsMap.insert(std::make_pair(kMacro_namedParam,
                                    new SpecParamParamSyntaxMatcher(kNamed)));
    paramsMap.insert(std::make_pair(kMacro_restParam,
                                    new SpecParamParamSyntaxMatcher(kRest)));
  }

  String paramName;
  MacroParamType macroPrmType = macroParamType(macroParam, &paramName);

  ParamFuncMap::iterator it = paramsMap.find(macroPrmType);
  if (it != paramsMap.end()) {
    return it->second->match(this, paramName, bindings, followSet);
  }
  else {
    errorf(macroParam.srcpos(), E_MacroParamType,
           "Unknown macro parameter type: %s",
           (const char*)StrHelper(macroParam.toString()));
    return false;
  }
}


bool
FirstPass::matchSyntax(TokenVector* result, SyntaxTable* syntaxTable)
{

  SyntaxTreeNode* node = syntaxTable->rootNode();
  assert(node != NULL);

  NamedReplacementMap bindings;

  for ( ; ; ) {
    SyntaxTreeNode* followSet = node->findNode(fToken);
    if (followSet != NULL) {
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
      if (followSet != NULL) {
        if (matchParameter(macroParam, &bindings, followSet)) {
          node = followSet;
          continue;
        }
      }

      return false;
    }
  }

  return false;
}


bool
FirstPass::parseDoMatchSyntaxDef(TokenVector* result,
                                 const Token& expr, SyntaxTable* syntaxTable,
                                 bool isLocal)
{
  unreadToken(expr);

  if (isLocal)
    fToken = Token(expr.srcpos(), kLetId);
  else
    fToken = Token(expr.srcpos(), kDefId);

  return matchSyntax(result, syntaxTable);
}


bool
FirstPass::parseDoMatchSyntaxOn(TokenVector* result,
                                const Token& expr, SyntaxTable* syntaxTable,
                                bool isLocal)
{
  unreadToken(expr);
  fToken = Token(expr.srcpos(), kOnId);
  return matchSyntax(result, syntaxTable);
}


bool
FirstPass::parseDoMatchSyntaxFunc(TokenVector* result,
                                  const Token& expr,
                                  const TokenVector& args,
                                  SyntaxTable* syntaxTable,
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
      assert(args.size() == 1);
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
    for (std::list<Token>::iterator it = res.begin();
         it != res.end();
         it++)
    {
      unreadToken(*it);
    }

    unreadToken(Token(oldCurrentToken.srcpos(), kParanOpen));
  }

  fToken = expr;
  return matchSyntax(result, syntaxTable);
}


bool
FirstPass::parseExprStream(TokenVector* result, bool isTopLevel,
                           ScopeType scopeType)
{
  for ( ; ; ) {
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
      exprs = parseTop(scopeType);
    else
      exprs = parseExpr(true).toTokenVector();

    if (!exprs.empty())
      result->insert(result->end(), exprs.begin(), exprs.end());
    else {
      errorf(pos, E_UnexpectedToken,
             "unexpected token while scanning macro replacement: %s",
             (const char*)StrHelper(fToken.toString()));
      return false;
    }
  }

  return true;
}


Token
FirstPass::parseMakeMacroCall(const Token& expr, const TokenVector& args,
                              const Macro* macro,
                              bool shouldParseParams,
                              bool isLocal,
                              ScopeType scopeType)
{
  assert(expr == kSymbol);
  assert(!isQualified(expr.idValue()));

  Ptr<SyntaxTable> syntaxTable = macro->syntaxTable();

  TokenVector filtered;
  switch (macro->type()) {
  case kMacro_Invalid:
    // assert(0);
    return Token();

  case kMacro_Any:
    return Token();

  case kMacro_Def:
    parseDoMatchSyntaxDef(&filtered, expr, syntaxTable, isLocal);
    break;

  case kMacro_On:
    parseDoMatchSyntaxOn(&filtered, expr, syntaxTable, isLocal);
    break;

  case kMacro_Stmt:
  case kMacro_Function:
    parseDoMatchSyntaxFunc(&filtered, expr, args, syntaxTable, shouldParseParams);
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

    Token retval;
    Ptr<InternalTokenPort> tempPort = new InternalTokenPort(follows);

    {
      Compiler::PortStackHelper portStack(fCompiler, tempPort);

      TokenVector result;
      if (parseExprStream(&result, !isLocal, scopeType)) {
        if (result.size() == 1)
          retval = result[0];
        else if (result.size() > 1)
          retval = Token() << result;
      }
      else
        return Token();
    }

    fToken = lastCurrentToken;

    return retval;
  }

  return Token();
}
