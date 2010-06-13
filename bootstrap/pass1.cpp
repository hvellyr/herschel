/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "errcodes.h"
#include "log.h"
#include "macro.h"
#include "parser.h"
#include "pass1.h"
#include "properties.h"
#include "strbuf.h"
#include "tokeneval.h"
#include "tokenizer.h"
#include "valuesaver.h"


//----------------------------------------------------------------------------

using namespace heather;


//----------------------------------------------------------------------------

FirstPass::FirstPass(Parser* parser, const Token& currentToken)
  : fParser(parser),
    fToken(currentToken),
    fEvaluateExprs(true)
{ }


Token
FirstPass::nextToken()
{
  fToken = fParser->nextToken();
  return fToken;
}


void
FirstPass::unreadToken(const Token& token)
{
  fParser->unreadToken(token);
}


Token
FirstPass::scanUntilTopExprAndResume()
{
  while (fToken != kEOF &&
         fToken != kDefId &&
         fToken != kModuleId &&
         fToken != kInterfaceId &&
         fToken != kExportId &&
         fToken != kImportId &&
         fToken != kWhenId &&
         fToken != kExtendId)
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
                         bool skipFirst)
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
               << Token(SrcPos(), endToken).toString() << "' or ','").toString());
    }
  }

  if (fToken == endToken) {
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

String
FirstPass::qualifiedIdForLookup(const String& id) const
{
  // TODO
  return id;
}


//----------------------------------------------------------------------------

struct heather::ModuleParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    Token n = pass->parseTop(FirstPass::kNonScopedDef);
    if (n.isSet())
      result << n;
    else {
      errorf(pass->fToken.srcpos(), E_UnexpectedToken,
             "Parsing module definitions found unexpected token: %s",
             (const char*)StrHelper(pass->fToken.toString()));
      return false;
    }

    return true;
  }
};

Token
FirstPass::parseModule(bool isModule)
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
      parseSequence(ModuleParser(),
                    kBraceOpen, kBraceClose, false, E_MissingBraceClose,
                    defines,
                    "module-body");
      modExpr << defines;
    }
  }

  return modExpr;
}


struct heather::ExportParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    if (pass->fToken.isSymbol()) {
      result << pass->fToken;
      pass->nextToken();
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


Token
FirstPass::parseExport()
{
  Token expr;
  expr << fToken;

  nextToken();

  if (fToken == kSymbol) {
    if (fToken == Parser::publicToken ||
        fToken == Parser::innerToken ||
        fToken == Parser::outerToken) {
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

  if (fToken == kAs) {
    Token asToken = fToken;
    nextToken();

    if (fToken != Parser::finalToken) {
      errorf(fToken.srcpos(), E_UnexpectedToken, "expected 'final'");
      return expr;
    }

    expr << asToken << fToken;
    nextToken();
  }

  return ignore ? Token() : expr;
}


struct heather::ImportRenameParser
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
      bool isPublic = false;
      String srcName = importFile.stringValue();
      Ptr<Port<Char> > file = fParser->lookupFileAndOpen(srcName, isPublic);
      Token imported = fParser->importFile(file, srcName);

      file->close();

      return imported;
    }
    catch (const Exception& e) {
      error(importFile.srcpos(), E_UnknownInputFile, e.message());
    }

    return Token();
  }

  return expr;
}


struct heather::TypeParser
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


Token
FirstPass::parseSimpleType(const Token& baseToken, bool nextIsParsedYet)
{
  assert(baseToken == kSymbol);

  Token typeName = baseToken;
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
  if (fToken == kBracketOpen) {
    Token arrayType = Token(fToken.srcpos(), kBracketOpen, kBracketClose);
    nextToken();

    Token idxExpr;
    if (fToken != kBracketClose) {
      SrcPos idxPos = fToken.srcpos();
      idxExpr = parseExpr();
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

    return Token() << baseType << arrayType;
  }

  return baseType;
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

    Token constExpr = parseExpr();
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
  if (fToken == kSymbol) {
    return ( onlyNestedConstraints
             ? parseArrayExtend(parseSimpleType(fToken))
             : parseConstraintExtend(parseArrayExtend(parseSimpleType(fToken))) );
  }
  else if (fToken == kFUNCTIONId) {
    return parseArrayExtend(parseFunctionType());
  }
  else if (fToken == kQuote) {
    // no constraints for generics
    return parseArrayExtend(parseQuotedType());
  }
  else if (fToken == kUnionOpen) {
    // no constraints for union types
    return parseArrayExtend(parseUnionType());
  }
  else if (fToken == kParanOpen) {
    // no constraints for sequence types
    return parseArrayExtend(parseGroupType());
  }

  return Token();
}


struct heather::LiteralVectorParser
{
  LiteralVectorParser()
    : fIsDict(false),
      fIsFirst(true)
  { }

  bool operator() (FirstPass* pass, Token& result)
  {
    Token expr = pass->parseExpr();

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

        Token toValue = pass->parseExpr();
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


struct heather::LiteralArrayParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    Token n = pass->parseExpr();
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

  Token test = parseExpr();
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_ParamMissParanClose,
           "Syntax error, missing ')'");
  }
  else
    nextToken();

  Token consequent = parseExpr();

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

    Token alternate = parseExpr();

    result << elseToken << alternate;
  }

  return result;
}


struct heather::ParseFuncParamsParser
{
  enum ParamType {
    kPositional,
    kNamed,
    kRest
  };

  ParamType fExpected;
  bool      fAutoCompleteTypes;

  ParseFuncParamsParser(bool autoCompleteTypes)
    : fExpected(kPositional),
      fAutoCompleteTypes(autoCompleteTypes)
  { }

  bool operator() (FirstPass* pass, Token& result)
  {
    Token paramSeq;
    ParamType paramType = kPositional;

    if (pass->fToken == kKeyarg) {
      paramSeq << pass->fToken;
      pass->nextToken();
      paramType = kNamed;
    }

    if (pass->fToken != kSymbol) {
      errorf(pass->fToken.srcpos(), E_SymbolExpected,
             "parameter name expected");
      pass->scanUntilNextParameter();
      return true;
    }
    else {
      paramSeq << pass->fToken;
      pass->nextToken();

      Token typeIntroToken = pass->fToken;
      if (pass->fToken == kColon ||
          pass->fToken == kAt)
      {
        pass->nextToken();

        SrcPos pos = pass->fToken.srcpos();
        Token type = pass->parseTypeSpec(true);
        if (!type.isSet()) {
          errorf(pos, E_MissingType,
                 "type expression expected");
          if (fAutoCompleteTypes)
            paramSeq << typeIntroToken << Token(pos, kSymbol, "Any");
        }
        else
          paramSeq << typeIntroToken << type;
      }
      else if (fAutoCompleteTypes)
        paramSeq << Token(typeIntroToken.srcpos(), kColon)
                 << Token(typeIntroToken.srcpos(), kSymbol, "Any");

      if (pass->fToken == kAssign) {
        Token assignToken = pass->fToken;
        pass->nextToken();

        SrcPos pos = pass->fToken.srcpos();
        Token initExpr = pass->parseExpr();
        if (!initExpr.isSet())
          errorf(pos, E_MissingRHExpr, "no value in keyed argument");
        else {
          paramSeq << assignToken << initExpr;
          paramType = kNamed;
        }
      }
      else if (pass->fToken == kEllipsis) {
        Token restToken = pass->fToken;
        pass->nextToken();

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

    if (fExpected == kPositional) {
      fExpected = paramType;
      result << paramSeq.unwrapSingleton();
    }
    else if (fExpected == kNamed) {
      if (paramType == kPositional)
        errorf(paramSeq.srcpos(), E_ParamOrder,
               "out of order (positional) parameter");
      else {
        fExpected = paramType;
        result << paramSeq.unwrapSingleton();
      }
    }
    else if (fExpected == kRest) {
      errorf(paramSeq.srcpos(), E_ParamOrder,
             "no parameter after rest parameter");
    }

    return true;
  }
};


bool
FirstPass::parseFunctionsParams(TokenVector* exprlist, bool autoCompleteType,
                                bool exceptEmptyList)
{
  Token params;
  parseSequence(ParseFuncParamsParser(autoCompleteType),
                kParanOpen, kParanClose, true, E_BadParameterList,
                params,
                "func-params");

  if (params.isSet() || exceptEmptyList) {
    *exprlist = params.children();
    return true;
  }

  return false;
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

  String macroId = qualifiedIdForLookup(keyToken.idValue());
  Ptr<Macro> macro = fParser->macroRegistry()->lookupMacro(macroId);
  Token macroName = Token(keyToken.srcpos(), keyToken.idValue());

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

    if (keyToken != Parser::syncToken &&
        keyToken != Parser::initToken &&
        keyToken != Parser::deleteToken &&
        keyToken != Parser::exitToken &&
        keyToken != Parser::signalToken) {
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
      Token body = parseExpr();

      if (!ignoreStmt && body.isSet())
        return Token() << tagToken << keyToken
                       << ( Token(paranPos, kParanOpen, kParanClose)
                            << params )
                       << body;
      else
        return Token() << Token(tagToken.srcpos(), "unspecified");
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

    Token body = parseExpr();
    if (body.isSet())
      return Token() << funcToken
                     << ( Token(paranPos, kParanOpen, kParanClose)
                          << params )
                     << colonToken << returnType
                     << body;
  }
  return Token();
}


struct heather::FuncallArgsParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    if (pass->fToken.isKeyArg()) {
      Token key = pass->fToken;
      pass->nextToken();

      Token val = pass->parseExpr();
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
      Token val = pass->parseExpr();
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
    String macroId  = qualifiedIdForLookup(expr.idValue());
    Ptr<Macro> macro = fParser->macroRegistry()->lookupMacro(macroId);
    Token macroName = Token(expr.srcpos(), expr.idValue());

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

  Token idx = parseExpr();

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
  Token expr = parseExpr();
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
    Token expr;

    if (fToken == kDefId ||
        fToken == kExtendId ||
        fToken == kExportId ||
        fToken == kImportId ||
        fToken == kInterfaceId ||
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
      expr = parseDef(isLocal, kNonScopedDef);
    }
    else if (fToken == kBraceClose) {
      return true;
    }
    else if (fToken == kEOF) {
      return true;
    }
    else if (fToken == kWhenId) {
      expr = parseWhen(!isLocal, kNonScopedDef);
    }
    else {
      Token before = fToken;
      SrcPos startPos = fToken.srcpos();
      expr = parseExpr();
      if (!expr.isSet()) {
        error(startPos, E_UnexpectedToken,
               String("unexpected token while scanning block: ") + before);
        return false;
      }
    }

    if (expr.isSet())
      result->push_back(expr);
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

  return Token(bosp, kBraceOpen, kBraceClose) << exprlist;
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


class heather::BasePatternParser
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

    Token body = pass->parseExpr();
    if (body.isSet()) {
      if (mapToReq)
        result.push_back(mapToToken);
      result.push_back(body);
    }
    return result;
  }
};


struct heather::SelectPatternParser : public BasePatternParser
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

        Token test = pass->parseExpr();
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


struct heather::MatchPatternParser : public BasePatternParser
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

    if (pass->fToken != kSymbol) {
      errorf(pass->fToken.srcpos(), E_SymbolExpected,
             "variable name expected");
      pass->scanUntilBrace();
      return false;
    }
    Token varToken = pass->fToken;
    pass->nextToken();

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
    if (varToken.isSet() && colonToken.isSet() &&
        matchType.isSet() && !consq.empty())
      result << ( Token()
                  << pipeToken
                  << ( Token() << varToken << colonToken << matchType )
                  << consq );

    return true;
  }
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


struct heather::ForClauseParser
{
  bool parseInCollClause(FirstPass* pass, Token& result,
                         const Token& symToken,
                         const Token& colonToken,
                         const Token& type)
  {
    assert(pass->fToken == kIn);
    Token inToken = pass->fToken;
    pass->nextToken();

    Token collToken = pass->parseExpr();
    if (!collToken.isSet()) {
      error(pass->fToken.srcpos(), E_MissingRHExpr,
            String("unexpected token: ") + pass->fToken.toString());
      pass->scanUntilNextParameter();
      return true;
    }

    Token subexpr = Token() << symToken;
    if (colonToken.isSet() && type.isSet())
      subexpr << colonToken << type;
    subexpr << inToken << collToken;

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

    Token iterator = pass->parseExpr();
    if (!iterator.isSet()) {
      error(pass->fToken.srcpos(), E_MissingRHExpr,
            String("unexpected token: ") + pass->fToken.toString());
      pass->scanUntilNextParameter();
      return true;
    }

    Token subexpr = Token() << symToken;
    if (colonToken.isSet() && type.isSet())
      subexpr << colonToken << type;
    subexpr << assignToken << iterator;

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
            Token expr = pass->parseExprRec(first, op1, op1Srcpos);
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
              String("unexpected token in for clause (1): ") + pass->fToken.toString());
        pass->scanUntilNextParameter();
      }
    }
    else {
      Token expr = pass->parseExpr();
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

  Token body = parseExpr();

  Token elseToken;
  Token alternate;
  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();

    alternate = parseExpr();
  }

  if (body.isSet()) {
    Token result = Token() << forToken
                           << args
                           << body;
    if (elseToken.isSet() && alternate.isSet())
      result << elseToken << alternate;
    return result;
  }
  return Token() << Token(forToken.srcpos(), "unspecified");
}


Token
FirstPass::parseUnitNumber(const Token& token)
{
  Token number = token;
  nextToken();
  if (fToken == kBackQuote) {
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
    // rename the function call in expr1 to name! and append expr2 as last
    // parameter to expr1's parameter list
    return Token() << Token(expr1[0].srcpos(), expr1[0].idValue() + "!")
                   << ( Token(op1Srcpos, kParanOpen, kParanClose)
                        << expr1.children()
                        << Token(op1Srcpos, kComma)
                        << expr2 );
  }

  return Token() << expr1
                 << Token(op1Srcpos, kAssign)
                 << expr2;
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
FirstPass::parseExprRec(const Token& expr1,
                        OperatorType op1, const SrcPos& op1Srcpos)
{
  if (op1 == kOpInvalid)
    return expr1;

  nextToken();
  SrcPos before2ndPos = fToken.srcpos();
  Token expr2 = parseAtomicExpr();
  OperatorType op2 = tokenTypeToOperator(fToken.tokenType());
  SrcPos op2Srcpos = fToken.srcpos();

  if (!expr2.isSet()) {
    errorf(before2ndPos, E_MissingRHExpr, "no right hand expression");
    return expr1;
  }

  if (op2 == kOpInvalid) {
    if (op1 == kOpAssign)
      return makeAssignToken(expr1, expr2, op1Srcpos);
    else
      return Token() << expr1
                     << Token(op1Srcpos, operatorToTokenType(op1))
                     << expr2;
  }
  else {
    if (!isRightOperator(op1) && isOpWeightAbove(op1, op2))
      return parseExprRec(makeBinaryToken(expr1, op1, expr2, op1Srcpos),
                          op2,
                          op2Srcpos);
    else
      return makeBinaryToken(expr1, op1, parseExprRec(expr2, op2, op2Srcpos),
                             op1Srcpos);
  }
}


Token
FirstPass::parseExpr()
{
  Token expr1 = parseAtomicExpr();
  if (expr1.isSet()) {
    OperatorType op1 = tokenTypeToOperator(fToken.tokenType());
    SrcPos op1Srcpos = fToken.srcpos();

    if (op1 != kOpInvalid)
      return parseExprRec(expr1, op1, op1Srcpos);
  }
  return expr1;
}


void
FirstPass::parseTopExprUntilBrace(TokenVector* result, ScopeType scope)
{
  while (fToken != kBraceClose) {
    if (fToken == kEOF)
      break;
    Token topexpr = parseTop(scope);
    result->push_back(topexpr);
  }

  if (fToken == kBraceClose)
    nextToken();
}


Token
FirstPass::parseTopOrExprList(bool isTopLevel, ScopeType scope)
{
  if (isTopLevel) {
    if (fToken == kBraceOpen) {
      SrcPos bracePos = fToken.srcpos();
      nextToken();

      TokenVector exprs;
      parseTopExprUntilBrace(&exprs, scope);

      return Token(bracePos, kBraceOpen, kBraceClose) << exprs;
    }
    return parseTop(scope);
  }
  return parseExpr();
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
    parseExpr();
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
    if (fToken == Parser::ignoreToken) {
      inclConsequent = false;
    }
    else if (fToken == Parser::includeToken) {
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

    Token test = parseExpr();
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
      TokenEvalContext ctx(fParser->configVarRegistry());
      Token p = ctx.evalToken(test);
      if (p.isBool()) {
        inclConsequent = p.boolValue();
        inclAlternate = !p.boolValue();
      }
      else {
        fprintf(stderr, "ERROR: when-expression did not evaluate to boolean. "
                "Treat it as false\n");
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
    consequent = parseTopOrExprList(isTopLevel, scope);
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
      alternate = parseTopOrExprList(isTopLevel, scope);
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

  if (fToken == kBraceOpen) {
    Token code = parseTopOrExprList(true, scope);
    if (code.isSet())
      return Token() << extendToken << moduleToken
                     << modNameToken << code;
  }

  return Token();
}


Token
FirstPass::parseVarDef(const Token& defToken, const Token& tagToken, bool isLocal)
{
  Token keepTagToken = tagToken;

  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "Missing name");
    return scanUntilTopExprAndResume();
  }
  Token symbolToken = fToken;

  nextToken();

  return parseVarDef2(defToken, keepTagToken, symbolToken, isLocal);
}


Token
FirstPass::evaluateConfigExpr(const Token& initExpr)
{
  TokenEvalContext ctx(fParser->configVarRegistry());
  return ctx.evalToken(initExpr);
}


Token
FirstPass::parseVarDef2(const Token& defToken, const Token& tagToken,
                        const Token& symbolToken,
                        bool isLocal)
{
  Token type;
  Token colonToken, assignToken;

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

  Token docString = parseOptDocString();

  Token initExpr;
  if (fToken == kAssign) {
    assignToken = fToken;
    nextToken();

    SrcPos pos = fToken.srcpos();
    initExpr = parseExpr();
    if (!initExpr.isSet())
      errorf(pos, E_MissingRHExpr, "no value in var init");
  }

  Token vardefExpr;
  vardefExpr << defToken;
  if (tagToken.isSet())
    vardefExpr << tagToken;

  vardefExpr << symbolToken;

  if (type.isSet())
    vardefExpr << colonToken << type;

  if (tagToken == Parser::configToken) {
    if (fEvaluateExprs) {
      if (!initExpr.isSet()) {
        error(symbolToken.srcpos(), E_DefNoInitValue,
              ( String("Config variable '") + symbolToken +
                "' without default value") );

        // if no default value is given assume ''
        initExpr = Token(symbolToken.srcpos(), kString, "");
        if (!assignToken.isSet())
          assignToken = Token(symbolToken.srcpos(), kAssign);
      }
      fParser->configVarRegistry()->registerValue(symbolToken.idValue(),
                                                  evaluateConfigExpr(initExpr));
      // even if we have to evaluate the config var expression, we have to
      // keep the constructed expr since config-vars can be used like
      // normal const-vars by code
    }
  }

  if (docString.isSet())
    vardefExpr << docString;

  if (initExpr.isSet())
    vardefExpr << assignToken << initExpr;

  return vardefExpr;
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
    fParser->charRegistry()->registerValue(charNameToken.idValue(),
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

    Token constrExpr = parseExpr();
    if (constrExpr.isSet()) {
      if (delayedCommaToken.isSet()) {
        constraints.push_back(delayedCommaToken);
        delayedCommaToken = Token();
      }
      constraints.push_back(constrExpr);
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
                            const Token& symToken, bool isLocal)
{
  assert(fToken == kParanOpen);
  Token paranOpenToken = fToken;

  Token result;
  result << defToken;
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
        body = parseExpr();
        if (!body.isSet()) {
          errorf(bodyPos, E_MissingBody, "expected function body");
          return Token();
        }
      }
      else {
        TokenVector bodyExprs;
        parseExprListUntilBrace(&bodyExprs, !isLocal, true);

        body = Token(bodyPos, kBraceOpen, kBraceClose) << bodyExprs;
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


Token
FirstPass::parseFunctionOrVarDef(const Token& defToken, bool isLocal)
{
  assert(fToken == kSymbol);

  Token symToken = fToken;

  String macroId = qualifiedIdForLookup(symToken.idValue());
  Ptr<Macro> macro = fParser->macroRegistry()->lookupMacro(macroId);
  Token macroName = Token(symToken.srcpos(), symToken.idValue());

  if (macro != NULL) {
    TokenVector dummyArgs;
    Token expr= parseMakeMacroCall(macroName, dummyArgs, macro,
                                   true /* parseParams */, isLocal,
                                   kNonScopedDef);
    return expr;
  }
  else
  {
    nextToken();
    if (fToken == kParanOpen)
      return parseFunctionDef(defToken, Token(), symToken, isLocal);

    return parseVarDef2(defToken, Token(), symToken, isLocal);
  }
  return Token();
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

  return parseFunctionDef(defToken, tagToken, symToken, isLocal);
}


Token
FirstPass::parseAliasDef(const Token& defToken, bool isLocal)
{
  assert(fToken == Parser::aliasToken);

  Token tagToken;
  if (isLocal) {
    errorf(fToken.srcpos(), E_LocalAliasDef,
           "inner alias definitions are not supported.");
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
  assert((isClass && fToken == Parser::classToken) ||
         (!isClass && fToken == Parser::typeToken) );

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

  Token requiredProtocol;
  if (fToken == kBraceOpen) {
    requiredProtocol = parseTopOrExprList(true, (isClass
                                                 ? kInClassDef
                                                 : kInTypeDef) );
    if (!requiredProtocol.isSet())
      return scanUntilTopExprAndResume();
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

  if (requiredProtocol.isSet())
    result << requiredProtocol;

  return result;
}


Token
FirstPass::parseSlotDef(const Token& defToken)
{
  assert(fToken == Parser::slotToken);
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
    initExpr = parseExpr();
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
  assert(fToken == Parser::measureToken);
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
  assert(fToken == Parser::unitToken);
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
  Token body = parseExpr();
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


struct heather::EnumItemParser
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

      Token value = pass->parseExpr();
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


Token
FirstPass::parseEnumDef(const Token& defToken, bool isLocal)
{
  assert(fToken == Parser::enumToken);
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
FirstPass::parseMacroComponent(TokenVector* component)
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

    if (fToken == kBraceOpen) {
      braceCount++;
      component->push_back(fToken);
      nextToken();
    }
    else if (fToken == kBraceClose) {
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
    if (fToken == kEOF)
      break;

    if (fToken != kBraceOpen) {
      errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
      scanUntilTopExprAndResume();
      return false;
    }
    nextToken();

    SrcPos patternPos = fToken.srcpos();
    TokenVector pattern;
    TokenVector replacement;
    if (parseMacroComponent(&pattern)) {
      if (fToken == kMapTo) {
        nextToken();

        if (fToken == kBraceOpen) {
          nextToken();
          SrcPos pos = fToken.srcpos();
          if (parseMacroComponent(&replacement)) {
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

    if (fToken == kBraceOpen)
      continue;
    else if (fToken != kBraceClose) {
      errorf(fToken.srcpos(), E_UnexpectedToken,"expected '{' or '}'");
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
  assert(fToken == Parser::macroToken);
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

      Ptr<SyntaxTable> synTable = SyntaxTable::compile(String(""), patterns);
      fParser->macroRegistry()->registerMacro(macroNameToken.idValue(),
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
FirstPass::parseDef(bool isLocal, ScopeType scope)
{
  Token defToken = fToken;
  nextToken();

  switch (scope) {
  case kInTypeDef:
    if (fToken == Parser::genericToken) {
      return parseGenericFunctionDef(defToken, isLocal);
    }
    else {
      error(fToken.srcpos(), E_UnexpDefInClass,
            ( String("unexpected definition type '") + fToken.toString()
              + "'in class") );
      return scanUntilTopExprAndResume();
    }
    break;

  case kInClassDef:
    if (fToken == Parser::slotToken) {
      return parseSlotDef(defToken);
    }
    else if (fToken == Parser::genericToken) {
      return parseGenericFunctionDef(defToken, isLocal);
    }
    else {
      error(fToken.srcpos(), E_UnexpDefInClass,
            ( String("unexpected definition type '") + fToken.toString()
              + "'in class") );
      return scanUntilTopExprAndResume();
    }
    break;

  case kNonScopedDef:
    if (fToken == Parser::typeToken) {
      return parseTypeDef(defToken, false, isLocal);
    }
    else if (fToken == Parser::classToken) {
      return parseTypeDef(defToken, true, isLocal);
    }
    else if (fToken == Parser::aliasToken) {
      return parseAliasDef(defToken, isLocal);
    }
    else if (fToken == Parser::slotToken) {
      errorf(fToken.srcpos(), E_SlotNotInClassDef,
             "slot definitions only allowed in class defs.");
      return scanUntilTopExprAndResume();
    }
    else if (fToken == Parser::enumToken) {
      return parseEnumDef(defToken, isLocal);
    }
    else if (fToken == Parser::measureToken) {
      return parseMeasure(defToken, isLocal);
    }
    else if (fToken == Parser::unitToken) {
      return parseUnit(defToken, isLocal);
    }
    else if (fToken == Parser::constToken ||
             fToken == Parser::fluidToken ||
             fToken == Parser::configToken) {
      return parseVarDef(defToken, fToken, isLocal);
    }
    else if (fToken == Parser::genericToken) {
      return parseGenericFunctionDef(defToken, isLocal);
    }
    else if (fToken == Parser::charToken) {
      return parseCharDef(defToken);
    }
    else if (fToken == Parser::macroToken) {
      return parseMacroDef(defToken);
    }
    else if (fToken == kSymbol)
      return parseFunctionOrVarDef(defToken, isLocal);
    else {
      errorf(fToken.srcpos(), E_DefInitUnexpToken,
             "Bad init value: %s", (const char*)StrHelper(fToken.toString()));
      return scanUntilTopExprAndResume();
    }
    break;
  }

  return Token();
}


Token
FirstPass::parseTop(ScopeType scope)
{
  if (fToken == kModuleId) {
    return parseModule(true);
  }
  else if (fToken == kInterfaceId) {
    return parseModule(false);
  }
  else if (fToken == kExportId) {
    return parseExport();
  }
  else if (fToken == kImportId) {
    return parseImport();
  }
  else if (fToken == kDefId) {
    return parseDef(false, scope);
  }
  else if (fToken == kWhenId) {
    return parseWhen(true, scope);
  }
  else if (fToken == kExtendId) {
    return parseExtend(scope);
  }
  else if (fToken == kOnId) {
    if (scope == kInClassDef)
      return parseOn(kInClassDef);
    else {
      errorf(fToken.srcpos(), E_UnexpectedToken, "Unexpected 'on' expr");
      return scanUntilTopExprAndResume();
    }
  }
  else {
    errorf(fToken.srcpos(), E_UnexpectedToken,
           "Unexpected top expression: %s",
           (const char*)StrHelper(fToken.toString()));
    return scanUntilTopExprAndResume();
  }

  return Token();
}


Token
FirstPass::parse()
{
  Token seq;

  nextToken();
  while (fToken != kEOF) {
    Token n = parseTop(kNonScopedDef);
    if (n.isSet())
      seq << n;
  }

  return seq;
}


//------------------------------------------------------------------------------

bool
FirstPass::replaceSangHashIds(TokenVector* result, const TokenVector& source)
{
  for (size_t idx = 0; idx < source.size(); ) {
    Token token = source[idx];
    if (token == kSymbol) {
      if (idx + 1 < source.size()) {
        if (source[idx + 1] == kSangHash) {
          if (idx + 2 < source.size()) {
            if (source[idx + 2] == kSymbol) {
              result->push_back(
                Token(source[idx].srcpos(),
                      source[idx].idValue() + source[idx + 2].idValue()));
              idx += 3;
              continue;
            }
            else {
              errorf(source[idx + 2].srcpos(), E_OrphanedSangHash,
                     "## requires right hand id value");
              idx += 3;
              continue;
            }
          }
          else {
            errorf(source[idx + 1].srcpos(), E_OrphanedSangHash,
                   "Orphaned ## without following ID");
            idx += 2;
            continue;
          }
        }
      }
    }

    result->push_back(token);
    idx++;
  }

  return true;
}


Token
FirstPass::findReplaceToken(const Token& token,
                            const std::map<String, Token>& bindings)
{
  String paramName = token.macroParamName();
  std::map<String, Token>::const_iterator it = bindings.find(paramName);
  if (it != bindings.end())
    return it->second;

  return Token();
}


bool
FirstPass::replaceMatchBindings(TokenVector* result,
                                const TokenVector& templ,
                                const std::map<String, Token>& bindings)
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
      if (token.tokenType() == kMacroParam) {
        Token replToken = findReplaceToken(token, bindings);
        if (replToken.isSet())
          replacement.push_back(replToken);
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
        if (!replaceMatchBindings(&temp2, token.children(), bindings)) {
          return false;
        }

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

bool
FirstPass::matchExprParamSyntax(const String& paramName,
                                std::map<String, Token>* bindings)
{
  SrcPos pos = fToken.srcpos();
  Token expr = parseExpr();
  if (!expr.isSet()) {
    errorf(pos, E_MacroParamMismatch,
           "Macro parameter %s requires expression",
           (const char*)StrHelper(paramName));
    return false;
  }

  bindings->insert(std::make_pair(paramName, expr));
  return true;
}


bool
FirstPass::matchNameParamSyntax(const String& paramName,
                               std::map<String, Token>* bindings)
{
  if (fToken == kSymbol) {
    bindings->insert(std::make_pair(paramName, fToken));
    nextToken();
    return true;
  }

  errorf(fToken.srcpos(), E_MacroParamMismatch,
         "Macro parameter %s requires identifier",
         (const char*)StrHelper(paramName));
  return false;
}


bool
FirstPass::matchSyntax(TokenVector* result, SyntaxTable* syntaxTable)
{
  SyntaxTreeNode* node = syntaxTable->rootNode();
  assert(node != NULL);

  std::map<String, Token> bindings;

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
        String paramName;
        MacroParamType macroPrmType = macroParamType(macroParam, &paramName);
        switch (macroPrmType) {
        case kMacro_expr:
          if (!matchExprParamSyntax(paramName, &bindings))
            return false;
          node = followSet;
          continue;

        case kMacro_name:
          if (!matchNameParamSyntax(paramName, &bindings))
            return false;
          node = followSet;
          continue;

        case kMacro_body:
          // TODO
          // break;

        default:
          errorf(macroParam.srcpos(), E_MacroParamType,
                 "Unknown macro parameter type: %s",
                 (const char*)StrHelper(macroParam.toString()));
          return false;
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
    Token expr;
    if (isTopLevel)
      expr = parseTop(scopeType);
    else
      expr = parseExpr();

    if (expr.isSet())
      result->push_back(expr);
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
                              Macro* macro,
                              bool shouldParseParams,
                              bool isLocal,
                              ScopeType scopeType)
{
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
      Parser::PortStackHelper portStack(fParser, tempPort);

      TokenVector result;
      if (parseExprStream(&result, !isLocal, scopeType))
        retval = Token() << result;
      else
        return Token();
    }

    fToken = lastCurrentToken;

    return retval;
  }

  return Token();
}
