/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "pass1.h"
#include "tokenizer.h"
#include "valuesaver.h"
#include "tokeneval.h"
#include "log.h"
#include "errcodes.h"
#include "strbuf.h"


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

struct heather::ModuleParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    Token n = pass->parseTop(FirstPass::kNonScopedDef);
    if (n.isSet())
      result << n;
    else {
      errorf(pass->fToken.srcpos(), E_UnexpectedToken,
             "Unexpected token: %s",
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

  while (fToken == kSymbol) {
    expr << fToken;
    nextToken();
  }

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  Token symbols = Token(fToken.srcpos(), kParanOpen, kParanClose);
  parseSequence(ExportParser(),
                kParanOpen, kParanClose, true, E_BadParameterList,
                symbols,
                "export-symbols");

  expr << symbols;

  return expr;
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

  expr << fToken;

  nextToken();
  if (fToken == kParanOpen) {
    Token renames = Token(fToken.srcpos(), kParanOpen, kParanClose);

    parseSequence(ImportRenameParser(),
                  kParanOpen, kParanClose, true, E_BadParameterList,
                  renames,
                  "import-renames");

    expr << renames;
  }

  return expr;
}


struct heather::TypeParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    SrcPos pos = pass->fToken.srcpos();
    Token type = pass->parseTypeSpec(false);
    if (!type.isSet()) {
      error(pos, E_UnexpectedToken,
            String("returntype expression expected: ") + pass->fToken.toString());
      pass->scanUntilNextParameter();
    }
    else
      result << type;
    return true;
  }
};


Token
FirstPass::parseSimpleType(const Token& baseType)
{
  assert(fToken == kSymbol);

  Token typeName = baseType;
  nextToken();

  if (fToken == kGenericOpen) {
    Token generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(),
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
  parseSequence(TypeParser(),
                kParanOpen, kParanClose, true, E_BadParameterList,
                nested,
                "group-type");

  return nested;
}


Token
FirstPass::parseUnionType()
{
  Token nested = Token(fToken.srcpos(), kUnionOpen, kParanClose);
  parseSequence(TypeParser(),
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
      idxExpr = parseTypeSpec(false);
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
             "Unexpected token: %s", (const char*)StrHelper(pass->fToken.toString()));
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
FirstPass::parseOn()
{
  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
    return scanUntilTopExprAndResume();
  }

  Token keyToken = fToken;

#if 0
  MacroId macroId = qualifiedIdForLookup(keyToken.idValue());
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Token(keyToken.srcpos(), macroId.name());

  if (macro != NULL) {
    Token expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, kIsLocal);
    if (!expr.isSet())
      throw IncompleteMacroException(macroId);
  }
  else
#endif
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
        error(pass->fToken.srcpos(), E_UnexpectedToken,
              String("unexpected token: ") + pass->fToken.toString());
        pass->scanUntilNextParameter();
        return true;
      }
      result << key;
      result << val;
    }
    else {
      Token val = pass->parseExpr();
      if (!val.isSet()) {
        error(pass->fToken.srcpos(), E_UnexpectedToken,
              String("unexpected token: ") + pass->fToken.toString());
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
                          bool parseParams)
{
  if (expr.isSymbol()) {
#if 0
    MacroId macroId  = qualifiedIdForLookup(expr.idValue());
    Ptr<Macro> macro = lookupMacro(macroId);
    MacroType mtype  = lookupMacroType(macroId);
    String mname     = Token(expr.srcpos(), macroId.name());

    if (macro != NULL) {
      Token expr= parseMakeMacroCall(mname, &preScannedArgs, macro, mtype,
                                     parseParams, false);
      if (!expr.isSet())
        throw IncompleteMacroException(macroId);
    }
#endif
  }

  return parseFunctionCall(expr, preScannedArgs, parseParams);
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
      SrcPos startPos = fToken.srcpos();
      expr = parseExpr();
      if (!expr.isSet()) {
        errorf(startPos, E_UnexpectedToken, "unexpected token");
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
  TokenVector parseConsequent(FirstPass* pass)
  {
    TokenVector result;

    assert(pass->fToken == kMapTo);
    Token mapToToken = pass->fToken;
    pass->nextToken();

    Token body = pass->parseExpr();
    if (body.isSet()) {
      result.push_back(mapToToken);
      result.push_back(body);
    }
    return result;
  }
};


struct heather::SelectPatternParser : public BasePatternParser
{
  SelectPatternParser()
    : fOtherwiseSeen(false)
  {}

  bool operator() (FirstPass* pass, Token& result)
  {
    if (pass->fToken == kOtherwiseId) {
      bool ignore = false;
      Token otherwiseToken = pass->fToken;

      if (fOtherwiseSeen) {
        errorf(pass->fToken.srcpos(), E_RedefinedPattern,
               "'otherwise' pattern redefined");
        ignore = true;
      }
      fOtherwiseSeen = true;
      pass->nextToken();

      if (pass->fToken != kMapTo) {
        errorf(pass->fToken.srcpos(), E_BadPatternList,
               "expected '->'");
        pass->scanUntilBrace();
        return false;
      }

      TokenVector consq = parseConsequent(pass);
      if (!ignore && !consq.empty())
        result << ( Token() << otherwiseToken << consq );
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
      }

      TokenVector consq = parseConsequent(pass);
      if (!pattern.empty() && !consq.empty())
        result << ( Token() << ( pattern.size() == 1
                                 ? pattern[0]
                                 : ( Token() << pattern ) )
                    << consq );
    }

    return true;
  }

  bool fOtherwiseSeen;
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

    TokenVector consq = parseConsequent(pass);
    if (varToken.isSet() && colonToken.isSet() &&
        matchType.isSet() && !consq.empty())
      result << ( Token()
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
        if (allowNormalExpr) {
          OperatorType op1 = tokenTypeToOperator(pass->fToken.tokenType());
          SrcPos op1Srcpos = pass->fToken.srcpos();

          if (op1 != kOpInvalid) {
            Token expr = pass->parseExprRec(symToken, op1, op1Srcpos);
            if (expr.isSet()) {
              result << expr;
              return true;
            }
          }
        }

        error(pass->fToken.srcpos(), E_UnexpectedToken,
              String("unexpected token: ") + pass->fToken.toString());
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
            String("unexpected token: ") + pass->fToken.toString());
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
FirstPass::parseAtomicExpr()
{
  switch (fToken.tokenType()) {
  case kInt:
  case kReal:
  case kRational:
    return parseUnitNumber(fToken);

  case kBool:
  case kString:
  case kChar:
  case kKeyword:
  case kNilId:
  case kEofId:
    {
      Token t = fToken;
      nextToken();
      return t;
    }

  case kIfId:
    return parseIf();
  case kOnId:
    return parseOn();
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

  {
    ValueSaver<bool> keep(fEvaluateExprs, inclConsequent);
    consequent = parseTopOrExprList(isTopLevel, scope);
  }

  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();
    {
      ValueSaver<bool> keep(fEvaluateExprs, inclAlternate);
      alternate = parseTopOrExprList(isTopLevel, scope);
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
  else
    return Token() << defToken << tagToken << charNameToken
                   << assignToken << codePointToken;
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
    if (fToken == kEllipsis) {
      body = fToken;
      nextToken();
    }
    else {
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

#if 0
  MacroId macroId = qualifiedIdForLookup(symToken.idValue());
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Token(macroId.name());

  if (macro != NULL) {
    Token expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, isLocal);
    if (!expr.isSet())
      throw IncompleteMacroException(macroId);
  }
  else
#endif
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
    parseSequence(TypeParser(),
                  kGenericOpen, kGenericClose, true, E_GenericTypeList,
                  generics,
                  "alias-params");
  }


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
  result << assignToken << type;
  return result;
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
    parseSequence(TypeParser(),
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

  return Token() << defToken << tagToken << symToken
                 << (Token(paranPos, kParanOpen, kParanClose) << unitToken)
                 << colonToken << isaType;
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

  SrcPos bodyPos = fToken.srcpos();
  Token body = parseExpr();
  if (!body.isSet()) {
    errorf(bodyPos, E_MissingBody, "expected unit def function body");
    return Token();
  }

  return Token() << defToken << tagToken
                 << newUnitToken << mapToToken << refUnitToken
                 << signature.children()
                 << body;
}


struct heather::EnumItemParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    if (pass->fToken != kSymbol) {
      errorf(pass->fToken.srcpos(), E_SymbolExpected, 
             "expected enum item name");
      pass->scanUntilNextParameter(kBraceClose);
      return true;
    }

    Token itemName = pass->fToken;
    pass->nextToken();

    TokenVector enumValue;
    if (pass->fToken == kAssign) {
      Token assignToken = pass->fToken;
      pass->nextToken();

      Token value = pass->parseExpr();
      if (value.isSet()) {
        enumValue.push_back(assignToken);
        enumValue.push_back(value);
      }
    }

    if (enumValue.empty())
      result << itemName;
    else
      result << ( Token() << itemName << enumValue );

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

  if (fToken != kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingBraceOpen, "expected '{'");
    return scanUntilTopExprAndResume();
  }

  Token items = Token(fToken.srcpos(), kBraceOpen, kBraceClose);
  parseSequence(EnumItemParser(),
                kBraceOpen, kBraceClose, true, E_BadEnumItemList,
                items, "enum-items");

  Token enumDefToken = Token() << defToken << tagToken << enumToken;
  if (colonToken.isSet() && isaType.isSet())
    enumDefToken << colonToken << isaType;
  enumDefToken << items;

  return enumDefToken;
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
      // TODO
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
      return parseOn();
    else {
      errorf(fToken.srcpos(), E_UnexpectedToken,
             "Unexpected token: %s", (const char*)StrHelper(fToken.toString()));
      return scanUntilTopExprAndResume();
    }
  }
  else {
    errorf(fToken.srcpos(), E_UnexpectedToken,
           "Unexpected token: %s", (const char*)StrHelper(fToken.toString()));
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


