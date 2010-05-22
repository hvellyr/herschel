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
FirstPass::scanUntilNextParameter()
{
  while (fToken != kEOF &&
         fToken != kComma &&
         fToken != kParanClose)
    nextToken();

  return Token();
}


template<typename ParseFunctor>
void
FirstPass::parseSequence(ParseFunctor functor,
                         TokenType startToken, TokenType endToken,
                         bool hasSeparator,
                         ErrCodes errorCode,
                         Token& result,
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
              (StringBuffer() << "expected '"
               << Token(SrcPos(), endToken).toString() << "' or ','").toString());
    }
  }

  if (fToken == endToken) {
    nextToken();
  }
  else {
    error(fToken.srcpos(), errorCode,
          (StringBuffer() << "expected '"
           << Token(SrcPos(), endToken).toString() << "'").toString());

    if (startToken != kInvalid && startPos != fToken.srcpos())
      error(startPos, errorCode,
            (StringBuffer() << "beginning '" << Token(SrcPos(), startToken).toString()
             << "' was here").toString());
    scanUntilTopExprAndResume();
  }
}


//----------------------------------------------------------------------------

struct heather::ModuleParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    Token n = pass->parseTop();
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
                    defines);
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
                symbols);

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
                  renames);

    expr << renames;
  }

  return expr;
}


struct heather::TypeParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    Token type = pass->parseTypeSpec(false);
    if (type.isSet())
      result << type;
    return true;
  }
};


Token
FirstPass::parseSimpleType()
{
  assert(fToken == kSymbol);

  Token typeName = fToken;
  nextToken();

  if (fToken == kGenericOpen) {
    Token generics = Token(fToken.srcpos(), kGenericOpen, kGenericClose);
    parseSequence(TypeParser(),
                  kGenericOpen, kGenericClose, true, E_GenericTypeList,
                  generics);
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
                nested);

  return nested;
}


Token
FirstPass::parseUnionType()
{
  Token nested = Token(fToken.srcpos(), kUnionOpen, kParanClose);
  parseSequence(TypeParser(),
                kUnionOpen, kParanClose, true, E_BadParameterList,
                nested);
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


Token
FirstPass::parseConstraintExtend(const Token& baseType)
{
  if (fToken == kEqual || fToken == kUnequal || fToken == kLess ||
      fToken == kGreater || fToken == kLessEqual || fToken == kGreaterEqual ||
      fToken == kCompare || fToken == kIn)
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
FirstPass::parseFunctionType()
{
  Token funcToken = fToken;
  nextToken();

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  SrcPos posp = fToken.srcpos();
  TokenVector params;
  if (parseFunctionsParams(&params)) {
    Token colonToken;
    Token returnType;

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      returnType = parseTypeSpec(true);
    }
    else {
      colonToken = Token(fToken.srcpos(), kColon);
      returnType = Token(fToken.srcpos(), kSymbol, "Any");
    }

    return Token() << funcToken
                   << ( Token(posp, kParanOpen, kParanClose)
                        << params )
                   << colonToken << returnType;
  }
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
             ? parseArrayExtend(parseSimpleType())
             : parseConstraintExtend(parseArrayExtend(parseSimpleType())) );
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

    if (fIsFirst) {
      if (expr.isBinarySeq(kMapTo))
        fIsDict = true;
      result << expr;
      fIsFirst = false;
    }
    else if (fIsDict) {
      if (!expr.isBinarySeq(kMapTo))
        errorf(expr.srcpos(), E_InconsistentArgs,
               "For literal dictionaries all elements must be '->' pairs");
      else
        result << expr;
    }
    else if (expr.isSet())
      result << expr;

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
                nested);
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
                array);
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
  SrcPos posp = fToken.srcpos();
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
    result << ( Token(posp, kParanOpen, kParanClose)
                << test.children() );
  else
    result << ( Token(posp, kParanOpen, kParanClose)
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

  ParseFuncParamsParser()
    : fExpected(kPositional)
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
          paramSeq << typeIntroToken << Token(pos, kSymbol, "Any");
        }
        else
          paramSeq << typeIntroToken << type;
      }
      else
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
      result << paramSeq;
    }
    else if (fExpected == kNamed) {
      if (paramType == kPositional)
        errorf(paramSeq.srcpos(), E_ParamOrder,
               "out of order (positional) parameter");
      else {
        fExpected = paramType;
        result << paramSeq;
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
FirstPass::parseFunctionsParams(TokenVector* exprlist)
{
  Token params;
  parseSequence(ParseFuncParamsParser(),
                kParanOpen, kParanClose, true, E_BadParameterList,
                params);

  if (params.isSeq()) {
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
    SrcPos posp = fToken.srcpos();

    TokenVector params;
    if (parseFunctionsParams(&params)) {
      Token body = parseExpr();

      if (!ignoreStmt && body.isSet())
        return Token() << tagToken << keyToken
                       << ( Token(posp, kParanOpen, kParanClose)
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

  SrcPos posp = fToken.srcpos();
  TokenVector params;
  if (parseFunctionsParams(&params)) {
    Token colonToken;
    Token returnType;

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      returnType = parseTypeSpec(true);
    }
    else {
      colonToken = Token(fToken.srcpos(), kColon);
      returnType = Token(fToken.srcpos(), kSymbol, "Any");
    }

    Token body = parseExpr();
    if (body.isSet())
      return Token() << funcToken
                     << ( Token(posp, kParanOpen, kParanClose)
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
      result << key;
      result << val;
    }
    else {
      Token val = pass->parseExpr();
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
                args, false);

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
        parseDef(isLocal);
        continue;
      }
      expr = parseDef(isLocal);
    }
    else if (fToken == kBraceClose) {
      return true;
    }
    else if (fToken == kEOF) {
      return true;
    }
    else if (fToken == kSemicolon) {
      nextToken();
      continue;
    }
    else if (fToken == kWhenId) {
      expr = parseWhen(!isLocal);
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


Token
FirstPass::parseAtomicExpr()
{
  switch (fToken.tokenType()) {
  case kBool:
  case kInt:
  case kReal:
  case kRational:
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

  case kSelectId:
    // TODO
  case kUntilId:
    // TODO
    break;
  case kWhenId:
    return parseWhen(false);
  case kWhileId:
    // TODO
  case kForId:
    // TODO
  case kMatchId:
    // TODO
    break;

  case kLetId:
    assert(0);
    break;

  case kSymbol:
    {
      Token t = fToken;
      nextToken();
      return parseAccess(t);
    }

  case kLiteralVectorOpen:
    return parseLiteralVector();

  case kLiteralArrayOpen:
    return parseLiteralArray();

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
  else if ( (op1 == kOpRange || op1 == kOpEllipsis) &&
            expr2.isBinarySeq(kBy) ) {
    return Token() << expr1
                   << Token(op1Srcpos, operatorToTokenType(op1))
                   << expr2[0]
                   << expr2[1]
                   << expr2[2];
  }
  else if ( op1 == kOpBy && ( expr1.isBinarySeq(kRange) ||
                              expr1.isBinarySeq(kEllipsis) )) {
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
  case kOpMapTo:          return  10;

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
  case kOpEllipsis:
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
FirstPass::parseTopExprUntilBrace(TokenVector* result)
{
  while (fToken != kBraceClose) {
    if (fToken == kEOF)
      break;
    Token topexpr = parseTop();
    result->push_back(topexpr);
  }

  if (fToken == kBraceClose)
    nextToken();
}


Token
FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken == kBraceOpen) {
      SrcPos bosp = fToken.srcpos();
      nextToken();
      TokenVector exprs;
      parseTopExprUntilBrace(&exprs);

      return Token(bosp, kBraceOpen, kBraceClose) << exprs;
    }
    return parseTop();
  }
  return parseExpr();
}


Token
FirstPass::parseWhen(bool isTopLevel)
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
    SrcPos posp = fToken.srcpos();
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
      result << ( Token(posp, kParanOpen, kParanClose) << test );
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
    consequent = parseTopOrExprList(isTopLevel);
  }

  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();
    {
      ValueSaver<bool> keep(fEvaluateExprs, inclAlternate);
      alternate = parseTopOrExprList(isTopLevel);
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
    type = parseTypeSpec(true);
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

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "missing char name");
    return scanUntilTopExprAndResume();
  }
  Token charNameToken = fToken;

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
    errorf(fToken.srcpos(), E_DefInitValueUnexpectedToken,
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
// generics-const  := `where' constraint-expr { `,' constraint-expr }

// constraint-expr := type-constraint | logic-const | group-const

// type-constraint := subtype-const | sign-constraint
// subtype-const   := type-id subtype-op const-expr
// subtype-op      := COMPARE-OP
// const-expr      := expression
// sign-constraint := type-id `isa' type-clause

// logic-const     := constraint-expr constraint-op constraint-expr
// constraint-op   := `and' | `or'

// group-const     := `(' constraint-expr `)'

  // TODO
  return Token();
}


Token
FirstPass::parseReifyClause()
{
  // TODO
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
    Token reifyToken;
    Token reifyClause;
    Token whereToken;
    Token whereClause;

    if (fToken == kColon) {
      colonToken = fToken;
      nextToken();
      returnType = parseTypeSpec(true);
    }
    else {
      colonToken = Token(fToken.srcpos(), kColon);
      returnType = Token(fToken.srcpos(), kSymbol, "Any");
    }

    if (fToken == kReifyId) {
      reifyToken = fToken;
      nextToken();
      reifyClause = parseReifyClause();
    }

    if (fToken == kWhereId) {
      whereToken = fToken;
      nextToken();
      whereClause = parseWhereClause();
    }

    SrcPos bodyPos = fToken.srcpos();
    Token body;
    if (isLocal) {
      body = parseExpr();
      if (!body.isSet()) {
        errorf(bodyPos, E_MissingBody, "expected function body");
        return Token();
      }
    }
    else {
      TokenVector bodyExprs;
      parseExprListUntilBrace(&bodyExprs, !isLocal, isLocal);

      body = Token(bodyPos, kBraceOpen, kBraceClose) << bodyExprs;
    }

    result << ( Token(paranOpenToken.srcpos(), kParanOpen, kParanClose)
                << params );
    if (colonToken.isSet())
      result << colonToken << returnType;

    if (reifyToken.isSet())
      result << reifyToken << reifyClause;

    if (whereToken.isSet())
      result << whereToken << whereClause;

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
FirstPass::parseDef(bool isLocal)
{
  Token defToken = fToken;
  nextToken();

  if (fToken == Parser::typeToken) {
    // TODO
  }
  else if (fToken == Parser::aliasToken) {
    // TODO
  }
  else if (fToken == Parser::classToken) {
    // TODO
  }
  else if (fToken == Parser::enumToken) {
    // TODO
  }
  else if (fToken == Parser::measureToken) {
    // TODO
  }
  else if (fToken == Parser::unitToken) {
    // TODO
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
    errorf(fToken.srcpos(), E_DefInitValueUnexpectedToken,
           "Bad init value: %s", (const char*)StrHelper(fToken.toString()));
    return scanUntilTopExprAndResume();
  }

  return Token();
}


Token
FirstPass::parseTop()
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
    return parseDef(false);
  }
  else if (fToken == kWhenId) {
    return parseWhen(true);
  }
  else if (fToken == kExtendId) {
    // TODO
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
    Token n = parseTop();
    if (n.isSet())
      seq << n;
  }

  return seq;
}


