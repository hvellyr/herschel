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
         fToken != Parser::defToken &&
         fToken != Parser::moduleToken &&
         fToken != Parser::interfaceToken &&
         fToken != Parser::exportToken &&
         fToken != Parser::importToken &&
         fToken != Parser::whenToken)
    nextToken();

  return Token();
}


Token
FirstPass::parseModule(bool isModule)
{
  Token tagToken = fToken;
  nextToken();

  Token modExpr;

  if (fToken.tokenType() == kSymbol) {
    Token modName = fToken;

    nextToken();
    if (fToken.tokenType() == kParanOpen) {
      SrcPos begSp = fToken.srcpos();
      nextToken();

      if (fToken.tokenType() == kString) {
        Token publicId = fToken;

        nextToken();
        if (fToken.tokenType() != kParanClose) {
          logf(fToken.srcpos(), kError, "Syntax error, missing ')'");
          if (fToken == kBraceOpen) {
            // try to continue with this
          }
          else
            return scanUntilTopExprAndResume();
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
        logf(fToken.srcpos(), kError, "Syntax error, missing module name");
        return scanUntilTopExprAndResume();
      }
    }
    else
      modExpr = Token() << tagToken
                        << modName;


    if (fToken.tokenType() == kBraceOpen) {
      SrcPos begSp = fToken.srcpos();
      nextToken();

      Token defines = Token(begSp, kBraceOpen, kBraceClose);
      while (fToken != kBraceClose) {
        if (fToken == kEOF)
          break;

        Token n = parseTop();
        if (n.isSet())
          defines << n;
      }

      if (fToken.tokenType() == kBraceClose) {
        nextToken();
      }
      else {
        logf(fToken.srcpos(), kError, "Syntax error, missing '}'");
        scanUntilTopExprAndResume();
      }

      modExpr << defines;
    }
  }

  return modExpr;
}


Token
FirstPass::parseExport()
{
  Token expr;
  expr << fToken;

  nextToken();

  while (fToken.tokenType() == kSymbol) {
    expr << fToken;
    nextToken();
  }

  if (fToken.tokenType() != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");

  if (fToken.tokenType() == kParanOpen) {
    Token symbols = Token(fToken.srcpos(), kParanOpen, kParanClose);

    nextToken();
    while (fToken.tokenType() != kParanClose) {
      if (fToken == kEOF)
        throw PrematureEndOfFileException();

      if (fToken.isSymbol())
        symbols << fToken;
      else if (fToken == kMultiply)
        symbols << Token(fToken.srcpos(), "*");
      else
        throw UnexpectedTokenException(fToken, "expected SYMBOL or *");
      nextToken();

      if (fToken == kComma)
        nextToken();
      else if (fToken.tokenType() != kParanClose)
        throw UnexpectedTokenException(fToken, "expected ) or ,");
    }

    if (fToken.tokenType() == kParanClose)
      nextToken();

    expr << symbols;
  }
  else
    throw UnexpectedTokenException(fToken, "expected (");

  return expr;
}


Token
FirstPass::parseImport()
{
  Token expr;
  expr << fToken;
  nextToken();

  if (fToken.tokenType() != kString)
    throw UnexpectedTokenException(fToken, "expected STRING");

  expr << Token(fToken);

  nextToken();
  if (fToken.tokenType() == kParanOpen) {
    Token renames = Token(fToken.srcpos(), kParanOpen, kParanClose);

    nextToken();
    while (fToken != kParanClose) {
      if (fToken == kEOF)
        throw PrematureEndOfFileException();
      if (fToken != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      Token first = fToken;

      nextToken();
      if (fToken != kMapTo)
        throw UnexpectedTokenException(fToken, "expected ->");
      Token maptoToken = fToken;

      nextToken();
      if (fToken != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      Token second = fToken;

      renames << ( Token() << first << maptoToken << second );

      nextToken();
      if (fToken == kComma)
        nextToken();
      else if (fToken != kParanClose)
        throw UnexpectedTokenException(fToken, "expected ) or ,");
    }

    if (fToken == kParanClose)
      nextToken();

    expr << renames;
  }

  return expr;
}


Token
FirstPass::parseTypeSpec()
{
  // TODO
  return Token();
}


Token
FirstPass::parseLiteralVector()
{
  bool isDict = false;
  Token nested = Token(fToken.srcpos(), kLiteralVectorOpen, kParanClose);

  nextToken();

  while (fToken.tokenType() != kParanClose) {
    if (fToken == kEOF)
      throw PrematureEndOfFileException();

    Token expr = parseExpr();

    if (!nested.isSet()) {
      if (expr.isBinarySeq(kMapTo))
        isDict = true;

      nested << expr;
    }
    else if (isDict) {
      if (!expr.isBinarySeq(kMapTo))
        throw SyntaxException(String("For literal dictionaries all elements "
                                     "must be MAPTO pairs"));
      nested << expr;
    }
    else
      nested << expr;

    if (fToken.tokenType() == kComma)
      nextToken();
    else if (fToken.tokenType() != kParanClose)
      throw UnexpectedTokenException(fToken, "expected ] or ,");
  }

  if (fToken.tokenType() == kParanClose)
    nextToken();

  return nested;
}


Token
FirstPass::parseLiteralArray()
{
  Token nested = Token(fToken.srcpos(), kLiteralArrayOpen, kBracketClose);

  nextToken();

  while (fToken != kBracketClose) {
    if (fToken == kEOF)
      throw PrematureEndOfFileException();

    Token n = parseExpr();
    nested << n;

    if (fToken.tokenType() == kComma)
      nextToken();
    else if (fToken.tokenType() != kBracketClose)
      throw UnexpectedTokenException(fToken, "expected ] or ,");
  }

  if (fToken.tokenType() == kBracketClose)
    nextToken();

  return nested;
}


Token
FirstPass::parseIf()
{
  Token ifToken = fToken;

  nextToken();

  if (fToken.tokenType() != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");
  SrcPos poSp = fToken.srcpos();
  nextToken();

  Token test = parseExpr();
  if (fToken.tokenType() != kParanClose)
    throw UnexpectedTokenException(fToken, "expected )");
  nextToken();

  Token consequent = parseExpr();

  Token result;
  result << ifToken;

  if (test.isSeq())
    result << ( Token(poSp, kParanOpen, kParanClose)
                << test.children() );
  else
    result << ( Token(poSp, kParanOpen, kParanClose)
                << test );
  result << consequent;

  if (fToken == Parser::elseToken) {
    Token elseToken = fToken;
    nextToken();

    Token alternate = parseExpr();

    result << elseToken << alternate;
  }

  return result;
}


void
FirstPass::parseFunctionsParams(TokenVector* exprlist)
{
  if (fToken.tokenType() == kParanClose) {
    nextToken();
    return;
  }
}


Token
FirstPass::parseOn()
{
  Token tagToken = fToken;
  nextToken();

  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected a SYMBOL");

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
    if (keyToken != Parser::syncToken &&
        keyToken != Parser::initToken &&
        keyToken != Parser::deleteToken &&
        keyToken != Parser::exitToken &&
        keyToken != Parser::signalToken)
      throw UnexpectedTokenException(keyToken);
    nextToken();
    if (fToken.tokenType() != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");
    SrcPos posp = fToken.srcpos();
    nextToken();

    TokenVector params;
    parseFunctionsParams(&params);

    Token body = parseExpr();

    return Token() << tagToken << keyToken
                   << ( Token(posp, kParanOpen, kParanClose)
                        << params )
                   << body;
  }
}


Token
FirstPass::parseAnonFun()
{
  // TODO
  return Token();
}


void
FirstPass::parseFuncallParams(TokenVector* params)
{
  while (fToken != kParanClose) {
    if (fToken == kEOF)
      throw PrematureEndOfFileException();
    if (fToken.isKeyArg()) {
      Token key = fToken;
      nextToken();

      Token val = parseExpr();
      params->push_back(key);
      params->push_back(val);
    }
    else {
      Token val = parseExpr();
      params->push_back(val);
    }

    if (fToken == kComma) {
      params->push_back(fToken);
      nextToken();
    }
    else if (fToken.tokenType() != kParanClose)
      throw UnexpectedTokenException(fToken, "expected ) or ,");
  }

  if (fToken.tokenType() == kParanClose)
    nextToken();
}


Token
FirstPass::parseFunctionCall(const Token& expr,
                             const TokenVector& preScannedArgs,
                             bool parseParams)
{
  TokenVector effParams;
  if (!preScannedArgs.empty())
    effParams.assign(preScannedArgs.begin(), preScannedArgs.end());

  if (parseParams) {
    TokenVector params;
    parseFuncallParams(&params);

    if (!params.empty()) {
      if (!effParams.empty())
        effParams.push_back(Token(params.front().srcpos(), kComma));

      effParams.insert(effParams.end(),
                       params.begin(), params.end());
    }
  }

  return Token() << expr
                 << ( Token(expr.srcpos(), kParanOpen, kParanClose)
                      << effParams );
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
  Token idx = parseExpr();
  if (fToken.tokenType() != kBracketClose)
    throw UnexpectedTokenException(fToken, "expected ]");
  SrcPos bcsp = fToken.srcpos();
  nextToken();

  return Token() << Token(expr.srcpos(), "slice")
                 << ( Token(bcsp, kParanOpen, kParanClose)
                      << expr
                      << Token(idx.srcpos(), kComma)
                      << idx );
}


Token
FirstPass::parseAccess(const Token& expr)
{
  TokenVector args;
  if (fToken.tokenType() == kParanOpen) {
    nextToken();
    return parseAccess(parseParamCall(expr, args, true));
  }
  else if (fToken.tokenType() == kBracketOpen) {
    nextToken();
    return parseAccess(parseSlice(expr));
  }
  else if (fToken.tokenType() == kDot) {
    nextToken();
    if (fToken.tokenType() != kSymbol)
      throw UnexpectedTokenException(fToken, "expected SYMBOL");
    Token symToken = fToken;

    nextToken();
    args.push_back(expr);
    if (fToken.tokenType() == kParanOpen) {
      nextToken();
      return parseAccess(parseParamCall(symToken, args, true));
    }
    else if (fToken.tokenType() == kBracketOpen ||
             fToken.tokenType() == kDot) {
      return parseAccess(parseParamCall(symToken, args, false));
    }
    else {
      return parseParamCall(symToken, args, false);
    }
  }

  return expr;
}


Token
FirstPass::parseGroup()
{
  Token expr = parseExpr();
  if (fToken.tokenType() != kParanClose)
    throw UnexpectedTokenException(fToken, "expected )");

  nextToken();
  return expr;
}


void
FirstPass::parseExprListUntilBrace(TokenVector* result)
{
  for ( ; ; ) {
    if (fToken == Parser::defToken) {
      Token expr = parseDef(false);
      result->push_back(expr);
    }
    else if (fToken == kBraceClose) {
      return;
    }
    else if (fToken == kEOF) {
      return;
    }
    else if (fToken == kSemicolon) {
      nextToken();
    }
    else {
      Token expr = parseExpr();
      result->push_back(expr);
    }
  }
}


void
FirstPass::parseTopExprUntilBrace(TokenVector* result)
{
  while (fToken != kBraceClose) {
    if (fToken == kEOF)
      throw PrematureEndOfFileException();
    Token topexpr = parseTop();
    result->push_back(topexpr);
  }

  if (fToken == kBraceClose)
    nextToken();
}


Token
FirstPass::parseBlock()
{
  TokenVector exprlist;
  parseExprListUntilBrace(&exprlist);

  if (fToken.tokenType() != kBraceClose)
    throw UnexpectedTokenException(fToken, "expected }");
  SrcPos bosp = fToken.srcpos();
  nextToken();

  return Token(bosp, kBraceOpen, kBraceClose) << exprlist;
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
    {
      Token t = fToken;
      nextToken();
      return t;
    }

  case kSymbol:
    if (fToken == Parser::ifToken) {
      return parseIf();
    }
    else if (fToken == Parser::letToken) {
      return parseDef(true);
    }
    else if (fToken == Parser::onToken) {
      return parseOn();
    }
    else if (fToken == Parser::functionToken) {
      nextToken();
      return parseAnonFun();
    }
    else if (fToken == Parser::whenToken) {
      return parseWhen(false);
    }
    else {
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
    nextToken();
    return parseAccess(parseBlock());

  default:
    assert(0);
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
  case kOpFold:
  case kOpMapTo:
  case kOpBy:             return  10;

  case kOpRange:
  case kOpEllipsis:       return  20;

  case kOpLogicalAnd:
  case kOpLogicalOr:      return  30;

  case kOpIsa:            return  35;

  case kOpEqual:
  case kOpUnequal:
  case kOpLess:
  case kOpLessEqual:
  case kOpGreater:
  case kOpGreaterEqual:
  case kOpCompare:        return  40;

  case kOpIn:             return  45;

  case kOpAppend:         return  47;

  case kOpBitAnd:
  case kOpBitOr:
  case kOpBitXor:         return  50;

  case kOpShiftLeft:
  case kOpShiftRight:     return  80;

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
  Token expr2 = parseAtomicExpr();
  OperatorType op2 = tokenTypeToOperator(fToken.tokenType());
  SrcPos op2Srcpos = fToken.srcpos();

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
  OperatorType op1 = tokenTypeToOperator(fToken.tokenType());
  SrcPos opSrcpos = fToken.srcpos();
  if (op1 != kOpInvalid)
    return parseExprRec(expr1, op1, opSrcpos);
  return expr1;
}


Token
FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken.tokenType() == kBraceOpen) {
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
      nextToken();
      inclConsequent = false;
    }
    else if (fToken == Parser::includeToken) {
      nextToken();
      inclAlternate = false;
    }
    else
      throw UnexpectedTokenException(fToken, "expected 'ignore' or 'include'");
  }
  else if (fToken == kParanOpen) {
    SrcPos posp = fToken.srcpos();
    nextToken();

    Token test = parseExpr();
    if (fToken.tokenType() != kParanClose)
      throw UnexpectedTokenException(fToken, "expected )");
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
  else
    throw UnexpectedTokenException(fToken, "expected (");

  Token consequent;
  Token alternate;
  Token elseToken;

  {
    ValueSaver<bool> keep(fEvaluateExprs, inclConsequent);
    consequent = parseTopOrExprList(isTopLevel);
  }

  if (fToken == Parser::elseToken) {
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
FirstPass::parseVarDef(const Token& defToken, VardefFlags flags, bool isLocal)
{
  Token tagToken = fToken;

  nextToken();

  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  Token symbolToken = fToken;

  nextToken();

  return parseVarDef2(defToken, tagToken, symbolToken, flags, isLocal);
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
                        VardefFlags flags, bool isLocal)
{
  Token type;
  Token colonToken, assignToken;

  if (fToken.tokenType() == kColon) {
    colonToken = fToken;
    nextToken();
    type = parseTypeSpec();
  }

  Token initExpr;
  if (fToken.tokenType() == kAssign) {
    assignToken = fToken;
    nextToken();
    initExpr = parseExpr();
  }

  Token vardefExpr;
  vardefExpr << defToken;
  if (flags == kIsConst)
    vardefExpr << tagToken;
  else if (flags == kIsFluid)
    vardefExpr << tagToken;
  else if (flags == kIsConfig)
    vardefExpr << tagToken;
  else if (flags != 0)
    assert(0);

  if (flags == kIsConfig) {
    if (fEvaluateExprs) {
      if (!initExpr.isSet())
        throw SyntaxException(String("Config variable '") + symbolToken +
                              "' without default value");
      fParser->configVarRegistry()->registerValue(symbolToken.idValue(),
                                                  evaluateConfigExpr(initExpr));
      return Token();
    }
  }

  vardefExpr << symbolToken;

  if (type.isSet())
    vardefExpr << colonToken << type;
  if (initExpr.isSet())
    vardefExpr << assignToken << initExpr;

  return vardefExpr;
}


Token
FirstPass::parseCharDef(const Token& defToken)
{
  Token tagToken = fToken;

  nextToken();

  if (fToken != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  Token charNameToken = fToken;

  nextToken();
  if (fToken != kAssign)
    throw UnexpectedTokenException(fToken, "expected =");
  Token assignToken = fToken;

  nextToken();
  if (fToken.tokenType() != kInt)
    throw UnexpectedTokenException(fToken, "expected INTEGER");
  Token codePointToken = fToken;

  int codePoint = fToken.intValue();
  if (codePoint < 0 || codePoint > 0x10FFFF)
    throw SyntaxException(String("invalid expected INTEGER"));

  nextToken();

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
FirstPass::parseFunctionDef(const Token& defToken, const Token& tagToken, 
                            const Token& symToken,
                            bool isGeneric, bool isLocal)
{
  // TODO
  return Token();
}


Token
FirstPass::parseFunctionOrVarDef(const Token& defToken, bool isLocal)
{
  assert(fToken.tokenType() == kSymbol);

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
    if (fToken.tokenType() == kParanOpen)
      return parseFunctionDef(defToken, Token(), symToken, false, isLocal);

    return parseVarDef2(defToken, Token(), symToken, kNoFlags, isLocal);
  }
  return Token();
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
  else if (fToken == Parser::constToken) {
    return parseVarDef(defToken, kIsConst, isLocal);
  }
  else if (fToken == Parser::fluidToken) {
    return parseVarDef(defToken, kIsFluid, false);
  }
  else if (fToken == Parser::configToken) {
    return parseVarDef(defToken, kIsConfig, false);
  }
  else if (fToken == Parser::genericToken) {
    Token tagToken = fToken;

    nextToken();
    if (fToken.tokenType() != kSymbol)
      throw UnexpectedTokenException(fToken, "expected SYMBOL");
    Token symToken = fToken;

    nextToken();
    if (fToken.tokenType() != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");

    return parseFunctionDef(defToken, tagToken, symToken, true, isLocal);
  }
  else if (fToken == Parser::charToken) {
    return parseCharDef(defToken);
  }
  else if (fToken == Parser::macroToken) {
    // TODO
  }
  else if (fToken.tokenType() == kSymbol)
    return parseFunctionOrVarDef(defToken, isLocal);
  else
    throw UnexpectedTokenException(fToken);

  return Token();
}


Token
FirstPass::parseTop()
{
  if (fToken == Parser::moduleToken) {
    return parseModule(true);
  }
  else if (fToken == Parser::interfaceToken) {
    nextToken();
    return parseModule(false);
  }
  else if (fToken == Parser::exportToken) {
    return parseExport();
  }
  else if (fToken == Parser::importToken) {
    return parseImport();
  }
  else if (fToken == Parser::defToken) {
    return parseDef(false);
  }
  else if (fToken == Parser::whenToken) {
    return parseWhen(true);
  }
  else
    throw UnexpectedTokenException(fToken);

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


