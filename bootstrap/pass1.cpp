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
#include "pexpreval.h"


//----------------------------------------------------------------------------

using namespace heather;


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
FirstPass::parseModule(bool isModule)
{
  Token modExpr;

  if (fToken.tokenType() == kSymbol) {
    Token modName = fToken;

    nextToken();
    if (fToken.tokenType() == kParanOpen) {
      nextToken();

      if (fToken.tokenType() == kString) {
        Token publicId = fToken;

        nextToken();
        if (fToken.tokenType() == kParanClose) {
          nextToken();

          modExpr = Token()
            << Token(isModule ? "module" : "interface")
            << modName
            << ( Token(kParanOpen, kParanClose)
                 << publicId );
        }
        else
          throw UnexpectedTokenException(fToken, "expected )");
      }
      else
        throw UnexpectedTokenException(fToken, "expected string");
    }
    else
      modExpr = Token() << Token(isModule ? "module" : "interface")
                        << modName;


    if (fToken.tokenType() == kBraceOpen) {
      nextToken();

      Token defines = Token(kBraceOpen, kBraceClose);
      while (fToken != kBraceClose) {
        if (fToken == kEOF)
          throw PrematureEndOfFileException();

        Token n = parseTop();
        if (n.isSet())
          defines << n;
      }

      if (fToken.tokenType() == kBraceClose) {
        nextToken();
      }
      else
        throw UnexpectedTokenException(fToken, "expected }");

      modExpr << defines;
    }
  }

  return modExpr;
}


Token
FirstPass::parseExport()
{
  Token expr;

  expr << Token("export");

  while (fToken.tokenType() == kSymbol) {
    expr << fToken;
    nextToken();
  }

  if (fToken.tokenType() != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");

  if (fToken.tokenType() == kParanOpen) {
    Token symbols = Token(kParanOpen, kParanClose);

    nextToken();
    while (fToken.tokenType() != kParanClose) {
      if (fToken.tokenType() == kEOF)
        throw PrematureEndOfFileException();

      if (fToken.tokenType() == kSymbol)
        symbols << fToken;
      else if (fToken.tokenType() == kMultiply)
        symbols << Token("*");
      else
        throw UnexpectedTokenException(fToken, "expected SYMBOL or *");
      nextToken();

      if (fToken.tokenType() == kComma)
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
  if (fToken.tokenType() != kString)
    throw UnexpectedTokenException(fToken, "expected STRING");

  Token expr;
  expr << Token("import");
  expr << Token(fToken);

  nextToken();
  if (fToken.tokenType() == kParanOpen) {
    Token renames = Token(kParanOpen, kParanClose);

    nextToken();
    while (fToken.tokenType() != kParanClose) {
      if (fToken.tokenType() == kEOF)
        throw PrematureEndOfFileException();
      if (fToken.tokenType() != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      Token first = fToken;

      nextToken();
      if (fToken.tokenType() != kMapTo)
        throw UnexpectedTokenException(fToken, "expected ->");

      nextToken();
      if (fToken.tokenType() != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      Token second = fToken;

      renames << ( Token() << first << Token(kMapTo) << second );

      nextToken();
      if (fToken.tokenType() == kComma)
        nextToken();
      else if (fToken.tokenType() != kParanClose)
        throw UnexpectedTokenException(fToken, "expected ) or ,");
    }

    if (fToken.tokenType() == kParanClose)
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
  Token nested = Token(kLiteralVectorOpen, kParanClose);

  while (fToken.tokenType() != kParanClose) {
    if (fToken.tokenType() == kEOF)
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
  Token nested = Token(kLiteralArrayOpen, kBracketClose);

  while (fToken.tokenType() != kBracketClose) {
    if (fToken.tokenType() == kEOF)
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
  if (fToken.tokenType() != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");
  nextToken();

  Token test = parseExpr();
  if (fToken.tokenType() != kParanClose)
    throw UnexpectedTokenException(fToken, "expected )");
  nextToken();

  Token consequent = parseExpr();

  Token result = Token()
    << Token("if");
  if (test.isSeq())
    result << ( Token(kParanOpen, kParanClose)
                << test.children() );
  else
    result << ( Token(kParanOpen, kParanClose)
                << test );
  result << consequent;

  if (fToken.isSymbol("else")) {
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
  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected a SYMBOL");

  String key = fToken.idValue();

#if 0
  MacroId macroId = qualifiedIdForLookup(key);
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Token(macroId.name());

  if (macro != NULL) {
    Token expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, kIsLocal);
    if (!expr.isSet())
      throw IncompleteMacroException(macroId);
  }
  else
#endif
  {
    if (key != String("sync") && key != String("init") &&
        key != String("delete") && key != String("exit") &&
        key != String("signal"))
      throw UnexpectedTokenException(Token(kSymbol, key));
    nextToken();
    if (fToken.tokenType() != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");
    nextToken();

    TokenVector params;
    parseFunctionsParams(&params);

    Token body = parseExpr();

    return Token() << Token("on") << Token(key)
                   << ( Token(kParanOpen, kParanClose)
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
  while (fToken.tokenType() != kParanClose) {
    if (fToken.tokenType() == kEOF)
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

    if (fToken.tokenType() == kComma) {
      params->push_back(Token(kComma));
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
        effParams.push_back(Token(kComma));
      effParams.insert(effParams.end(),
                       params.begin(), params.end());
    }
  }

  return Token() << expr
                 << ( Token(kParanOpen, kParanClose)
                      << effParams );
}


Token
FirstPass::parseParamCall(const Token& expr,
                          const TokenVector& preScannedArgs,
                          bool parseParams)
{
  if (expr.isSymbol()) {
#if 0
  MacroId macroId  = qualifiedIdForLookup(sym);
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype  = lookupMacroType(macroId);
  String mname     = Token(macroId.name());

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
  nextToken();

  return Token() << Token("slice")
                 << ( Token(kParanOpen, kParanClose)
                      << expr
                      << Token(kComma)
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
    String sym = fToken.idValue();

    nextToken();
    args.push_back(expr);
    if (fToken.tokenType() == kParanOpen) {
      nextToken();
      return parseAccess(parseParamCall(sym, args, true));
    }
    else if (fToken.tokenType() == kBracketOpen ||
             fToken.tokenType() == kDot) {
      return parseAccess(parseParamCall(sym, args, false));
    }
    else {
      return parseParamCall(sym, args, false);
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
    if (fToken.isSymbol("def")) {
      nextToken();
      Token expr = parseDef(false);
      result->push_back(expr);
    }
    else if (fToken.tokenType() == kBraceClose) {
      return;
    }
    else if (fToken.tokenType() == kEOF) {
      return;
    }
    else if (fToken.tokenType() == kSemicolon) {
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
  while (fToken.tokenType() != kBraceClose) {
    if (fToken.tokenType() == kEOF)
      throw PrematureEndOfFileException();
    Token topexpr = parseTop();
    result->push_back(topexpr);
  }

  if (fToken.tokenType() == kBraceClose)
    nextToken();
}


Token
FirstPass::parseBlock()
{
  TokenVector exprlist;
  parseExprListUntilBrace(&exprlist);

  if (fToken.tokenType() != kBraceClose)
    throw UnexpectedTokenException(fToken, "expected }");
  nextToken();

  return Token(kBraceOpen, kBraceClose) << exprlist;
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
    if (fToken.isSymbol("if")) {
      nextToken();
      return parseIf();
    }
    else if (fToken.isSymbol("let")) {
      nextToken();
      return parseDef(true);
    }
    else if (fToken.isSymbol("on")) {
      nextToken();
      return parseOn();
    }
    else if (fToken.isSymbol("function")) {
      nextToken();
      return parseAnonFun();
    }
    else if (fToken.isSymbol("when")) {
      nextToken();
      return parseWhen(false);
    }
    else {
      Token t = fToken;
      nextToken();
      return parseAccess(t);
    }

  case kLiteralVectorOpen:
    nextToken();
    return parseLiteralVector();

  case kLiteralArrayOpen:
    nextToken();
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
FirstPass::makeAssignToken(const Token& expr1, const Token& expr2) const
{
  if (expr1.isSymFuncall()) {
    // rename the function call in expr1 to name! and append expr2 as last
    // parameter to expr1's parameter list
    return Token() << Token(expr1[0].idValue() + "!")
                   << ( Token(kParanOpen, kParanClose)
                        << expr1.children()
                        << Token(kComma)
                        << expr2 );
  }
  else
    return Token() << expr1 << Token(kAssign) << expr2;
}


Token
FirstPass::makeBinaryToken(const Token& expr1, OperatorType op1,
                           const Token& expr2) const
{
  if (op1 == kOpAssign)
    return makeAssignToken(expr1, expr2);
  else
    return Token() << expr1 << Token(operatorToTokenType(op1)) << expr2;
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
FirstPass::parseExprRec(const Token& expr1, OperatorType op1)
{
  if (op1 == kOpInvalid)
    return expr1;

  nextToken();
  Token expr2 = parseAtomicExpr();
  OperatorType op2 = tokenTypeToOperator(fToken.tokenType());
  if (op2 == kOpInvalid) {
    if (op1 == kOpAssign)
      return makeAssignToken(expr1, expr2);
    else
      return Token() << expr1 << Token(operatorToTokenType(op1)) << expr2;
  }
  else {
    if (!isRightOperator(op1) && isOpWeightAbove(op1, op2))
      return parseExprRec(makeBinaryToken(expr1, op1, expr2), op2);
    else
      return makeBinaryToken(expr1, op1, parseExprRec(expr2, op2));
  }
}


Token
FirstPass::parseExpr()
{
  Token expr1 = parseAtomicExpr();
  OperatorType op1 = tokenTypeToOperator(fToken.tokenType());
  if (op1 != kOpInvalid)
    return parseExprRec(expr1, op1);
  return expr1;
}


Token
FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken.tokenType() == kBraceOpen) {
      nextToken();
      TokenVector exprs;
      parseTopExprUntilBrace(&exprs);

      return Token(kBraceOpen, kBraceClose) << exprs;
    }
    else
      return parseTop();
  }
  else
    return parseExpr();
}


Token
FirstPass::parseWhen(bool isTopLevel)
{
  Token result;
  result << Token("when");

  bool inclConsequent = true;
  bool inclAlternate = true;

  if (fToken.isSymbol()) {
    if (fToken.isSymbol("ignore")) {
      nextToken();
      inclConsequent = false;
    }
    else if (fToken.isSymbol("include")) {
      nextToken();
      inclAlternate = false;
    }
    else
      throw UnexpectedTokenException(fToken, "expected 'ignore' or 'include'");
  }
  else if (fToken.tokenType() == kParanOpen) {
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
      result << ( Token(kParanOpen, kParanClose) << test );
  }
  else
    throw UnexpectedTokenException(fToken, "expected (");

  Token consequent;
  Token alternate;

  {
    ValueSaver<bool> keep(fEvaluateExprs, inclConsequent);
    consequent = parseTopOrExprList(isTopLevel);
  }

  if (fToken.isSymbol("else")) {
    nextToken();
    {
      ValueSaver<bool> keep(fEvaluateExprs, inclAlternate);
      alternate = parseTopOrExprList(isTopLevel);
    }
  }

  if (inclConsequent && inclAlternate) {
    result << consequent;
    if (alternate.isSet())
      result << Token("else") << alternate;

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
FirstPass::parseVarDef(VardefFlags flags, bool isLocal)
{
  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  String symbolName = fToken.idValue();

  nextToken();

  return parseVarDef2(symbolName, flags, isLocal);
}


Token
FirstPass::evaluateConfigExpr(const Token& initExpr)
{
  TokenEvalContext ctx(fParser->configVarRegistry());
  return ctx.evalToken(initExpr);
}


Token
FirstPass::parseVarDef2(const String& symbolName, VardefFlags flags,
                        bool isLocal)
{
  Token type;
  if (fToken.tokenType() == kColon) {
    nextToken();
    type = parseTypeSpec();
  }

  Token initExpr;
  if (fToken.tokenType() == kAssign) {
    nextToken();
    initExpr = parseExpr();
  }

  Token vardefExpr;
  vardefExpr << Token(isLocal ? "let" : "def");
  if (flags == kIsConst)
    vardefExpr << Token("const");
  else if (flags == kIsFluid)
    vardefExpr << Token("fluid");
  else if (flags == kIsConfig)
    vardefExpr << Token("config");
  else if (flags != 0)
    assert(0);

  if (flags == kIsConfig) {
    if (fEvaluateExprs) {
      if (!initExpr.isSet())
        throw SyntaxException(String("Config variable '") + symbolName +
                              "' without default value");
      fParser->configVarRegistry()->registerValue(symbolName,
                                                  evaluateConfigExpr(initExpr));
      return Token();
    }
  }

  vardefExpr << Token(symbolName);

  if (type.isSet())
    vardefExpr << Token(kColon) << type;
  if (initExpr.isSet())
    vardefExpr << Token(kAssign) << initExpr;

  return vardefExpr;
}


Token
FirstPass::parseCharDef()
{
  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  String charName = fToken.idValue();

  nextToken();
  if (fToken.tokenType() != kAssign)
    throw UnexpectedTokenException(fToken, "expected =");

  nextToken();
  if (fToken.tokenType() != kInt)
    throw UnexpectedTokenException(fToken, "expected INTEGER");

  int codePoint = fToken.intValue();
  if (codePoint < 0 || codePoint > 0x10FFFF)
    throw SyntaxException(String("invalid expected INTEGER"));

  nextToken();

  if (fEvaluateExprs) {
    fParser->charRegistry()->registerValue(charName, codePoint);
    return Token();
  }
  else
    return Token() << Token("def") << Token("char") << Token(charName)
                   << Token(kAssign) << Token(kInt, codePoint);
}


Token
FirstPass::parseFunctionDef(const String& sym, bool isGeneric,
                            bool isLocal)
{
  // TODO
  return Token();
}


Token
FirstPass::parseFunctionOrVarDef(bool isLocal)
{
  assert(fToken.tokenType() == kSymbol);

  String sym = fToken.idValue();

#if 0
  MacroId macroId = qualifiedIdForLookup(sym);
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
      return parseFunctionDef(sym, false, isLocal);

    return parseVarDef2(sym, kNoFlags, isLocal);
  }
  return Token();
}


Token
FirstPass::parseDef(bool isLocal)
{
  if (fToken.isSymbol("type")) {
    // TODO
  }
  else if (fToken.isSymbol("alias")) {
    // TODO
  }
  else if (fToken.isSymbol("class")) {
    // TODO
  }
  else if (fToken.isSymbol("enum")) {
    // TODO
  }
  else if (fToken.isSymbol("measure")) {
    // TODO
  }
  else if (fToken.isSymbol("unit")) {
    // TODO
  }
  else if (fToken.isSymbol("const")) {
    nextToken();
    return parseVarDef(kIsConst, isLocal);
  }
  else if (fToken.isSymbol("fluid")) {
    nextToken();
    return parseVarDef(kIsFluid, false);
  }
  else if (fToken.isSymbol("config")) {
    nextToken();
    return parseVarDef(kIsConfig, false);
  }
  else if (fToken.isSymbol("generic")) {
    nextToken();
    if (fToken.tokenType() != kSymbol)
      throw UnexpectedTokenException(fToken, "expected SYMBOL");
    String sym = fToken.idValue();

    nextToken();
    if (fToken.tokenType() != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");
    return parseFunctionDef(sym, true, isLocal);
  }
  else if (fToken.isSymbol("char")) {
    nextToken();
    return parseCharDef();
  }
  else if (fToken.isSymbol("macro")) {
    // TODO
  }
  else if (fToken.tokenType() == kSymbol)
    return parseFunctionOrVarDef(isLocal);
  else
    throw UnexpectedTokenException(fToken);

  return Token();
}


Token
FirstPass::parseTop()
{
  if (fToken.isSymbol("module")) {
    nextToken();
    return parseModule(true);
  }
  else if (fToken.isSymbol("interface")) {
    nextToken();
    return parseModule(false);
  }
  else if (fToken.isSymbol("export")) {
    nextToken();
    return parseExport();
  }
  else if (fToken.isSymbol("import")) {
    nextToken();
    return parseImport();
  }
  else if (fToken.isSymbol("def")) {
    nextToken();
    return parseDef(false);
  }
  else if (fToken.isSymbol("when")) {
    nextToken();
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
  while (fToken.tokenType() != kEOF) {
    Token n = parseTop();
    if (n.isSet())
      seq << n;
  }

  return seq;
}


