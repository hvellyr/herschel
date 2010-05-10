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


FirstPass::FirstPass(Parser* parser, Token currentToken)
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


Pexpr
FirstPass::parseModule(bool isModule)
{
  Pexpr modExpr;

  if (fToken.fType == kSymbol) {
    String modName = fToken.fStrValue;

    nextToken();
    if (fToken.fType == kParanOpen) {
      nextToken();

      if (fToken.fType == kString) {
        Token publicId = fToken;

        nextToken();
        if (fToken.fType == kParanClose) {
          nextToken();

          modExpr = Pexpr()
            << Pexpr(isModule ? "module" : "interface")
            << Pexpr(modName)
            << ( Pexpr(kParanOpen, kParanClose)
                 << Pexpr(publicId) );
        }
        else
          throw UnexpectedTokenException(fToken, "expected )");
      }
      else
        throw UnexpectedTokenException(fToken, "expected string");
    }
    else
      modExpr = Pexpr() << Pexpr(isModule ? "module" : "interface")
                        << Pexpr(modName);


    if (fToken.fType == kBraceOpen) {
      nextToken();

      Pexpr defines = Pexpr(kBraceOpen, kBraceClose);
      while (fToken != kBraceClose) {
        if (fToken == kEOF)
          throw PrematureEndOfFileException();

        Pexpr n = parseTop();
        if (n.isSet())
          defines << n;
      }

      if (fToken.fType == kBraceClose) {
        nextToken();
      }
      else
        throw UnexpectedTokenException(fToken, "expected }");

      modExpr << defines;
    }
  }

  return modExpr;
}


Pexpr
FirstPass::parseExport()
{
  Pexpr expr;

  expr << Pexpr("export");

  while (fToken.fType == kSymbol) {
    expr << Pexpr(fToken.fStrValue);
    nextToken();
  }

  if (fToken.fType != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");

  if (fToken.fType == kParanOpen) {
    Pexpr symbols = Pexpr(kParanOpen, kParanClose);

    nextToken();
    while (fToken.fType != kParanClose) {
      if (fToken.fType == kEOF)
        throw PrematureEndOfFileException();

      if (fToken.fType == kSymbol)
        symbols << Pexpr(fToken.fStrValue);
      else if (fToken.fType == kMultiply)
        symbols << Pexpr("*");
      else
        throw UnexpectedTokenException(fToken, "expected SYMBOL or *");
      nextToken();

      if (fToken.fType == kComma)
        nextToken();
      else if (fToken.fType != kParanClose)
        throw UnexpectedTokenException(fToken, "expected ) or ,");
    }

    if (fToken.fType == kParanClose)
      nextToken();

    expr << symbols;
  }
  else
    throw UnexpectedTokenException(fToken, "expected (");

  return expr;
}


Pexpr
FirstPass::parseImport()
{
  if (fToken.fType != kString)
    throw UnexpectedTokenException(fToken, "expected STRING");

  Pexpr expr;
  expr << Pexpr("import");
  expr << Pexpr(fToken);

  nextToken();
  if (fToken.fType == kParanOpen) {
    Pexpr renames = Pexpr(kParanOpen, kParanClose);

    nextToken();
    while (fToken.fType != kParanClose) {
      if (fToken.fType == kEOF)
        throw PrematureEndOfFileException();
      if (fToken.fType != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      String first = fToken.fStrValue;

      nextToken();
      if (fToken.fType != kMapTo)
        throw UnexpectedTokenException(fToken, "expected ->");

      nextToken();
      if (fToken.fType != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      String second = fToken.fStrValue;

      renames << ( Pexpr() << Pexpr(first)
                   << Pexpr(kMapTo)
                   << Pexpr(second) );

      nextToken();
      if (fToken.fType == kComma)
        nextToken();
      else if (fToken.fType != kParanClose)
        throw UnexpectedTokenException(fToken, "expected ) or ,");
    }

    if (fToken.fType == kParanClose)
      nextToken();

    expr << renames;
  }

  return expr;
}


Pexpr
FirstPass::parseTypeSpec()
{
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseLiteralVector()
{
  bool isDict = false;
  Pexpr nested = Pexpr(kLiteralVectorOpen, kParanClose);

  while (fToken.fType != kParanClose) {
    if (fToken.fType == kEOF)
      throw PrematureEndOfFileException();

    Pexpr expr = parseExpr();

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

    if (fToken.fType == kComma)
      nextToken();
    else if (fToken.fType != kParanClose)
      throw UnexpectedTokenException(fToken, "expected ] or ,");
  }

  if (fToken.fType == kParanClose)
    nextToken();

  return nested;
}


Pexpr
FirstPass::parseLiteralArray()
{
  Pexpr nested = Pexpr(kLiteralArrayOpen, kBracketClose);

  while (fToken.fType != kBracketClose) {
    if (fToken.fType == kEOF)
      throw PrematureEndOfFileException();

    Pexpr n = parseExpr();
    nested << n;

    if (fToken.fType == kComma)
      nextToken();
    else if (fToken.fType != kBracketClose)
      throw UnexpectedTokenException(fToken, "expected ] or ,");
  }

  if (fToken.fType == kBracketClose)
    nextToken();

  return nested;
}


Pexpr
FirstPass::parseIf()
{
  if (fToken.fType != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");
  nextToken();

  Pexpr test = parseExpr();
  if (fToken.fType != kParanClose)
    throw UnexpectedTokenException(fToken, "expected )");
  nextToken();

  Pexpr consequent = parseExpr();

  Pexpr result = Pexpr()
    << Pexpr("if");
  if (test.isSeq())
    result << ( Pexpr(kParanOpen, kParanClose)
                << test.children() );
  else
    result << ( Pexpr(kParanOpen, kParanClose)
                << test );
  result << consequent;

  if (fToken == Token(kSymbol, "else")) {
    nextToken();

    Pexpr alternate = parseExpr();

    result << Pexpr("else") << alternate;
  }

  return result;
}


void
FirstPass::parseFunctionsParams(PexprVector* exprlist)
{
  if (fToken.fType == kParanClose) {
    nextToken();
    return;
  }
}


Pexpr
FirstPass::parseOn()
{
  if (fToken.fType != kSymbol)
    throw UnexpectedTokenException(fToken, "expected a SYMBOL");

  String key = fToken.fStrValue;

#if 0
  MacroId macroId = qualifiedIdForLookup(key);
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Pexpr(macroId.name());

  if (macro != NULL) {
    Pexpr expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, kIsLocal);
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
    if (fToken.fType != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");
    nextToken();

    PexprVector params;
    parseFunctionsParams(&params);

    Pexpr body = parseExpr();

    return Pexpr() << Pexpr("on") << Pexpr(key)
                   << ( Pexpr(kParanOpen, kParanClose)
                        << params )
                   << body;
  }
}


Pexpr
FirstPass::parseAnonFun()
{
  // TODO
  return Pexpr();
}


void
FirstPass::parseFuncallParams(PexprVector* params)
{
  while (fToken.fType != kParanClose) {
    if (fToken.fType == kEOF)
      throw PrematureEndOfFileException();
    if (fToken.isKeyArg()) {
      String key = fToken.fStrValue;
      nextToken();

      Pexpr val = parseExpr();
      params->push_back(key);
      params->push_back(val);
    }
    else {
      Pexpr val = parseExpr();
      params->push_back(val);
    }

    if (fToken.fType == kComma) {
      params->push_back(Pexpr(kComma));
      nextToken();
    }
    else if (fToken.fType != kParanClose)
      throw UnexpectedTokenException(fToken, "expected ) or ,");
  }

  if (fToken.fType == kParanClose)
    nextToken();
}


Pexpr
FirstPass::parseFunctionCall(const Pexpr& expr,
                             const PexprVector& preScannedArgs,
                             bool parseParams)
{
  PexprVector effParams;
  if (!preScannedArgs.empty())
    effParams.assign(preScannedArgs.begin(), preScannedArgs.end());

  if (parseParams) {
    PexprVector params;
    parseFuncallParams(&params);

    if (!params.empty()) {
      if (!effParams.empty())
        effParams.push_back(Pexpr(kComma));
      effParams.insert(effParams.end(),
                       params.begin(), params.end());
    }
  }

  return Pexpr() << expr
                 << ( Pexpr(kParanOpen, kParanClose)
                      << effParams );
}


Pexpr
FirstPass::parseParamCall(const Pexpr& expr,
                          const PexprVector& preScannedArgs,
                          bool parseParams)
{
  if (expr.isId()) {
#if 0
  MacroId macroId  = qualifiedIdForLookup(sym);
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype  = lookupMacroType(macroId);
  String mname     = Pexpr(macroId.name());

  if (macro != NULL) {
    Pexpr expr= parseMakeMacroCall(mname, &preScannedArgs, macro, mtype,
                                   parseParams, false);
    if (!expr.isSet())
      throw IncompleteMacroException(macroId);
  }
#endif
  }

  return parseFunctionCall(expr, preScannedArgs, parseParams);
}


Pexpr
FirstPass::parseSlice(const Pexpr& expr)
{
  Pexpr idx = parseExpr();
  if (fToken.fType != kBracketClose)
    throw UnexpectedTokenException(fToken, "expected ]");
  nextToken();

  return Pexpr() << Pexpr("slice")
                 << ( Pexpr(kParanOpen, kParanClose)
                      << expr
                      << Pexpr(kComma)
                      << idx );
}


Pexpr
FirstPass::parseAccess(const Pexpr& expr)
{
  PexprVector args;
  if (fToken.fType == kParanOpen) {
    nextToken();
    return parseAccess(parseParamCall(expr, args, true));
  }
  else if (fToken.fType == kBracketOpen) {
    nextToken();
    return parseAccess(parseSlice(expr));
  }
  else if (fToken.fType == kDot) {
    nextToken();
    if (fToken.fType != kSymbol)
      throw UnexpectedTokenException(fToken, "expected SYMBOL");
    String sym = fToken.fStrValue;

    nextToken();
    args.push_back(expr);
    if (fToken.fType == kParanOpen) {
      nextToken();
      return parseAccess(parseParamCall(sym, args, true));
    }
    else if (fToken.fType == kBracketOpen ||
             fToken.fType == kDot) {
      return parseAccess(parseParamCall(sym, args, false));
    }
    else {
      return parseParamCall(sym, args, false);
    }
  }

  return expr;
}


Pexpr
FirstPass::parseGroup()
{
  Pexpr expr = parseExpr();
  if (fToken.fType != kParanClose)
    throw UnexpectedTokenException(fToken, "expected )");

  nextToken();
  return expr;
}


void
FirstPass::parseExprListUntilBrace(PexprVector* result)
{
  for ( ; ; ) {
    if (fToken == Token(kSymbol, "def")) {
      nextToken();
      Pexpr expr = parseDef(false);
      result->push_back(expr);
    }
    else if (fToken.fType == kBraceClose) {
      return;
    }
    else if (fToken.fType == kEOF) {
      return;
    }
    else if (fToken.fType == kSemicolon) {
      nextToken();
    }
    else {
      Pexpr expr = parseExpr();
      result->push_back(expr);
    }
  }
}


void
FirstPass::parseTopExprUntilBrace(PexprVector* result)
{
  while (fToken.fType != kBraceClose) {
    if (fToken.fType == kEOF)
      throw PrematureEndOfFileException();
    Pexpr topexpr = parseTop();
    result->push_back(topexpr);
  }

  if (fToken.fType == kBraceClose)
    nextToken();
}


Pexpr
FirstPass::parseBlock()
{
  PexprVector exprlist;
  parseExprListUntilBrace(&exprlist);

  if (fToken.fType != kBraceClose)
    throw UnexpectedTokenException(fToken, "expected }");
  nextToken();

  return Pexpr(kBraceOpen, kBraceClose) << exprlist;
}


Pexpr
FirstPass::parseAtomicExpr()
{
  switch (fToken.fType) {
  case kBool:
  case kInteger:
  case kReal:
  case kRational:
  case kString:
  case kChar:
  case kKeyword:
    {
      Token t = fToken;
      nextToken();
      return Pexpr(t);
    }

  case kSymbol:
    if (fToken == Token(kSymbol, "if")) {
      nextToken();
      return parseIf();
    }
    else if (fToken == Token(kSymbol, "let")) {
      nextToken();
      return parseDef(true);
    }
    else if (fToken == Token(kSymbol, "on")) {
      nextToken();
      return parseOn();
    }
    else if (fToken == Token(kSymbol, "function")) {
      nextToken();
      return parseAnonFun();
    }
    else if (fToken == Token(kSymbol, "when")) {
      nextToken();
      return parseWhen(false);
    }
    else {
      Token t = fToken;
      nextToken();
      return parseAccess(Pexpr(t.fStrValue));
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

  return Pexpr();
}


Pexpr
FirstPass::makeAssignPexpr(const Pexpr& expr1, const Pexpr& expr2) const
{
  if (expr1.isSymFuncall()) {
    // rename the function call in expr1 to name! and append expr2 as last
    // parameter to expr1's parameter list
    return Pexpr() << Pexpr(expr1[0].idValue() + "!")
                   << ( Pexpr(kParanOpen, kParanClose)
                        << expr1.children()
                        << Pexpr(kComma)
                        << expr2 );
  }
  else
    return Pexpr() << expr1 << Pexpr(kAssign) << expr2;
}


Pexpr
FirstPass::makeBinaryPexpr(const Pexpr& expr1, OperatorType op1,
                           const Pexpr& expr2) const
{
  if (op1 == kOpAssign)
    return makeAssignPexpr(expr1, expr2);
  else
    return Pexpr() << expr1 << Pexpr(operatorToTokenType(op1)) << expr2;
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


Pexpr
FirstPass::parseExprRec(const Pexpr& expr1, OperatorType op1)
{
  if (op1 == kOpInvalid)
    return expr1;

  nextToken();
  Pexpr expr2 = parseAtomicExpr();
  OperatorType op2 = tokenTypeToOperator(fToken.fType);
  if (op2 == kOpInvalid) {
    if (op1 == kOpAssign)
      return makeAssignPexpr(expr1, expr2);
    else
      return Pexpr() << expr1 << Pexpr(operatorToTokenType(op1)) << expr2;
  }
  else {
    if (!isRightOperator(op1) && isOpWeightAbove(op1, op2))
      return parseExprRec(makeBinaryPexpr(expr1, op1, expr2), op2);
    else
      return makeBinaryPexpr(expr1, op1, parseExprRec(expr2, op2));
  }
}


Pexpr
FirstPass::parseExpr()
{
  Pexpr expr1 = parseAtomicExpr();
  OperatorType op1 = tokenTypeToOperator(fToken.fType);
  if (op1 != kOpInvalid)
    return parseExprRec(expr1, op1);
  return expr1;
}


Pexpr
FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken.fType == kBraceOpen) {
      nextToken();
      PexprVector exprs;
      parseTopExprUntilBrace(&exprs);

      return Pexpr(kBraceOpen, kBraceClose) << exprs;
    }
    else
      return parseTop();
  }
  else
    return parseExpr();
}


Pexpr
FirstPass::parseWhen(bool isTopLevel)
{
  Pexpr result;
  result << Pexpr("when");

  bool inclConsequent = true;
  bool inclAlternate = true;

  if (fToken.fType == kSymbol) {
    if (fToken == Token(kSymbol, "ignore")) {
      nextToken();
      inclConsequent = false;
    }
    else if (fToken == Token(kSymbol, "include")) {
      nextToken();
      inclAlternate = false;
    }
    else
      throw UnexpectedTokenException(fToken, "expected 'ignore' or 'include'");
  }
  else if (fToken.fType == kParanOpen) {
    nextToken();
    Pexpr test = parseExpr();
    if (fToken.fType != kParanClose)
      throw UnexpectedTokenException(fToken, "expected )");
    nextToken();

    if (fEvaluateExprs) {
      PexprEvalContext ctx(fParser->configVarRegistry());
      Pexpr p = ctx.evalPexpr(test);
      if (p.isBoolLit()) {
        inclConsequent = p.boolLitValue();
        inclAlternate = !p.boolLitValue();
      }
      else {
        fprintf(stderr, "ERROR: when-expression did not evaluate to boolean. "
                "Treat it as false\n");
        inclConsequent = false;
        inclAlternate = true;
      }
    }
    else
      result << ( Pexpr(kParanOpen, kParanClose) << test );
  }
  else
    throw UnexpectedTokenException(fToken, "expected (");

  Pexpr consequent;
  Pexpr alternate;

  {
    ValueSaver<bool> keep(fEvaluateExprs, inclConsequent);
    consequent = parseTopOrExprList(isTopLevel);
  }

  if (fToken == Token(kSymbol, "else")) {
    nextToken();
    {
      ValueSaver<bool> keep(fEvaluateExprs, inclAlternate);
      alternate = parseTopOrExprList(isTopLevel);
    }
  }

  if (inclConsequent && inclAlternate) {
    result << consequent;
    if (alternate.isSet())
      result << Pexpr("else") << alternate;

    return result;
  }
  else if (inclConsequent)
    return consequent;
  else if (inclAlternate)
    return alternate;

  assert(0);
  return Pexpr();
}


Pexpr
FirstPass::parseVarDef(VardefFlags flags, bool isLocal)
{
  if (fToken.fType != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  String symbolName = fToken.fStrValue;

  nextToken();

  return parseVarDef2(symbolName, flags, isLocal);
}


Pexpr
FirstPass::evaluateConfigExpr(const Pexpr& initExpr)
{
  PexprEvalContext ctx(fParser->configVarRegistry());
  return ctx.evalPexpr(initExpr);
}


Pexpr
FirstPass::parseVarDef2(const String& symbolName, VardefFlags flags,
                        bool isLocal)
{
  Pexpr type;
  if (fToken.fType == kColon) {
    nextToken();
    type = parseTypeSpec();
  }

  Pexpr initExpr;
  if (fToken.fType == kAssign) {
    nextToken();
    initExpr = parseExpr();
  }

  Pexpr vardefExpr;
  vardefExpr << Pexpr(isLocal ? "let" : "def");
  if (flags == kIsConst)
    vardefExpr << Pexpr("const");
  else if (flags == kIsFluid)
    vardefExpr << Pexpr("fluid");
  else if (flags == kIsConfig)
    vardefExpr << Pexpr("config");
  else if (flags != 0)
    assert(0);

  if (flags == kIsConfig) {
    if (fEvaluateExprs) {
      if (!initExpr.isSet())
        throw SyntaxException(String("Config variable '") + symbolName +
                              "' without default value");
      fParser->configVarRegistry()->registerValue(symbolName,
                                                  evaluateConfigExpr(initExpr));
      return Pexpr();
    }
  }

  vardefExpr << Pexpr(symbolName);

  if (type.isSet())
    vardefExpr << Pexpr(kColon) << type;
  if (initExpr.isSet())
    vardefExpr << Pexpr(kAssign) << initExpr;

  return vardefExpr;
}


Pexpr
FirstPass::parseCharDef()
{
  if (fToken.fType != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  String charName = fToken.fStrValue;

  nextToken();
  if (fToken.fType != kAssign)
    throw UnexpectedTokenException(fToken, "expected =");

  nextToken();
  if (fToken.fType != kInteger)
    throw UnexpectedTokenException(fToken, "expected INTEGER");

  int codePoint = fToken.fIntValue;
  if (codePoint < 0 || codePoint > 0x10FFFF)
    throw SyntaxException(String("invalid expected INTEGER"));

  nextToken();

  if (fEvaluateExprs) {
    fParser->charRegistry()->registerValue(charName, codePoint);
    return Pexpr();
  }
  else
    return Pexpr() << Pexpr("def") << Pexpr("char") << Pexpr(charName)
                   << Pexpr(kAssign) << Pexpr(Token(kInteger, codePoint));
}


Pexpr
FirstPass::parseFunctionDef(const String& sym, bool isGeneric,
                            bool isLocal)
{
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseFunctionOrVarDef(bool isLocal)
{
  assert(fToken.fType == kSymbol);

  String sym = fToken.fStrValue;

#if 0
  MacroId macroId = qualifiedIdForLookup(sym);
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Pexpr(macroId.name());

  if (macro != NULL) {
    Pexpr expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, isLocal);
    if (!expr.isSet())
      throw IncompleteMacroException(macroId);
  }
  else
#endif
  {
    nextToken();
    if (fToken.fType == kParanOpen)
      return parseFunctionDef(sym, false, isLocal);

    return parseVarDef2(sym, kNoFlags, isLocal);
  }
  return Pexpr();
}


Pexpr
FirstPass::parseDef(bool isLocal)
{
  if (fToken == Token(kSymbol, "type")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "alias")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "class")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "enum")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "measure")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "unit")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "const")) {
    nextToken();
    return parseVarDef(kIsConst, isLocal);
  }
  else if (fToken == Token(kSymbol, "fluid")) {
    nextToken();
    return parseVarDef(kIsFluid, false);
  }
  else if (fToken == Token(kSymbol, "config")) {
    nextToken();
    return parseVarDef(kIsConfig, false);
  }
  else if (fToken == Token(kSymbol, "generic")) {
    nextToken();
    if (fToken.fType != kSymbol)
      throw UnexpectedTokenException(fToken, "expected SYMBOL");
    String sym = fToken.fStrValue;

    nextToken();
    if (fToken.fType != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");
    return parseFunctionDef(sym, true, isLocal);
  }
  else if (fToken == Token(kSymbol, "char")) {
    nextToken();
    return parseCharDef();
  }
  else if (fToken == Token(kSymbol, "macro")) {
    // TODO
  }
  else if (fToken.fType == kSymbol)
    return parseFunctionOrVarDef(isLocal);
  else
    throw UnexpectedTokenException(fToken);

  return Pexpr();
}


Pexpr
FirstPass::parseTop()
{
  if (fToken == Token(kSymbol, "module")) {
    nextToken();
    return parseModule(true);
  }
  else if (fToken == Token(kSymbol, "interface")) {
    nextToken();
    return parseModule(false);
  }
  else if (fToken == Token(kSymbol, "export")) {
    nextToken();
    return parseExport();
  }
  else if (fToken == Token(kSymbol, "import")) {
    nextToken();
    return parseImport();
  }
  else if (fToken == Token(kSymbol, "def")) {
    nextToken();
    return parseDef(false);
  }
  else if (fToken == Token(kSymbol, "when")) {
    nextToken();
    return parseWhen(true);
  }
  else
    throw UnexpectedTokenException(fToken);

  return Pexpr();
}


Pexpr
FirstPass::parse()
{
  Pexpr seq;

  nextToken();
  while (fToken != Token(kEOF)) {
    Pexpr n = parseTop();
    if (n.isSet())
      seq << n;
  }

  return seq;
}


