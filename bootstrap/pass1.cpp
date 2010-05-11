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


FirstPass::FirstPass(Parser* parser, const Pexpr& currentToken)
  : fParser(parser),
    fToken(currentToken),
    fEvaluateExprs(true)
{ }


Pexpr
FirstPass::nextToken()
{
  fToken = fParser->nextToken();
  return fToken;
}


Pexpr
FirstPass::parseModule(bool isModule)
{
  Pexpr modExpr;

  if (fToken.tokenType() == kSymbol) {
    String modName = fToken.stringLitValue();

    nextToken();
    if (fToken.tokenType() == kParanOpen) {
      nextToken();

      if (fToken.tokenType() == kString) {
        Pexpr publicId = fToken;

        nextToken();
        if (fToken.tokenType() == kParanClose) {
          nextToken();

          modExpr = Pexpr()
            << Pexpr(isModule ? "module" : "interface")
            << Pexpr(modName)
            << ( Pexpr(kParanOpen, kParanClose)
                 << publicId );
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


    if (fToken.tokenType() == kBraceOpen) {
      nextToken();

      Pexpr defines = Pexpr(kBraceOpen, kBraceClose);
      while (fToken != kBraceClose) {
        if (fToken == kEOF)
          throw PrematureEndOfFileException();

        Pexpr n = parseTop();
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


Pexpr
FirstPass::parseExport()
{
  Pexpr expr;

  expr << Pexpr("export");

  while (fToken.tokenType() == kSymbol) {
    expr << fToken;
    nextToken();
  }

  if (fToken.tokenType() != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");

  if (fToken.tokenType() == kParanOpen) {
    Pexpr symbols = Pexpr(kParanOpen, kParanClose);

    nextToken();
    while (fToken.tokenType() != kParanClose) {
      if (fToken.tokenType() == kEOF)
        throw PrematureEndOfFileException();

      if (fToken.tokenType() == kSymbol)
        symbols << fToken;
      else if (fToken.tokenType() == kMultiply)
        symbols << Pexpr("*");
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


Pexpr
FirstPass::parseImport()
{
  if (fToken.tokenType() != kString)
    throw UnexpectedTokenException(fToken, "expected STRING");

  Pexpr expr;
  expr << Pexpr("import");
  expr << Pexpr(fToken);

  nextToken();
  if (fToken.tokenType() == kParanOpen) {
    Pexpr renames = Pexpr(kParanOpen, kParanClose);

    nextToken();
    while (fToken.tokenType() != kParanClose) {
      if (fToken.tokenType() == kEOF)
        throw PrematureEndOfFileException();
      if (fToken.tokenType() != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      Pexpr first = fToken;

      nextToken();
      if (fToken.tokenType() != kMapTo)
        throw UnexpectedTokenException(fToken, "expected ->");

      nextToken();
      if (fToken.tokenType() != kSymbol)
        throw UnexpectedTokenException(fToken, "expected SYMBOL");
      Pexpr second = fToken;

      renames << ( Pexpr() << first << Pexpr(kMapTo) << second );

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

  while (fToken.tokenType() != kParanClose) {
    if (fToken.tokenType() == kEOF)
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

    if (fToken.tokenType() == kComma)
      nextToken();
    else if (fToken.tokenType() != kParanClose)
      throw UnexpectedTokenException(fToken, "expected ] or ,");
  }

  if (fToken.tokenType() == kParanClose)
    nextToken();

  return nested;
}


Pexpr
FirstPass::parseLiteralArray()
{
  Pexpr nested = Pexpr(kLiteralArrayOpen, kBracketClose);

  while (fToken.tokenType() != kBracketClose) {
    if (fToken.tokenType() == kEOF)
      throw PrematureEndOfFileException();

    Pexpr n = parseExpr();
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


Pexpr
FirstPass::parseIf()
{
  if (fToken.tokenType() != kParanOpen)
    throw UnexpectedTokenException(fToken, "expected (");
  nextToken();

  Pexpr test = parseExpr();
  if (fToken.tokenType() != kParanClose)
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

  if (fToken.isId("else")) {
    nextToken();

    Pexpr alternate = parseExpr();

    result << Pexpr("else") << alternate;
  }

  return result;
}


void
FirstPass::parseFunctionsParams(PexprVector* exprlist)
{
  if (fToken.tokenType() == kParanClose) {
    nextToken();
    return;
  }
}


Pexpr
FirstPass::parseOn()
{
  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected a SYMBOL");

  String key = fToken.idValue();

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
      throw UnexpectedTokenException(Pexpr(kSymbol, key));
    nextToken();
    if (fToken.tokenType() != kParanOpen)
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
  while (fToken.tokenType() != kParanClose) {
    if (fToken.tokenType() == kEOF)
      throw PrematureEndOfFileException();
    if (fToken.isKeyArgLit()) {
      String key = fToken.idValue();
      nextToken();

      Pexpr val = parseExpr();
      params->push_back(key);
      params->push_back(val);
    }
    else {
      Pexpr val = parseExpr();
      params->push_back(val);
    }

    if (fToken.tokenType() == kComma) {
      params->push_back(Pexpr(kComma));
      nextToken();
    }
    else if (fToken.tokenType() != kParanClose)
      throw UnexpectedTokenException(fToken, "expected ) or ,");
  }

  if (fToken.tokenType() == kParanClose)
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
  if (fToken.tokenType() != kBracketClose)
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


Pexpr
FirstPass::parseGroup()
{
  Pexpr expr = parseExpr();
  if (fToken.tokenType() != kParanClose)
    throw UnexpectedTokenException(fToken, "expected )");

  nextToken();
  return expr;
}


void
FirstPass::parseExprListUntilBrace(PexprVector* result)
{
  for ( ; ; ) {
    if (fToken.isId("def")) {
      nextToken();
      Pexpr expr = parseDef(false);
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
      Pexpr expr = parseExpr();
      result->push_back(expr);
    }
  }
}


void
FirstPass::parseTopExprUntilBrace(PexprVector* result)
{
  while (fToken.tokenType() != kBraceClose) {
    if (fToken.tokenType() == kEOF)
      throw PrematureEndOfFileException();
    Pexpr topexpr = parseTop();
    result->push_back(topexpr);
  }

  if (fToken.tokenType() == kBraceClose)
    nextToken();
}


Pexpr
FirstPass::parseBlock()
{
  PexprVector exprlist;
  parseExprListUntilBrace(&exprlist);

  if (fToken.tokenType() != kBraceClose)
    throw UnexpectedTokenException(fToken, "expected }");
  nextToken();

  return Pexpr(kBraceOpen, kBraceClose) << exprlist;
}


Pexpr
FirstPass::parseAtomicExpr()
{
  switch (fToken.tokenType()) {
  case kBool:
  case kInteger:
  case kReal:
  case kRational:
  case kString:
  case kChar:
  case kKeyword:
    {
      Pexpr t = fToken;
      nextToken();
      return t;
    }

  case kSymbol:
    if (fToken.isId("if")) {
      nextToken();
      return parseIf();
    }
    else if (fToken.isId("let")) {
      nextToken();
      return parseDef(true);
    }
    else if (fToken.isId("on")) {
      nextToken();
      return parseOn();
    }
    else if (fToken.isId("function")) {
      nextToken();
      return parseAnonFun();
    }
    else if (fToken.isId("when")) {
      nextToken();
      return parseWhen(false);
    }
    else {
      Pexpr t = fToken;
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
  OperatorType op2 = tokenTypeToOperator(fToken.tokenType());
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
  OperatorType op1 = tokenTypeToOperator(fToken.tokenType());
  if (op1 != kOpInvalid)
    return parseExprRec(expr1, op1);
  return expr1;
}


Pexpr
FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken.tokenType() == kBraceOpen) {
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

  if (fToken.isId()) {
    if (fToken.isId("ignore")) {
      nextToken();
      inclConsequent = false;
    }
    else if (fToken.isId("include")) {
      nextToken();
      inclAlternate = false;
    }
    else
      throw UnexpectedTokenException(fToken, "expected 'ignore' or 'include'");
  }
  else if (fToken.tokenType() == kParanOpen) {
    nextToken();
    Pexpr test = parseExpr();
    if (fToken.tokenType() != kParanClose)
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

  if (fToken.isId("else")) {
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
  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  String symbolName = fToken.idValue();

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
  if (fToken.tokenType() == kColon) {
    nextToken();
    type = parseTypeSpec();
  }

  Pexpr initExpr;
  if (fToken.tokenType() == kAssign) {
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
  if (fToken.tokenType() != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  String charName = fToken.idValue();

  nextToken();
  if (fToken.tokenType() != kAssign)
    throw UnexpectedTokenException(fToken, "expected =");

  nextToken();
  if (fToken.tokenType() != kInteger)
    throw UnexpectedTokenException(fToken, "expected INTEGER");

  int codePoint = fToken.intLitValue();
  if (codePoint < 0 || codePoint > 0x10FFFF)
    throw SyntaxException(String("invalid expected INTEGER"));

  nextToken();

  if (fEvaluateExprs) {
    fParser->charRegistry()->registerValue(charName, codePoint);
    return Pexpr();
  }
  else
    return Pexpr() << Pexpr("def") << Pexpr("char") << Pexpr(charName)
                   << Pexpr(kAssign) << Pexpr(kInteger, codePoint);
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
  assert(fToken.tokenType() == kSymbol);

  String sym = fToken.idValue();

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
    if (fToken.tokenType() == kParanOpen)
      return parseFunctionDef(sym, false, isLocal);

    return parseVarDef2(sym, kNoFlags, isLocal);
  }
  return Pexpr();
}


Pexpr
FirstPass::parseDef(bool isLocal)
{
  if (fToken.isId("type")) {
    // TODO
  }
  else if (fToken.isId("alias")) {
    // TODO
  }
  else if (fToken.isId("class")) {
    // TODO
  }
  else if (fToken.isId("enum")) {
    // TODO
  }
  else if (fToken.isId("measure")) {
    // TODO
  }
  else if (fToken.isId("unit")) {
    // TODO
  }
  else if (fToken.isId("const")) {
    nextToken();
    return parseVarDef(kIsConst, isLocal);
  }
  else if (fToken.isId("fluid")) {
    nextToken();
    return parseVarDef(kIsFluid, false);
  }
  else if (fToken.isId("config")) {
    nextToken();
    return parseVarDef(kIsConfig, false);
  }
  else if (fToken.isId("generic")) {
    nextToken();
    if (fToken.tokenType() != kSymbol)
      throw UnexpectedTokenException(fToken, "expected SYMBOL");
    String sym = fToken.idValue();

    nextToken();
    if (fToken.tokenType() != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");
    return parseFunctionDef(sym, true, isLocal);
  }
  else if (fToken.isId("char")) {
    nextToken();
    return parseCharDef();
  }
  else if (fToken.isId("macro")) {
    // TODO
  }
  else if (fToken.tokenType() == kSymbol)
    return parseFunctionOrVarDef(isLocal);
  else
    throw UnexpectedTokenException(fToken);

  return Pexpr();
}


Pexpr
FirstPass::parseTop()
{
  if (fToken.isId("module")) {
    nextToken();
    return parseModule(true);
  }
  else if (fToken.isId("interface")) {
    nextToken();
    return parseModule(false);
  }
  else if (fToken.isId("export")) {
    nextToken();
    return parseExport();
  }
  else if (fToken.isId("import")) {
    nextToken();
    return parseImport();
  }
  else if (fToken.isId("def")) {
    nextToken();
    return parseDef(false);
  }
  else if (fToken.isId("when")) {
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
  while (fToken.tokenType() != kEOF) {
    Pexpr n = parseTop();
    if (n.isSet())
      seq << n;
  }

  return seq;
}


