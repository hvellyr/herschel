/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "pass1.h"
#include "tokenizer.h"


//----------------------------------------------------------------------------

using namespace heather;


FirstPass::FirstPass(Parser* parser, Token currentToken)
  : fParser(parser),
    fToken(currentToken)
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
        if (!n.isEmpty())
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

    if (nested.isEmpty()) {
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


Pexpr
FirstPass::parseOn()
{
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseAnonFun()
{
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseAccess(const Pexpr& expr)
{
  // TODO
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
FirstPass::parseExprListUntilBrace(std::vector<Pexpr>* result)
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


Pexpr
FirstPass::parseBlock()
{
  std::vector<Pexpr> exprlist;
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
      return parseDef(true);
    }
    else if (fToken == Token(kSymbol, "on")) {
      return parseOn();
    }
    else if (fToken == Token(kSymbol, "function")) {
      return parseAnonFun();
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


OperatorType
FirstPass::tokenTypeToOperator(TokenType type) const
{
  switch (type) {
  case kPlus:          return kOpPlus;
  case kMinus:         return kOpMinus;
  case kDivide:        return kOpDivide;
  case kMultiply:      return kOpMultiply;
  case kExponent:      return kOpExponent;
  case kFold:          return kOpFold;
  case kCompare:       return kOpCompare;
  case kEqual:         return kOpEqual;
  case kUnequal:       return kOpUnequal;
  case kLess:          return kOpLess;
  case kLessEqual:     return kOpLessEqual;
  case kGreater:       return kOpGreater;
  case kGreaterEqual:  return kOpGreaterEqual;
  case kAssign:        return kOpAssign;
  case kMapTo:         return kOpMapTo;
  case kIn:            return kOpIn;
  case kMod:           return kOpMod;
  case kIsa:           return kOpIsa;
  case kAs:            return kOpAs;
  case kBy:            return kOpBy;
  case kLogicalAnd:    return kOpLogicalAnd;
  case kLogicalOr:     return kOpLogicalOr;
  case kBitAnd:        return kOpBitAnd;
  case kBitOr:         return kOpBitOr;
  case kBitXor:        return kOpBitXor;
  case kShiftLeft:     return kOpShiftLeft;
  case kShiftRight:    return kOpShiftRight;
  case kRange:         return kOpRange;
  case kEllipsis:      return kOpEllipsis;

  default:
    return kOpInvalid;
  }
}


TokenType
FirstPass::operatorToTokenType(OperatorType op) const
{
  switch (op) {
  case kOpPlus:          return kPlus;
  case kOpMinus:         return kMinus;
  case kOpDivide:        return kDivide;
  case kOpMultiply:      return kMultiply;
  case kOpExponent:      return kExponent;
  case kOpFold:          return kFold;
  case kOpCompare:       return kCompare;
  case kOpEqual:         return kEqual;
  case kOpUnequal:       return kUnequal;
  case kOpLess:          return kLess;
  case kOpLessEqual:     return kLessEqual;
  case kOpGreater:       return kGreater;
  case kOpGreaterEqual:  return kGreaterEqual;
  case kOpAssign:        return kAssign;
  case kOpMapTo:         return kMapTo;
  case kOpIn:            return kIn;
  case kOpMod:           return kMod;
  case kOpIsa:           return kIsa;
  case kOpAs:            return kAs;
  case kOpBy:            return kBy;
  case kOpLogicalAnd:    return kLogicalAnd;
  case kOpLogicalOr:     return kLogicalOr;
  case kOpBitAnd:        return kBitAnd;
  case kOpBitOr:         return kBitOr;
  case kOpBitXor:        return kBitXor;
  case kOpShiftLeft:     return kShiftLeft;
  case kOpShiftRight:    return kShiftRight;
  case kOpRange:         return kRange;
  case kOpEllipsis:      return kEllipsis;

  case kOpInvalid:
    assert(0);
  }

  return kInvalid;
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

  case kOpIn:
  case kOpEqual:
  case kOpUnequal:
  case kOpLess:
  case kOpLessEqual:
  case kOpGreater:
  case kOpGreaterEqual:
  case kOpCompare:        return  40;

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
  if (!expr2.isEmpty()) {
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

  return Pexpr();
}


Pexpr
FirstPass::parseExpr()
{
  Pexpr expr1 = parseAtomicExpr();
  if (!expr1.isEmpty()) {
    OperatorType op1 = tokenTypeToOperator(fToken.fType);
    if (op1 != kOpInvalid)
      return parseExprRec(expr1, op1);
    else
      return expr1;
  }
  else
    throw UnexpectedTokenException(fToken);
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

  vardefExpr << Pexpr(symbolName);

  if (!type.isEmpty())
    vardefExpr << Pexpr(kColon) << type;
  if (!initExpr.isEmpty())
    vardefExpr << Pexpr(kAssign) << initExpr;

  return vardefExpr;
}


void
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

  fParser->charRegistry()->registerChar(charName, codePoint);

  nextToken();
}


Pexpr
FirstPass::parseFunction(const String& sym, bool isGeneric,
                         bool isLocal)
{
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseFunctionOrVar(bool isLocal)
{
  assert(fToken.fType == kSymbol);

  String sym = fToken.fStrValue;

#if 0
  MacroId macroId = qualifiedIdForLookup(sym);
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Pexpr(macroId.name());

  if (macro != NULL) {
    Pexpr expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, scope);
    if (expr.isEmpty())
      throw IncompleteMacroException(macroId);
  }
  else
#endif
  {
    nextToken();
    if (fToken.fType == kParanOpen)
      return parseFunction(sym, false, isLocal);

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
  else if (fToken == Token(kSymbol, "generic")) {
    nextToken();
    if (fToken.fType != kSymbol)
      throw UnexpectedTokenException(fToken, "expected SYMBOL");
    String sym = fToken.fStrValue;

    nextToken();
    if (fToken.fType != kParanOpen)
      throw UnexpectedTokenException(fToken, "expected (");
    return parseFunction(sym, true, isLocal);
  }
  else if (fToken == Token(kSymbol, "char")) {
    nextToken();
    parseCharDef();
  }
  else if (fToken == Token(kSymbol, "macro")) {
    // TODO
  }
  else if (fToken.fType == kSymbol)
    return parseFunctionOrVar(isLocal);
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
    // TODO
  }
  else
    throw UnexpectedTokenException(fToken);

  return NULL;
}


Pexpr
FirstPass::parse()
{
  Pexpr seq;

  nextToken();
  while (fToken != Token(kEOF)) {
    Pexpr n = parseTop();
    if (!n.isEmpty())
      seq << n;
  }

  return seq;
}


