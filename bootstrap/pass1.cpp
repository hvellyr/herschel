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
          throw UnexpectedTokenException(fToken, String("expected )"));
      }
      else
        throw UnexpectedTokenException(fToken, String("expected string"));
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
        throw UnexpectedTokenException(fToken, String("expected }"));

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
    throw UnexpectedTokenException(fToken, String("expected ("));

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
        throw UnexpectedTokenException(fToken, String("expected SYMBOL or *"));
      nextToken();

      if (fToken.fType == kComma)
        nextToken();
      else if (fToken.fType != kParanClose)
        throw UnexpectedTokenException(fToken, String("expected ) or ,"));
    }

    if (fToken.fType == kParanClose)
      nextToken();

    expr << symbols;
  }
  else
    throw UnexpectedTokenException(fToken, String("expected ("));

  return expr;
}


Pexpr
FirstPass::parseImport()
{
  if (fToken.fType != kString)
    throw UnexpectedTokenException(fToken, String("expected STRING"));

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
        throw UnexpectedTokenException(fToken, String("expected SYMBOL"));
      String first = fToken.fStrValue;

      nextToken();
      if (fToken.fType != kMapTo)
        throw UnexpectedTokenException(fToken, String("expected ->"));

      nextToken();
      if (fToken.fType != kSymbol)
        throw UnexpectedTokenException(fToken, String("expected SYMBOL"));
      String second = fToken.fStrValue;

      renames << ( Pexpr() << Pexpr(first)
                   << Pexpr(kMapTo)
                   << Pexpr(second) );

      nextToken();
      if (fToken.fType == kComma)
        nextToken();
      else if (fToken.fType != kParanClose)
        throw UnexpectedTokenException(fToken, String("expected ) or ,"));
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
      throw UnexpectedTokenException(fToken, String("expected ] or ,"));
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
      throw UnexpectedTokenException(fToken, String("expected ] or ,"));
  }

  if (fToken.fType == kBracketClose)
    nextToken();

  return nested;
}


Pexpr
FirstPass::parseIf()
{
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseOn()
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
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseBlock()
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
      return parseIf();
    }
    else if (fToken == Token(kSymbol, "let")) {
      return parseDef(false);
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
    return parseAccess(parseGroup());
  case kBraceOpen:
    return parseAccess(parseBlock());
    
  default:
    assert(0);
  }

  return Pexpr();
}


TokenType
FirstPass::mapOperator(const Token& token) const
{
  switch (token.fType) {
  case kPlus:         case kMinus:     case kDivide:    case kMultiply:
  case kExponent:     case kFold:      case kCompare:   case kEqual:
  case kUnequal:      case kLess:      case kLessEqual: case kGreater:
  case kGreaterEqual: case kAssign:    case kMapTo:     case kIn:
  case kMod:          case kIsa:       case kAs:        case kBy:
  case kLogicalAnd:   case kLogicalOr: case kBitAnd:    case kBitOr:
  case kBitXor:       case kShiftLeft: case kShiftRight: case kRange:
    return token.fType;
  default:
    return kInvalid;
  }
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
FirstPass::makeBinaryPexpr(const Pexpr& expr1, TokenType op1,
                           const Pexpr& expr2) const
{
  if (op1 == kAssign)
    return makeAssignPexpr(expr1, expr2);
  else
    return Pexpr() << expr1 << Pexpr(op1) << expr2;
}


bool
FirstPass::isRightOperator(TokenType op1) const
{
  return (op1 == kAssign);
}


int
FirstPass::weightOperator(TokenType op1) const
{
  switch (op1) {
  case kFold: case kMapTo: case kBy: case kIn: return  1;
  case kRange: case kEllipsis:                 return  2;
  case kLogicalAnd: case kLogicalOr:           return  3;
  case kBitAnd: case kBitOr: case kBitXor:     return  4;
  case kEqual: case kUnequal: case kLess:
  case kLessEqual: case kGreater:
  case kGreaterEqual: case kCompare:           return  5;
  case kExponent: 
  case kShiftLeft: case kShiftRight:           return  8;
  case kPlus: case kMinus:                     return  9;
  case kMultiply: case kDivide: case kMod:     return 10;
  case kDot:                                   return 20;
  default:
    assert(0);
    return 999999;
  }
}


bool
FirstPass::isOpWeightAbove(TokenType op1, TokenType op2) const
{
  return weightOperator(op1) > weightOperator(op2);
}


Pexpr
FirstPass::parseExprRec(const Pexpr& expr1, TokenType op1)
{
  if (op1 == kInvalid)
    return expr1;

  nextToken();
  Pexpr expr2 = parseAtomicExpr();
  if (!expr2.isEmpty()) {
    TokenType op2 = mapOperator(fToken);
    if (op2 == kInvalid) {
      if (op1 == kAssign)
        return makeAssignPexpr(expr1, expr2);
      else
        return Pexpr() << expr1 << Pexpr(op1) << expr2;
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
    TokenType op1 = mapOperator(fToken);
    if (op1 != kInvalid)
      return parseExprRec(expr1, op1);
    else
      return expr1;
  }
  else
    return Pexpr();
  // throw UnexpectedTokenException(fToken);
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
      throw UnexpectedTokenException(fToken, String("expected SYMBOL"));
    String sym = fToken.fStrValue;

    nextToken();
    if (fToken.fType != kParanOpen)
      throw UnexpectedTokenException(fToken, String("expected ("));
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


