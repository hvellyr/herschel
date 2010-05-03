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
FirstPass::parseAtomicExpr()
{
  // TODO
  return Pexpr();
}


Pexpr
FirstPass::parseExpr()
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
    // id expression
    return Pexpr(fToken.fStrValue);

  case kLiteralVectorOpen:
    nextToken();
    return parseLiteralVector();

  case kLiteralArrayOpen:
    nextToken();
    return parseLiteralArray();

  default:
    assert(0);
  }

  return Pexpr();
}


Pexpr
FirstPass::parseVarDef(VardefFlags flags)
{
  if (fToken.fType != kSymbol)
    throw UnexpectedTokenException(fToken, "expected SYMBOL");
  String symbolName = fToken.fStrValue;

  nextToken();

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
  vardefExpr << Pexpr("def");
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
FirstPass::parseDef()
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
    return parseVarDef(kIsConst);
  }
  else if (fToken == Token(kSymbol, "fluid")) {
    nextToken();
    return parseVarDef(kIsFluid);
  }
  else if (fToken == Token(kSymbol, "generic")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "char")) {
    nextToken();
    parseCharDef();
  }
  else if (fToken == Token(kSymbol, "macro")) {
    // TODO
  }
  else {
    // variable, function, or lookup macro
    throw UndefinedSymbolException(fToken.fStrValue);
  }

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

    return parseDef();
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


