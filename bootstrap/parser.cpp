/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "tokenizer.h"

using namespace heather;

Parser::Parser()
  : fCharRegistry(new CharRegistry)
{
}


Parser::Parser(Parser* parent)
  : fParent(parent),
    fCharRegistry(parent->charRegistry())
{
}


CharRegistry*
Parser::charRegistry() const
{
  return fCharRegistry;
}


Token
Parser::nextToken()
{
  try {
    fToken = fPort->read();
  }
  catch (const EofException& e) {
    fToken = Token(kEOF);
  }
  return fToken;
}


AptNode*
Parser::parseModule(bool isModule)
{
  Ptr<ModuleNode> modNode;
  if (fToken.fType == kSymbol) {
    String modName = fToken.fStrValue;

    nextToken();
    if (fToken.fType == kParanOpen) {
      nextToken();

      if (fToken.fType == kString) {
        String publicId = fToken.fStrValue;

        nextToken();
        if (fToken.fType == kParanClose) {
          nextToken();
          modNode = new ModuleNode(modName, publicId, isModule);
        }
        else
          throw UnexpectedTokenException(fToken, String("expected )"));
      }
      else
        throw UnexpectedTokenException(fToken, String("expected string"));
    }
    else
      modNode = new ModuleNode(modName, modName, isModule);

    assert(modNode != NULL);
    if (fToken.fType == kBraceOpen) {
      nextToken();

      fLastModules.push_front(modNode.obj());

      while (fToken != kBraceClose) {
        if (fToken == kEOF)
          throw PrematureEndOfFileException();

        Ptr<AptNode> targetNode = !fLastModules.empty()
          ? dynamic_cast<AptNode*>(fLastModules.front().obj())
          : dynamic_cast<AptNode*>(modNode.obj());

        Ptr<AptNode> n = parseTop();
        if (n != NULL) {
          targetNode->appendNode(n);
        }
      }

      if (fToken.fType == kBraceClose) {
        nextToken();
      }
      else
        throw UnexpectedTokenException(fToken, String("expected }"));

      fLastModules.pop_front();
    }
    else
      fLastModules.push_front(modNode.obj());
  }

  return modNode.release();
}


AptNode*
Parser::parseExport()
{
  StringList flags;
  StringList symbols;

  while (fToken.fType == kSymbol) {
    flags.push_back(fToken.fStrValue);
    nextToken();
  }

  if (fToken.fType != kParanOpen)
    throw UnexpectedTokenException(fToken, String("expected ("));
  nextToken();

  while (fToken.fType != kParanClose) {
    if (fToken.fType == kEOF)
      throw PrematureEndOfFileException();

    if (fToken.fType == kSymbol)
      flags.push_back(fToken.fStrValue);
    else if (fToken.fType == kMultiply)
      flags.push_back(String("*"));
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

  return new ExportNode(flags, symbols);
}


AptNode*
Parser::parseImport()
{
  if (fToken.fType != kString)
    throw UnexpectedTokenException(fToken, String("expected STRING"));

  String codeFile = fToken.fStrValue;
  std::map<String, String> renames;

  nextToken();
  if (fToken.fType == kParanOpen) {
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

      renames.insert(std::make_pair(first, second));

      nextToken();
      if (fToken.fType == kComma)
        nextToken();
      else if (fToken.fType != kParanClose)
        throw UnexpectedTokenException(fToken, String("expected ) or ,"));
    }

    if (fToken.fType == kParanClose)
      nextToken();
  }

  return new ImportNode(codeFile, renames);
}


AptNode*
Parser::parseCharDef()
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

  fCharRegistry->registerChar(charName, codePoint);

  nextToken();
  return NULL;
}


AptNode*
Parser::parseDef()
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
    // TODO
  }
  else if (fToken == Token(kSymbol, "fluid")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "generic")) {
    // TODO
  }
  else if (fToken == Token(kSymbol, "char")) {
    nextToken();
    return parseCharDef();
  }
  else if (fToken == Token(kSymbol, "macro")) {
    // TODO
  }
  else {
    // lookup macro
    throw UndefinedSymbolException(fToken.fStrValue);
  }

  return NULL;
}


AptNode*
Parser::parseTop()
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


AptNode*
Parser::parse(Port<Char>* port)
{
  fPort = new FileTokenPort(port, fCharRegistry);

  Ptr<CompileUnitNode> rootNode = new CompileUnitNode;

  try {
    nextToken();
    while (fToken != Token(kEOF)) {
      Ptr<AptNode> targetNode = !fLastModules.empty()
        ? dynamic_cast<AptNode*>(fLastModules.front().obj())
        : dynamic_cast<AptNode*>(rootNode.obj());

      Ptr<AptNode> n = parseTop();
      if (n != NULL)
        targetNode->appendNode(n);
    }
  }
  catch (const Exception& e) {
  }

  return rootNode.release();
}
