/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/


#include "parser.h"
#include "tokenizer.h"

using namespace heather;

Parser::Parser()
{
}


Token
Parser::nextToken()
{
  try {
    fCurrentToken = fPort->read();
  }
  catch (const EofException& e) {
    fCurrentToken = Token(kEOF);
  }
  return fCurrentToken;
}


AptNode*
Parser::parseModule(bool isModule)
{
  Ptr<ModuleNode> modNode;
  if (fCurrentToken.fType == kSymbol) {
    String modName = fCurrentToken.fStrValue;

    nextToken();
    if (fCurrentToken.fType == kParanOpen) {
      nextToken();

      if (fCurrentToken.fType == kString) {
        String publicId = fCurrentToken.fStrValue;

        nextToken();
        if (fCurrentToken.fType == kParanClose) {
          nextToken();
          modNode = new ModuleNode(modName, publicId, isModule);
        }
        else
          throw UnexpectedTokenException(fCurrentToken, String("expected )"));
      }
      else
        throw UnexpectedTokenException(fCurrentToken, String("expected string"));
    }
    else
      modNode = new ModuleNode(modName, modName, isModule);

    assert(modNode != NULL);
    if (fCurrentToken.fType == kBraceOpen) {
      nextToken();

      fLastModules.push_front(modNode.obj());

      while (fCurrentToken != kBraceClose) {
        if (fCurrentToken == kEOF)
          throw PrematureEndOfFileException();

        Ptr<AptNode> targetNode = !fLastModules.empty()
          ? dynamic_cast<AptNode*>(fLastModules.front().obj())
          : dynamic_cast<AptNode*>(modNode.obj());

        Ptr<AptNode> n = parseTop();
        if (n != NULL) {
          targetNode->appendNode(n);
        }
      }

      if (fCurrentToken.fType == kBraceClose) {
        nextToken();
      }
      else
        throw UnexpectedTokenException(fCurrentToken, String("expected }"));

      fLastModules.pop_front();
    }
    else
      fLastModules.push_front(modNode.obj());
  }

  return modNode.release();
}


AptNode*
Parser::parseTop()
{
  if (fCurrentToken == Token(kSymbol, "module")) {
    nextToken();
    return parseModule(true);
  }
  else if (fCurrentToken == Token(kSymbol, "interface")) {
    nextToken();
    return parseModule(false);
  }
  else if (fCurrentToken == Token(kSymbol, "export")) {
  }
  else if (fCurrentToken == Token(kSymbol, "import")) {
  }
  else if (fCurrentToken == Token(kSymbol, "def")) {
  }
  else if (fCurrentToken == Token(kSymbol, "when")) {
  }
  else
    throw UnexpectedTokenException(fCurrentToken);

  return NULL;
}


AptNode*
Parser::parse(Port<Char>* port)
{
  fPort = new FileTokenPort(port);

  Ptr<CompileUnitNode> rootNode = new CompileUnitNode;

  try {
    nextToken();
    while (fCurrentToken != Token(kEOF)) {
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
