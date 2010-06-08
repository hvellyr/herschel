/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "tokenizer.h"
#include "pass1.h"
#include "pass2.h"
#include "properties.h"

using namespace heather;


//----------------------------------------------------------------------------

const Token Parser::aliasToken     = Token(SrcPos(), kSymbol, "alias");
const Token Parser::charToken      = Token(SrcPos(), kSymbol, "char");
const Token Parser::classToken     = Token(SrcPos(), kSymbol, "class");
const Token Parser::configToken    = Token(SrcPos(), kSymbol, "config");
const Token Parser::constToken     = Token(SrcPos(), kSymbol, "const");
const Token Parser::deleteToken    = Token(SrcPos(), kSymbol, "delete");
const Token Parser::enumToken      = Token(SrcPos(), kSymbol, "enum");
const Token Parser::exitToken      = Token(SrcPos(), kSymbol, "exit");
const Token Parser::fluidToken     = Token(SrcPos(), kSymbol, "fluid");
const Token Parser::genericToken   = Token(SrcPos(), kSymbol, "generic");
const Token Parser::ignoreToken    = Token(SrcPos(), kSymbol, "ignore");
const Token Parser::includeToken   = Token(SrcPos(), kSymbol, "include");
const Token Parser::initToken      = Token(SrcPos(), kSymbol, "init");
const Token Parser::macroToken     = Token(SrcPos(), kSymbol, "macro");
const Token Parser::measureToken   = Token(SrcPos(), kSymbol, "measure");
const Token Parser::signalToken    = Token(SrcPos(), kSymbol, "signal");
const Token Parser::slotToken      = Token(SrcPos(), kSymbol, "slot");
const Token Parser::syncToken      = Token(SrcPos(), kSymbol, "sync");
const Token Parser::typeToken      = Token(SrcPos(), kSymbol, "type");
const Token Parser::unitToken      = Token(SrcPos(), kSymbol, "unit");


//----------------------------------------------------------------------------

Parser::Parser()
  : fCharRegistry(new CharRegistry),
    fConfigVarRegistry(
      new ConfigVarRegistry(Properties::globalConfigVarRegistry()))
{
}


Parser::Parser(Parser* parent)
  : fParent(parent),
    fCharRegistry(parent->charRegistry()),
    fConfigVarRegistry(parent->configVarRegistry())
{
}


CharRegistry*
Parser::charRegistry() const
{
  return fCharRegistry;
}


ConfigVarRegistry*
Parser::configVarRegistry() const
{
  return fConfigVarRegistry;
}


Token
Parser::nextToken()
{
  try {
    fToken = fPort->read();
  }
  catch (const AnnotatedEofException& ae) {
    fToken = Token(ae.srcpos(), kEOF);
  }
  catch (const EofException& e) {
    printf("FOUND EOF HERE: %s %d\n", __FILE__, __LINE__);
    fToken = Token(SrcPos(), kEOF);
  }
  return fToken;
}


AptNode*
Parser::parse(Port<Char>* port, const String& srcName)
{
  fPort = new FileTokenPort(port, srcName, fCharRegistry);

  try {
    FirstPass firstPass(this, fToken);

    Token parsedExprs = firstPass.parse();

    if (Properties::isTracePass1()) {
      Ptr<FilePort> stream = new FilePort(stdout);
      display(stream, "<?xml version='1.0' encoding='utf-8'?>\n");
      parsedExprs.toPort(stream);
      displayln(stream, "");
    }

    SecondPass secondPass(this);

    Ptr<AptNode> apt = secondPass.parse(parsedExprs);
    if (Properties::isTracePass2() && apt != NULL) {
      Ptr<FilePort> stream = new FilePort(stdout);
      display(stream, "<?xml version='1.0' encoding='utf-8'?>\n");
      apt->display(stream);
      displayln(stream, "");
    }

    return apt.release();
  }
  catch (const Exception& e) {
    fprintf(stderr, "Parse error: %s\n", (const char*)StrHelper(e.message()));
  }

  return NULL;
}
