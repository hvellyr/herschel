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
#include "log.h"

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
const Token Parser::finalToken     = Token(SrcPos(), kSymbol, "final");
const Token Parser::fluidToken     = Token(SrcPos(), kSymbol, "fluid");
const Token Parser::genericToken   = Token(SrcPos(), kSymbol, "generic");
const Token Parser::ignoreToken    = Token(SrcPos(), kSymbol, "ignore");
const Token Parser::includeToken   = Token(SrcPos(), kSymbol, "include");
const Token Parser::initToken      = Token(SrcPos(), kSymbol, "init");
const Token Parser::innerToken     = Token(SrcPos(), kSymbol, "inner");
const Token Parser::macroToken     = Token(SrcPos(), kSymbol, "macro");
const Token Parser::measureToken   = Token(SrcPos(), kSymbol, "measure");
const Token Parser::outerToken     = Token(SrcPos(), kSymbol, "outer");
const Token Parser::publicToken    = Token(SrcPos(), kSymbol, "public");
const Token Parser::signalToken    = Token(SrcPos(), kSymbol, "signal");
const Token Parser::slotToken      = Token(SrcPos(), kSymbol, "slot");
const Token Parser::syncToken      = Token(SrcPos(), kSymbol, "sync");
const Token Parser::typeToken      = Token(SrcPos(), kSymbol, "type");
const Token Parser::unitToken      = Token(SrcPos(), kSymbol, "unit");


//----------------------------------------------------------------------------

Parser::Parser()
  : fState(ParserState(
             new CharRegistry,
             new ConfigVarRegistry(Properties::globalConfigVarRegistry()),
             new MacroRegistry))
{
}


CharRegistry*
Parser::charRegistry() const
{
  return fState.fCharRegistry;
}


ConfigVarRegistry*
Parser::configVarRegistry() const
{
  return fState.fConfigVarRegistry;
}


MacroRegistry*
Parser::macroRegistry() const
{
  return fState.fMacroRegistry;
}


Token
Parser::nextToken()
{
  try {
    fState.fToken = fState.fPort->read();
  }
  catch (const AnnotatedEofException& ae) {
    fState.fToken = Token(ae.srcpos(), kEOF);
  }
  catch (const EofException& e) {
    fState.fToken = Token(SrcPos(), kEOF);
  }
  return fState.fToken;
}


void
Parser::unreadToken(const Token& token)
{
  fState.fPort->unread(token);
}


AptNode*
Parser::parse(Port<Char>* port, const String& srcName)
{
  fState.fPort = new FileTokenPort(port, srcName, fState.fCharRegistry);

  try {
    FirstPass firstPass(this, fState.fToken);

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
    logf(kError, "Parse error: %s", (const char*)StrHelper(e.message()));
  }

  return NULL;
}


Token
Parser::importFile(Port<Char>* port, const String& srcName)
{
  PortStackHelper helper(this);

  fState.fPort = new FileTokenPort(port, srcName, fState.fCharRegistry);

  try {
    FirstPass firstPass(this, fState.fToken);

    Token parsedExprs = firstPass.parse();

    if (Properties::isTraceImportFile())
      logf(kDebug, "Import file '%s'", (const char*)StrHelper(srcName));

    return parsedExprs;
  }
  catch (const Exception& e) {
    logf(kError, "Parse error: %s", (const char*)StrHelper(e.message()));
  }

  return Token();
}


String
Parser::lookupFile(const String& srcName, bool isPublic)
{
  return srcName;
}


Port<Char>*
Parser::lookupFileAndOpen(const String& srcName, bool isPublic)
{
  String absPath = lookupFile(srcName, isPublic);
  return new CharPort(new FilePort(absPath, "rb"));
}


//==============================================================================

Parser::ParserState::ParserState(CharRegistry* charReg,
                                 ConfigVarRegistry* configReg,
                                 MacroRegistry* macroReg)
  : fCharRegistry(charReg),
    fConfigVarRegistry(configReg),
    fMacroRegistry(macroReg)
{
}


Parser::ParserState::ParserState(const ParserState& item)
{
  *this = item;
}


Parser::ParserState&
Parser::ParserState::operator=(const ParserState& item)
{
  fPort              = item.fPort;
  fToken             = item.fToken;
  fCharRegistry      = item.fCharRegistry;
  fConfigVarRegistry = item.fConfigVarRegistry;
  fMacroRegistry     = item.fMacroRegistry;
  return *this;
}


//==============================================================================

Parser::PortStackHelper::PortStackHelper(Parser* parser)
  : fParser(parser),
    fPortOnly(false)
{
  fParser->fParserStates.push_front(fParser->fState);
  fParser->fState = ParserState(
    new CharRegistry,
    new ConfigVarRegistry(Properties::globalConfigVarRegistry()),
    new MacroRegistry);
}


Parser::PortStackHelper::PortStackHelper(Parser* parser, TokenPort* port)
  : fParser(parser),
    fPortOnly(true)
{
  fParser->fParserStates.push_front(fParser->fState);
  fParser->fState = ParserState(
    parser->charRegistry(),
    new ConfigVarRegistry(parser->configVarRegistry()),
    parser->macroRegistry());

  fParser->fState.fPort = port;
}


Parser::PortStackHelper::~PortStackHelper()
{
  assert(!fParser->fParserStates.empty());

  ParserState current = fParser->fState;
  fParser->fState = fParser->fParserStates.front();
  fParser->fParserStates.pop_front();

  if (!fPortOnly) {
    // merge current.fCharRegistry into fParser->fState; same for
    // configVarReg and fMacroRegistry
  }
}
