/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "annotate.h"
#include "file.h"
#include "log.h"
#include "parser.h"
#include "pass1.h"
#include "pass2.h"
#include "predefined.h"
#include "properties.h"
#include "rootscope.h"
#include "scope.h"
#include "tokenizer.h"
#include "transform.h"
#include "typify.h"
#include "xmlout.h"

using namespace heather;


//----------------------------------------------------------------------------

const Token Parser::aliasToken     = Token(SrcPos(), kSymbol, "alias");
const Token Parser::autoToken      = Token(SrcPos(), kSymbol, "auto");
const Token Parser::charToken      = Token(SrcPos(), kSymbol, "char");
const Token Parser::classToken     = Token(SrcPos(), kSymbol, "class");
const Token Parser::configToken    = Token(SrcPos(), kSymbol, "config");
const Token Parser::constToken     = Token(SrcPos(), kSymbol, "const");
const Token Parser::deleteToken    = Token(SrcPos(), kSymbol, "delete");
const Token Parser::enumToken      = Token(SrcPos(), kSymbol, "enum");
const Token Parser::exitToken      = Token(SrcPos(), kSymbol, MID_exitKeyword);
const Token Parser::finalToken     = Token(SrcPos(), kSymbol, "final");
const Token Parser::fluidToken     = Token(SrcPos(), kSymbol, "fluid");
const Token Parser::genericToken   = Token(SrcPos(), kSymbol, "generic");
const Token Parser::ignoreToken    = Token(SrcPos(), kSymbol, "ignore");
const Token Parser::includeToken   = Token(SrcPos(), kSymbol, "include");
const Token Parser::initToken      = Token(SrcPos(), kSymbol, "init");
const Token Parser::innerToken     = Token(SrcPos(), kSymbol, "inner");
const Token Parser::macroToken     = Token(SrcPos(), kSymbol, "macro");
const Token Parser::measureToken   = Token(SrcPos(), kSymbol, "measure");
const Token Parser::observableToken = Token(SrcPos(), kSymbol, "observable");
const Token Parser::outerToken     = Token(SrcPos(), kSymbol, "outer");
const Token Parser::publicToken    = Token(SrcPos(), kSymbol, "public");
const Token Parser::readonlyToken  = Token(SrcPos(), kSymbol, "readonly");
const Token Parser::signalToken    = Token(SrcPos(), kSymbol, MID_signalKeyword);
const Token Parser::slotToken      = Token(SrcPos(), kSymbol, "slot");
const Token Parser::syncToken      = Token(SrcPos(), kSymbol, "sync");
const Token Parser::transientToken = Token(SrcPos(), kSymbol, "transient");
const Token Parser::typeToken      = Token(SrcPos(), kSymbol, "type");
const Token Parser::unitToken      = Token(SrcPos(), kSymbol, "unit");


//----------------------------------------------------------------------------



//----------------------------------------------------------------------------

Parser::Parser(bool isParsingInterface)
  : fState(ParserState(
             new CharRegistry,
             new ConfigVarRegistry(Properties::globalConfigVarRegistry()),
             type::newRootScope())),
    fIsParsingInterface(isParsingInterface)
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


Scope*
Parser::scope() const
{
  return fState.fScope;
}


bool
Parser::isParsingInterface() const
{
  return fIsParsingInterface;
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
  return parseImpl(port, srcName, true);
}


AptNode*
Parser::parseImpl(Port<Char>* port, const String& srcName, bool doTrace)
{
  fState.fPort = new FileTokenPort(port, srcName, fState.fCharRegistry);

  assert(fState.fScope != NULL);

  try {
    Ptr<AptNode> apt;
    Token parsedExprs;
    Ptr<TokenCompilePass> tokenPass;
    Ptr<Token2AptNodeCompilePass> t2nPass;
    Ptr<AptNodeCompilePass> nodePass;

    tokenPass = new ExprPass(1, this, fState.fToken, fState.fScope);
    parsedExprs = tokenPass->apply(Token(), doTrace);

    // let all following passes run beneath the same root-scope.
    {
      ScopeHelper scopeHelper(fState.fScope, true, false, kScopeL_CompileUnit);

      Ptr<NodifyPass> nodifyPass = new NodifyPass(2, this, fState.fScope);
      apt = nodifyPass->apply(parsedExprs, doTrace);

      // if the compileunit contains open-ended module declarations
      // (i.e. without {}) get the last valid scope back and make it the
      // current one.  It contains the complete upstream chain of scopes.  (We
      // must not simply export it back to the original fState.fScope, since
      // the symbols may not be exportable at all).
      fState.fScope = nodifyPass->currentScope();

      nodePass = new TransformPass(3);
      apt = nodePass->apply(apt.release(), doTrace);

      nodePass = new AnnotatePass(4, fState.fScope);
      apt = nodePass->apply(apt.release(), doTrace);

      nodePass = new TypifyPass(5);
      apt = nodePass->apply(apt.release(), doTrace);
    }

    return apt.release();
  }
  catch (const Exception& e) {
    logf(kError, "Parse error: %s", (const char*)StrHelper(e.message()));
  }

  return NULL;
}


bool
Parser::importFile(const SrcPos& srcpos,
                   const String& srcName, bool isPublic,
                   Scope* currentScope)
{
  typedef std::map<String, Ptr<Scope> > ImportCache;
  static ImportCache sImportCache;

  String absPath = lookupFile(srcName, isPublic);
  if (absPath.isEmpty()) {
    errorf(srcpos, E_UnknownInputFile,
           "import '%s' failed: Unknown file\n",
           (const char*)StrHelper(srcName));
    return false;
  }

  if (currentScope->hasScopeForFile(absPath)) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "File '%s' already imported", (const char*)StrHelper(absPath));
    return true;
  }

  ImportCache::iterator it = sImportCache.find(absPath);
  if (it != sImportCache.end()) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "Reuse imported '%s'", (const char*)StrHelper(absPath));
    currentScope->addImportedScope(absPath, it->second);
    return true;
  }

  if (Properties::isTraceImportFile())
    logf(kDebug, "Import '%s'", (const char*)StrHelper(srcName));

  try {
    Ptr<Parser> parser = new Parser(true);
    Ptr<AptNode> apt = parser->parseImpl(new CharPort(new FilePort(absPath, "rb")),
                                         srcName, false);
    Ptr<Scope> scope = parser->scope();

    currentScope->addImportedScope(absPath, scope);

    sImportCache.insert(std::make_pair(absPath, scope));
  }
  catch (const Exception& e) {
    errorf(srcpos, E_UnknownInputFile,
           "import '%s' failed: %s\n",
           (const char*)StrHelper(absPath),
           (const char*)StrHelper(e.message()));
    return false;
  }

  return true;
}


String
Parser::lookupFile(const String& srcName, bool isPublic)
{
  StringVector exts;
  exts.push_back(String("hea"));

  return file::lookupInPath(srcName, Properties::inputDirSearchPath(), exts);
}


//==============================================================================

Parser::ParserState::ParserState(CharRegistry* charReg,
                                 ConfigVarRegistry* configReg,
                                 Scope* scope)
  : fCharRegistry(charReg),
    fConfigVarRegistry(configReg),
    fScope(scope)
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
  fScope             = item.fScope;

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
    type::newRootScope());
}


Parser::PortStackHelper::PortStackHelper(Parser* parser, TokenPort* port)
  : fParser(parser),
    fPortOnly(true)
{
  fParser->fParserStates.push_front(fParser->fState);
  fParser->fState = ParserState(
    parser->charRegistry(),
    new ConfigVarRegistry(parser->configVarRegistry()),
    parser->fState.fScope);

  fParser->fState.fPort = port;
}


Parser::PortStackHelper::~PortStackHelper()
{
  assert(!fParser->fParserStates.empty());

  ParserState current = fParser->fState;
  fParser->fState = fParser->fParserStates.front();
  fParser->fParserStates.pop_front();

  if (!fPortOnly) {
    // merge current.fScope into fParser->fState; same for configVarReg and
    // current.fCharRegistry
  }
}
