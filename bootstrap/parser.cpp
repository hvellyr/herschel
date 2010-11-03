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
const Token Parser::observableToken = Token(SrcPos(), kSymbol, "observable");
const Token Parser::outerToken     = Token(SrcPos(), kSymbol, "outer");
const Token Parser::publicToken    = Token(SrcPos(), kSymbol, "public");
const Token Parser::readonlyToken  = Token(SrcPos(), kSymbol, "readonly");
const Token Parser::signalToken    = Token(SrcPos(), kSymbol, "signal");
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


Token
Parser::doPass1Parse(bool doTrace)
{
  Ptr<FirstPass> firstPass = new FirstPass(this, fState.fToken, fState.fScope);

  Token parsedExprs = firstPass->parse();

  if (doTrace && Properties::isTracePass1()) {
    Ptr<FilePort> stream = new FilePort(stdout);
    display(stream, "<?xml version='1.0' encoding='utf-8'?>\n");
    parsedExprs.toPort(stream);
    displayln(stream, "");
  }

  // fState.fScope->dumpDebug();

  return parsedExprs;
}


AptNode*
Parser::doPass2Parse(const Token& parsedExprs, bool doTrace)
{
  bool doPass2 = true;
#if defined(UNITTESTS)
  doPass2 = Properties::test_passLevel() > 1;
#endif

  if (doPass2) {
    Ptr<SecondPass> secondPass = new SecondPass(this, fState.fScope);

    Ptr<AptNode> apt = secondPass->parse(parsedExprs);
    if (doTrace && Properties::isTracePass2() && apt != NULL) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout));
      out->render(apt);
    }

    // fState.fScope->dumpDebug();

    return apt.release();
  }

  return NULL;
}


AptNode*
Parser::transform(AptNode* node, bool doTrace)
{
  Ptr<AptNode> n = node;
  bool doPass3 = true;
#if defined(UNITTESTS)
  doPass3 = Properties::test_passLevel() > 2;
#endif

  if (doPass3) {
    Ptr<Transformator> pTr = new Transformator;

    pTr->transformNode(n);

    if (doTrace && Properties::isTraceTransform() && n != NULL) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout));
      out->render(n);
    }
  }

  return n.release();
}


AptNode*
Parser::annotate(AptNode* node, bool doTrace)
{
  Ptr<AptNode> n = node;
  bool doPass4 = true;
#if defined(UNITTESTS)
  doPass4 = Properties::test_passLevel() > 3;
#endif

  if (doPass4) {
    Ptr<Annotator> pAn = new Annotator;

    pAn->annotateRecursively(n);

    if (doTrace && Properties::isTraceAnnotate() && n != NULL) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout));
      out->render(n);
    }
  }

  return n.release();
}


AptNode*
Parser::typify(AptNode* node, bool doTrace)
{
  Ptr<AptNode> n = node;
  bool doPass5 = true;
#if defined(UNITTESTS)
  doPass5 = Properties::test_passLevel() > 4;
#endif

  if (doPass5) {
    Ptr<Typifier> pTy = new Typifier;

    pTy->typifyRecursively(n);

    if (doTrace && Properties::isTraceTypify() && n != NULL) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout), true);
      out->render(n);
    }
  }

  return n.release();
}


AptNode*
Parser::parseImpl(Port<Char>* port, const String& srcName,
                  bool doTrace)
{
  fState.fPort = new FileTokenPort(port, srcName, fState.fCharRegistry);

  assert(fState.fScope != NULL);

  try {
    Token parsedExprs = doPass1Parse(doTrace);
    Ptr<AptNode> apt  = doPass2Parse(parsedExprs, doTrace);
    Ptr<AptNode> apt2 = transform(apt.release(), doTrace);
    Ptr<AptNode> apt3 = annotate(apt2.release(), doTrace);
    Ptr<AptNode> apt4 = typify(apt3.release(), doTrace);
    return apt4.release();
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
