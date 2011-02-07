/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "annotate.h"
#include "apt.h"
#include "codegen.h"
#include "compiler.h"
#include "file.h"
#include "log.h"
#include "pass1.h"
#include "pass2.h"
#include "predefined.h"
#include "properties.h"
#include "ptr.h"
#include "rootscope.h"
#include "scope.h"
#include "str.h"
#include "tokenizer.h"
#include "transform.h"
#include "typify.h"
#include "filetool.h"

using namespace herschel;


//----------------------------------------------------------------------------

const Token Compiler::aliasToken     = Token(SrcPos(), kSymbol, "alias");
const Token Compiler::autoToken      = Token(SrcPos(), kSymbol, "auto");
const Token Compiler::charToken      = Token(SrcPos(), kSymbol, "char");
const Token Compiler::classToken     = Token(SrcPos(), kSymbol, "class");
const Token Compiler::configToken    = Token(SrcPos(), kSymbol, "config");
const Token Compiler::constToken     = Token(SrcPos(), kSymbol, "const");
const Token Compiler::deleteToken    = Token(SrcPos(), kSymbol, "delete");
const Token Compiler::enumToken      = Token(SrcPos(), kSymbol, "enum");
const Token Compiler::exitToken      = Token(SrcPos(), kSymbol, MID_exitKeyword);
const Token Compiler::finalToken     = Token(SrcPos(), kSymbol, "final");
const Token Compiler::fluidToken     = Token(SrcPos(), kSymbol, "fluid");
const Token Compiler::genericToken   = Token(SrcPos(), kSymbol, "generic");
const Token Compiler::ignoreToken    = Token(SrcPos(), kSymbol, "ignore");
const Token Compiler::includeToken   = Token(SrcPos(), kSymbol, "include");
const Token Compiler::initToken      = Token(SrcPos(), kSymbol, "init");
const Token Compiler::innerToken     = Token(SrcPos(), kSymbol, "inner");
const Token Compiler::macroToken     = Token(SrcPos(), kSymbol, "macro");
const Token Compiler::measureToken   = Token(SrcPos(), kSymbol, "measure");
const Token Compiler::observableToken = Token(SrcPos(), kSymbol, "observable");
const Token Compiler::outerToken     = Token(SrcPos(), kSymbol, "outer");
const Token Compiler::publicToken    = Token(SrcPos(), kSymbol, "public");
const Token Compiler::readonlyToken  = Token(SrcPos(), kSymbol, "readonly");
const Token Compiler::signalToken    = Token(SrcPos(), kSymbol, MID_signalKeyword);
const Token Compiler::slotToken      = Token(SrcPos(), kSymbol, "slot");
const Token Compiler::syncToken      = Token(SrcPos(), kSymbol, "sync");
const Token Compiler::transientToken = Token(SrcPos(), kSymbol, "transient");
const Token Compiler::typeToken      = Token(SrcPos(), kSymbol, "type");
const Token Compiler::unitToken      = Token(SrcPos(), kSymbol, "unit");


//----------------------------------------------------------------------------



//----------------------------------------------------------------------------

Compiler::Compiler(bool isParsingInterface)
  : fState(CompilerState(
             new CharRegistry,
             new ConfigVarRegistry(Properties::globalConfigVarRegistry()),
             type::newRootScope())),
    fIsParsingInterface(isParsingInterface)
{
}

CharRegistry*
Compiler::charRegistry() const
{
  return fState.fCharRegistry;
}


ConfigVarRegistry*
Compiler::configVarRegistry() const
{
  return fState.fConfigVarRegistry;
}


Scope*
Compiler::scope() const
{
  return fState.fScope;
}


bool
Compiler::isParsingInterface() const
{
  return fIsParsingInterface;
}


Token
Compiler::nextToken()
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
Compiler::unreadToken(const Token& token)
{
  fState.fPort->unread(token);
}


AptNode*
Compiler::process(Port<Char>* port, const String& srcName)
{
  fState.fScope = new Scope(kScopeL_CompileUnit, fState.fScope);
  importSystemHeaders(srcName);
  return processImpl(port, srcName, true);
}


AptNode*
Compiler::processImpl(Port<Char>* port, const String& srcName, bool doTrace)
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
Compiler::importFile(const SrcPos& srcpos,
                     const String& srcName, bool isPublic,
                     Scope* currentScope)
{
  String absPath = lookupFile(srcName, isPublic);
  return importFileImpl(srcpos, srcName, absPath, currentScope, true);
}


void
Compiler::importSystemHeader(const String& header, const String& avoidPath)
{
  String absPath = lookupFile(header, false);

  String fullAvoidPath = file::canonicalPathName(avoidPath);
  if (Properties::isTraceImportFile())
    log(kDebug, String("Load: ") + absPath + String(" while loading ") + fullAvoidPath);

  if (absPath == fullAvoidPath) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "Don't preload '%s'", (const char*)StrHelper(header));
    return;
  }

  if (Properties::isTraceImportFile())
    logf(kDebug, "Preload '%s'", (const char*)StrHelper(header));
  importFileImpl(SrcPos(), header, absPath, fState.fScope, false);
}


void
Compiler::importSystemHeaders(const String& avoidPath)
{
  importSystemHeader(String("builtin:lang/numbers.hr"), avoidPath);
  importSystemHeader(String("builtin:lang/runtime.hr"), avoidPath);
  importSystemHeader(String("builtin:lang/sliceable.hr"),   avoidPath);
}


bool
Compiler::importFileImpl(const SrcPos& srcpos,
                         const String& srcName, const String& absPath,
                         Scope* currentScope,
                         bool preload)
{
  typedef std::map<String, Ptr<Scope> > ImportCache;
  static ImportCache sImportCache;

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
    Ptr<Compiler> compiler = new Compiler(true);
    if (preload)
      compiler->importSystemHeaders(absPath);

    Ptr<AptNode> apt = compiler->processImpl(new CharPort(
                                               new FilePort(absPath, "rb")),
                                             srcName, false);
    Ptr<Scope> scope = compiler->scope();

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
Compiler::lookupFile(const String& srcName, bool isPublic)
{
  StringVector exts;
  exts.push_back(String("hr"));

  if (srcName.startsWith(String("builtin:")))
  {
    return file::lookupInPath(srcName.part(8, srcName.length()),
                              Properties::systemDirSearchPath(), exts);
  }

  String path = file::lookupInPath(srcName,
                                   Properties::systemDirSearchPath(), exts);
  if (path.isEmpty())
    path = file::lookupInPath(srcName, Properties::inputDirSearchPath(), exts);

  return path;
}


//==============================================================================

Compiler::CompilerState::CompilerState(CharRegistry* charReg,
                                       ConfigVarRegistry* configReg,
                                       Scope* scope)
  : fCharRegistry(charReg),
    fConfigVarRegistry(configReg),
    fScope(scope)
{
}


Compiler::CompilerState::CompilerState(const CompilerState& item)
{
  *this = item;
}


Compiler::CompilerState&
Compiler::CompilerState::operator=(const CompilerState& item)
{
  fPort              = item.fPort;
  fToken             = item.fToken;
  fCharRegistry      = item.fCharRegistry;
  fConfigVarRegistry = item.fConfigVarRegistry;
  fScope             = item.fScope;

  return *this;
}


//==============================================================================

Compiler::PortStackHelper::PortStackHelper(Compiler* compiler, TokenPort* port)
  : fCompiler(compiler),
    fPortOnly(true)
{
  fCompiler->fCompilerStates.push_front(fCompiler->fState);
  fCompiler->fState = CompilerState(
    compiler->charRegistry(),
    new ConfigVarRegistry(compiler->configVarRegistry()),
    compiler->fState.fScope);

  fCompiler->fState.fPort = port;
}


Compiler::PortStackHelper::~PortStackHelper()
{
  assert(!fCompiler->fCompilerStates.empty());

  CompilerState current = fCompiler->fState;
  fCompiler->fState = fCompiler->fCompilerStates.front();
  fCompiler->fCompilerStates.pop_front();

  if (!fPortOnly) {
    // merge current.fScope into fCompiler->fState; same for configVarReg and
    // current.fCharRegistry
  }
}


//----------------------------------------------------------------------------

namespace herschel
{
  void
  compileFile(const String& file, bool doParse, bool doCompile, bool doLink,
              const String& outfileName)
  {
    try {
      if (doParse) {
        Ptr<Compiler> compiler = new Compiler;
        Ptr<AptNode> apt = compiler->process(new CharPort(
                                               new FilePort(file, "rb")),
                                             file);
        if (doCompile) {
          assert(apt);
          CompileUnitNode* unit = dynamic_cast<CompileUnitNode*>(apt.obj());
          assert(unit != NULL);

          if (unit != NULL) {
            String outExt = makeCompileOutputFileExt(Properties::compileOutFormat());
            String outFile = makeOutputFileName(Properties::outdir(),
                                                outfileName, file, outExt);

            Ptr<CodeGenerator> codegen = new CodeGenerator();
            codegen->compileToCode(unit, outFile);
          }

          if (doLink) {
            // TODO
          }
        }
      }
    }
    catch (const Exception& e) {
      logf(kError, "compilation of '%s' failed: %s",
           (const char*)StrHelper(file),
           (const char*)StrHelper(e.message()));
    }
  }


  //--------------------------------------------------------------------------

  void
  parseFiles(const std::vector<String>& files, const String& outputFile)
  {
    for (std::vector<String>::const_iterator it = files.begin(), e = files.end();
         it != e;
         it++)
    {
      compileFile(*it, true, false, false, outputFile);
    }
  }


  //--------------------------------------------------------------------------

  void
  compileFiles(const std::vector<String>& files, const String& outputFile)
  {
    if (!outputFile.isEmpty() && files.size() > 1)
      logf(kError, "Outputfile and multiple compile files are given.");

    for (std::vector<String>::const_iterator it = files.begin(), e = files.end();
         it != e;
         it++)
    {
      compileFile(*it, true, true, false, outputFile);
    }
  }
};                              // namespace
