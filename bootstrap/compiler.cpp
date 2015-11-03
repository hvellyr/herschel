/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
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
#include "utils.h"

using namespace herschel;


//----------------------------------------------------------------------------

const Token Compiler::aliasToken     = Token(SrcPos(), kSymbol, "alias");
const Token Compiler::allocToken     = Token(SrcPos(), kSymbol, "alloc");
const Token Compiler::autoToken      = Token(SrcPos(), kSymbol, "auto");
const Token Compiler::charToken      = Token(SrcPos(), kSymbol, "char");
const Token Compiler::classToken     = Token(SrcPos(), kSymbol, "class");
const Token Compiler::configToken    = Token(SrcPos(), kSymbol, "config");
const Token Compiler::constToken     = Token(SrcPos(), kSymbol, "const");
const Token Compiler::deleteToken    = Token(SrcPos(), kSymbol, "delete");
const Token Compiler::enumToken      = Token(SrcPos(), kSymbol, "enum");
const Token Compiler::exitToken      = Token(SrcPos(), kSymbol, MID_exitKeyword);
const Token Compiler::finalToken     = Token(SrcPos(), kSymbol, "final");
const Token Compiler::genericToken   = Token(SrcPos(), kSymbol, "generic");
const Token Compiler::ignoreToken    = Token(SrcPos(), kSymbol, "ignore");
const Token Compiler::includeToken   = Token(SrcPos(), kSymbol, "include");
const Token Compiler::initToken      = Token(SrcPos(), kSymbol, "init");
const Token Compiler::innerToken     = Token(SrcPos(), kSymbol, "inner");
const Token Compiler::macroToken     = Token(SrcPos(), kSymbol, "macro");
const Token Compiler::measureToken   = Token(SrcPos(), kSymbol, "measure");
const Token Compiler::outerToken     = Token(SrcPos(), kSymbol, "outer");
const Token Compiler::privateToken   = Token(SrcPos(), kSymbol, "private");
const Token Compiler::publicToken    = Token(SrcPos(), kSymbol, "public");
const Token Compiler::readonlyToken  = Token(SrcPos(), kSymbol, "readonly");
const Token Compiler::signalToken    = Token(SrcPos(), kSymbol, MID_signalKeyword);
const Token Compiler::slotToken      = Token(SrcPos(), kSymbol, "slot");
const Token Compiler::syncToken      = Token(SrcPos(), kSymbol, "sync");
const Token Compiler::transientToken = Token(SrcPos(), kSymbol, "transient");
const Token Compiler::typeToken      = Token(SrcPos(), kSymbol, "type");
const Token Compiler::unitToken      = Token(SrcPos(), kSymbol, "unit");


//----------------------------------------------------------------------------

Compiler::Compiler(bool isParsingInterface)
  : fState(CompilerState(
             new CharRegistry,
             new ConfigVarRegistry(Properties::globalConfigVarRegistry()),
             type::newRootScope())),
    fIsParsingInterface(isParsingInterface),
    fReferredFunctionCache(makeScope(kScopeL_CompileUnit))
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


std::shared_ptr<Scope>
Compiler::scope() const
{
  return fState.fScope;
}


std::shared_ptr<Scope>&
Compiler::referredFunctionCache()
{
  return fReferredFunctionCache;
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


std::shared_ptr<AptNode>
Compiler::process(Port<Char>* port, const String& srcName)
{
  fState.fScope = makeScope(kScopeL_CompileUnit, fState.fScope);
  importSystemHeaders(srcName);
  return processImpl(port, srcName, K(doTrace));
}


std::shared_ptr<AptNode>
Compiler::processImpl(Port<Char>* port, const String& srcName, bool doTrace)
{
  fState.fPort = new FileTokenPort(port, srcName, fState.fCharRegistry);

  hr_assert(fState.fScope);

  try {
    std::shared_ptr<AptNode> apt;
    Token parsedExprs;

    ExprPass tokenPass{1, *this, fState.fToken, fState.fScope};
    parsedExprs = tokenPass.apply(Token(), doTrace);

    // let all following passes run beneath the same root-scope.
    {
      ScopeHelper scopeHelper(fState.fScope,
                              K(doExport),
                              !K(isInnerScope),
                              kScopeL_CompileUnit);

      NodifyPass nodifyPass{2, *this, fState.fScope};
      apt = nodifyPass.apply(parsedExprs, doTrace);

      // if the compileunit contains open-ended module declarations
      // (i.e. without {}) get the last valid scope back and make it the
      // current one.  It contains the complete upstream chain of scopes.  (We
      // must not simply export it back to the original fState.fScope, since
      // the symbols may not be exportable at all).
      fState.fScope = nodifyPass.currentScope();

      TransformPass nodePass1{3};
      apt = nodePass1.apply(apt, doTrace);

      AnnotatePass nodePass2{4, fState.fScope, *this};
      apt = nodePass2.apply(apt, doTrace);

      TypifyPass nodePass3{5};
      apt = nodePass3.apply(apt, doTrace);
    }

    return apt;
  }
  catch (const Exception& e) {
    logf(kError, "Parse error: %s", (zstring)StrHelper(e.message()));
  }

  return nullptr;
}


bool
Compiler::importFile(const SrcPos& srcpos,
                     const String& srcName, bool isPublic,
                     std::shared_ptr<Scope> currentScope)
{
  String absPath = lookupFile(srcName, isPublic);
  return importFileImpl(srcpos, srcName, absPath, currentScope, K(preload));
}


bool
Compiler::importSystemHeader(const String& header, const String& fullAvoidPath)
{
  String absPath = lookupFile(header, !K(isPublic));

  if (Properties::isTraceImportFile())
    log(kDebug, String("Load: ") + absPath + String(" while loading ") + fullAvoidPath);

  if (absPath == fullAvoidPath) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "Don't preload '%s'", (zstring)StrHelper(header));
    return false;
  }

  if (Properties::isTraceImportFile())
    logf(kDebug, "Preload '%s'", (zstring)StrHelper(header));
  importFileImpl(SrcPos(), header, absPath, fState.fScope, !K(preload));

  return true;
}


void
Compiler::importSystemHeaders(const String& avoidPath)
{
  String fullAvoidPath = file::canonicalPathName(avoidPath);

  if (!importSystemHeader(String("builtin:lang/types.hr"), fullAvoidPath))
    return;

  if (!importSystemHeader(String("builtin:lang/numbers.hr"), fullAvoidPath))
    return;

  if (!importSystemHeader(String("builtin:lang/comparable.hr"), fullAvoidPath))
    return;

  if (!importSystemHeader(String("builtin:lang/runtime.hr"), fullAvoidPath))
    return;

  if (!importSystemHeader(String("builtin:lang/sliceable.hr"), fullAvoidPath))
    return;

  if (!importSystemHeader(String("builtin:lang/copyable.hr"), fullAvoidPath))
    return;
  if (!importSystemHeader(String("builtin:lang/string.hr"), fullAvoidPath))
    return;
}


bool
Compiler::importFileImpl(const SrcPos& srcpos,
                         const String& srcName, const String& absPath,
                         std::shared_ptr<Scope> currentScope,
                         bool preload)
{
  using ImportCache = std::map<String, std::shared_ptr<Scope>>;
  static ImportCache sImportCache;

  if (absPath.isEmpty()) {
    errorf(srcpos, E_UnknownInputFile,
           "import '%s' failed: Unknown file\n",
           (zstring)StrHelper(srcName));
    return false;
  }

  if (currentScope->hasScopeForFile(absPath)) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "File '%s' already imported", (zstring)StrHelper(absPath));
    return true;
  }

  ImportCache::iterator it = sImportCache.find(absPath);
  if (it != sImportCache.end()) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "Reuse imported '%s'", (zstring)StrHelper(absPath));
    currentScope->addImportedScope(absPath, it->second);
    return true;
  }

  if (Properties::isTraceImportFile())
    logf(kDebug, "Import '%s'", (zstring)StrHelper(srcName));

  try {
    auto compiler = Compiler{K(isParsingInterface)};
    if (preload)
      compiler.importSystemHeaders(absPath);

    auto apt = compiler.processImpl(new CharPort(
                                      new FilePort(absPath, "rb")),
                                    srcName, !K(doTrace));
    auto scope = compiler.scope();

    currentScope->addImportedScope(absPath, scope);

    sImportCache.insert(std::make_pair(absPath, scope));
  }
  catch (const Exception& e) {
    errorf(srcpos, E_UnknownInputFile,
           "import '%s' failed: %s\n",
           (zstring)StrHelper(absPath),
           (zstring)StrHelper(e.message()));
    return false;
  }

  return true;
}


String
Compiler::lookupFile(const String& srcName, bool isPublic)
{
  auto exts = makeVector(String("hr"));

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
                                       std::shared_ptr<Scope> scope)
  : fCharRegistry(charReg),
    fConfigVarRegistry(configReg),
    fScope(std::move(scope))
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

Compiler::PortStackHelper::PortStackHelper(Compiler& compiler, TokenPort* port)
  : fCompiler(compiler),
    fPortOnly(true)
{
  fCompiler.fCompilerStates.push_front(fCompiler.fState);
  fCompiler.fState = CompilerState(
    compiler.charRegistry(),
    new ConfigVarRegistry(compiler.configVarRegistry()),
    compiler.fState.fScope);

  fCompiler.fState.fPort = port;
}


Compiler::PortStackHelper::~PortStackHelper()
{
  hr_assert(!fCompiler.fCompilerStates.empty());

  CompilerState current = fCompiler.fState;
  fCompiler.fState = fCompiler.fCompilerStates.front();
  fCompiler.fCompilerStates.pop_front();

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
        Compiler compiler{};
        auto apt = compiler.process(new CharPort(
                                      new FilePort(file, "rb")),
                                    file);
        if (doCompile) {
          hr_assert(apt);
          auto unit = dynamic_cast<CompileUnitNode*>(apt.get());
          hr_assert(unit);

          if (unit) {
            String outExt = makeCompileOutputFileExt(Properties::compileOutFormat());
            String outFile = makeOutputFileName(Properties::outdir(),
                                                outfileName, file, outExt);

            CodeGenerator codegen{compiler};
            codegen.compileToCode(unit, outFile);
          }

          if (doLink) {
            // TODO
          }
        }
      }
    }
    catch (const Exception& e) {
      logf(kError, "compilation of '%s' failed: %s",
           (zstring)StrHelper(file),
           (zstring)StrHelper(e.message()));
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
      compileFile(*it, K(doParse), !K(doCompile), !K(doLink), outputFile);
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
      compileFile(*it, K(doParse), K(doCompile), !K(doLink), outputFile);
    }
  }
};                              // namespace
