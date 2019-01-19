/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "compiler.hpp"

#include "annotate.hpp"
#include "annotate2.hpp"
#include "ast.hpp"
//#include "codegen.hpp"
#include "errcodes.hpp"
#include "exprpass.hpp"
#include "file.hpp"
#include "filetool.hpp"
#include "log.hpp"
#include "nodifypass.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "rootscope.hpp"
#include "scope.hpp"
#include "str.hpp"
#include "tokenizer.hpp"
#include "typecheck.hpp"
#include "typify.hpp"
#include "utils.hpp"
#include "xmlrenderer.hpp"

#include <map>
#include <set>


namespace herschel {


//----------------------------------------------------------------------------

const Token Compiler::aliasToken = Token(SrcPos(), kSymbol, "alias");
const Token Compiler::autoToken = Token(SrcPos(), kSymbol, "auto");
const Token Compiler::charToken = Token(SrcPos(), kSymbol, "char");
const Token Compiler::configToken = Token(SrcPos(), kSymbol, "config");
const Token Compiler::constToken = Token(SrcPos(), kSymbol, "const");
const Token Compiler::deleteToken = Token(SrcPos(), kSymbol, "delete");
const Token Compiler::enumToken = Token(SrcPos(), kSymbol, "enum");
const Token Compiler::exitToken = Token(SrcPos(), kSymbol, MID_exitKeyword);
const Token Compiler::finalToken = Token(SrcPos(), kSymbol, "final");
const Token Compiler::genericToken = Token(SrcPos(), kSymbol, "generic");
const Token Compiler::ignoreToken = Token(SrcPos(), kSymbol, "ignore");
const Token Compiler::includeToken = Token(SrcPos(), kSymbol, "include");
const Token Compiler::innerToken = Token(SrcPos(), kSymbol, "inner");
const Token Compiler::macroToken = Token(SrcPos(), kSymbol, "macro");
const Token Compiler::outerToken = Token(SrcPos(), kSymbol, "outer");
const Token Compiler::privateToken = Token(SrcPos(), kSymbol, "private");
const Token Compiler::publicToken = Token(SrcPos(), kSymbol, "public");
const Token Compiler::readonlyToken = Token(SrcPos(), kSymbol, "readonly");
const Token Compiler::recordToken = Token(SrcPos(), kSymbol, "record");
const Token Compiler::signalToken = Token(SrcPos(), kSymbol, MID_signalKeyword);
const Token Compiler::syncToken = Token(SrcPos(), kSymbol, "sync");
const Token Compiler::transientToken = Token(SrcPos(), kSymbol, "transient");
const Token Compiler::typeToken = Token(SrcPos(), kSymbol, "type");


//----------------------------------------------------------------------------

Compiler::Compiler(bool isParsingInterface)
    : Compiler(isParsingInterface, type::newRootScope())
{
}


Compiler::Compiler(bool isParsingInterface, std::shared_ptr<Scope> rootScope)
    : fState(CompilerState(
          std::make_shared<CharRegistry>(),
          std::make_shared<ConfigVarRegistry>(Properties::globalConfigVarRegistry()),
          rootScope))
    , fIsParsingInterface(isParsingInterface)
    , fReferredFunctionCache(makeScope(kScopeL_CompileUnit))
{
}


std::shared_ptr<CharRegistry> Compiler::charRegistry() const
{
  return fState.fCharRegistry;
}


std::shared_ptr<ConfigVarRegistry> Compiler::configVarRegistry() const
{
  return fState.fConfigVarRegistry;
}


std::shared_ptr<Scope> Compiler::scope() const
{
  return fState.fScope;
}


std::shared_ptr<Scope>& Compiler::referredFunctionCache()
{
  return fReferredFunctionCache;
}


bool Compiler::isParsingInterface() const
{
  return fIsParsingInterface;
}


Token Compiler::nextToken()
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


void Compiler::unreadToken(const Token& token)
{
  fState.fPort->unread(token);
}


std::shared_ptr<AstNode> Compiler::process(std::shared_ptr<Port<Char>> port,
                                           const String& srcName)
{
  fState.fScope = makeScope(kScopeL_CompileUnit, fState.fScope);
  importSystemHeaders();

  return processImpl(port, srcName, K(doTrace));
}


std::shared_ptr<AstNode> Compiler::processImpl(std::shared_ptr<Port<Char>> port,
                                               const String& srcName, bool doTrace)
{
  fState.fPort = std::make_shared<FileTokenPort>(port, srcName, fState.fCharRegistry);

  hr_assert(fState.fScope);

  try {
    std::shared_ptr<AstNode> ast;
    Token parsedExprs;

    ExprPass tokenPass{ 1, *this, fState.fToken, fState.fScope };
    parsedExprs = tokenPass.apply(Token(), doTrace);

    // let all following passes run beneath the same root-scope.
    {
      ScopeHelper scopeHelper(fState.fScope, K(doExport), !K(isInnerScope),
                              kScopeL_CompileUnit);

      NodifyPass nodifyPass{ 2, *this, fState.fScope };
      ast = nodifyPass.apply(parsedExprs, doTrace);

      // if the compileunit contains open-ended module declarations
      // (i.e. without {}) get the last valid scope back and make it the
      // current one.  It contains the complete upstream chain of scopes.  (We
      // must not simply export it back to the original fState.fScope, since
      // the symbols may not be exportable at all).
      fState.fScope = nodifyPass.currentScope();

      AnnotatePass nodePass2{ 3, *this };
      ast = nodePass2.apply(ast, doTrace);

      TypifyPass nodePass3{ 4 };
      ast = nodePass3.apply(ast, doTrace);

      Annotate2Pass nodePass4{ 5, *this };
      ast = nodePass4.apply(ast, doTrace);

      TypeCheckPass nodePass5{ 6 };
      ast = nodePass5.apply(ast, doTrace);
    }

    return ast;
  }
  catch (const Exception& e) {
    logf(kError, "Parse error: %s", (zstring)StrHelper(e.message()));
  }

  return nullptr;
}


TokenVector Compiler::includeFile(const SrcPos& srcpos, const String& srcName,
                                  const std::function<TokenVector()>& functor)
{
  String absPath = lookupFile(srcName, K(isPublic));
  return includeFileImpl(srcpos, srcName, absPath, functor);
}


bool Compiler::importFile(const SrcPos& srcpos, const String& srcName, bool isPublic,
                          std::shared_ptr<Scope> currentScope)
{
  String absPath = lookupFile(srcName, isPublic);
  return importFileImpl(srcpos, srcName, absPath, currentScope, K(preload));
}


bool Compiler::importSystemHeader(const String& header)
{
  String absPath = lookupFile(header, !K(isPublic));

  if (Properties::isTraceImportFile())
    log(kDebug, String("Load: ") + absPath);

  if (Properties::isTraceImportFile())
    logf(kDebug, "Preload '%s'", (zstring)StrHelper(header));
  importFileImpl(SrcPos(), header, absPath, fState.fScope, !K(preload));

  return true;
}


void Compiler::importSystemHeaders()
{
  if (!importSystemHeader(String("builtin:lang/lang.hr")))
    return;
}


namespace {
  std::set<String> sPathsInLoading;

  struct PathLoadingGuard {
    String fPath;
    bool fOwns = false;

    PathLoadingGuard(String p)
        : fPath(std::move(p))
    {
      fOwns = sPathsInLoading.insert(fPath).second;
    }

    ~PathLoadingGuard()
    {
      if (fOwns)
        sPathsInLoading.erase(fPath);
    }
  };
}  // namespace


TokenVector Compiler::includeFileImpl(const SrcPos& srcpos, const String& srcName,
                                      const String& absPath,
                                      const std::function<TokenVector()>& functor)
{
  TokenVector result;

  if (absPath.isEmpty()) {
    errorf(srcpos, E_UnknownInputFile, "include '%s' failed: Unknown file\n",
           (zstring)StrHelper(srcName));
    return result;
  }

  if (sPathsInLoading.count(absPath) > 0) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "File '%s' is currently included.  Avoid loop",
           (zstring)StrHelper(absPath));
    return result;
  }

  PathLoadingGuard guard(absPath);

  if (Properties::isTraceImportFile())
    logf(kDebug, "Include '%s'", (zstring)StrHelper(srcName));

  try {
    fCompilerStates.push_front(fState);
    fState = CompilerState(charRegistry(), configVarRegistry(), fState.fScope);
    fState.fPort = std::make_shared<FileTokenPort>(
        std::make_shared<CharPort>(std::make_shared<FilePort>(absPath, "rb")), srcName,
        fState.fCharRegistry);

    result = functor();

    CompilerState current = fState;
    fState = fCompilerStates.front();
    fCompilerStates.pop_front();

    unreadToken(fState.fToken);
  }
  catch (const Exception& e) {
    errorf(srcpos, E_UnknownInputFile, "import '%s' failed: %s\n",
           (zstring)StrHelper(absPath), (zstring)StrHelper(e.message()));
    return {};
  }

  return result;
}


bool Compiler::importFileImpl(const SrcPos& srcpos, const String& srcName,
                              const String& absPath, std::shared_ptr<Scope> currentScope,
                              bool preload)
{
  using ImportCache = std::map<String, std::shared_ptr<Scope>>;
  static ImportCache sImportCache;

  if (absPath.isEmpty()) {
    errorf(srcpos, E_UnknownInputFile, "import '%s' failed: Unknown file\n",
           (zstring)StrHelper(srcName));
    return false;
  }

  if (sPathsInLoading.count(absPath) > 0) {
    if (Properties::isTraceImportFile())
      logf(kDebug, "File '%s' is currently imported.  Avoid loop",
           (zstring)StrHelper(absPath));
    return true;
  }

  PathLoadingGuard guard(absPath);

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
    auto compiler = Compiler{ K(isParsingInterface) };
    if (preload)
      compiler.importSystemHeaders();

    auto ast = compiler.processImpl(
        std::make_shared<CharPort>(std::make_shared<FilePort>(absPath, "rb")), srcName,
        !K(doTrace));
    auto scope = compiler.scope();

    currentScope->addImportedScope(absPath, scope);

    sImportCache.insert(std::make_pair(absPath, scope));
  }
  catch (const Exception& e) {
    errorf(srcpos, E_UnknownInputFile, "import '%s' failed: %s\n",
           (zstring)StrHelper(absPath), (zstring)StrHelper(e.message()));
    return false;
  }

  return true;
}


String Compiler::lookupFile(const String& srcName, bool isPublic)
{
  auto exts = makeVector(String("hr"));

  if (srcName.startsWith(String("builtin:"))) {
    return file::lookupInPath(srcName.part(8, srcName.length()),
                              Properties::systemDirSearchPath(), exts);
  }

  String path = file::lookupInPath(srcName, Properties::systemDirSearchPath(), exts);
  if (path.isEmpty())
    path = file::lookupInPath(srcName, Properties::inputDirSearchPath(), exts);

  return path;
}


//==============================================================================

Compiler::CompilerState::CompilerState(std::shared_ptr<CharRegistry> charReg,
                                       std::shared_ptr<ConfigVarRegistry> configReg,
                                       std::shared_ptr<Scope> scope)
    : fCharRegistry(std::move(charReg))
    , fConfigVarRegistry(std::move(configReg))
    , fScope(std::move(scope))
{
}


Compiler::CompilerState::CompilerState(const CompilerState& item)
{
  *this = item;
}


Compiler::CompilerState& Compiler::CompilerState::operator=(const CompilerState& item)
{
  fPort = item.fPort;
  fToken = item.fToken;
  fCharRegistry = item.fCharRegistry;
  fConfigVarRegistry = item.fConfigVarRegistry;
  fScope = item.fScope;

  return *this;
}


//==============================================================================

Compiler::PortStackHelper::PortStackHelper(Compiler& compiler,
                                           std::shared_ptr<TokenPort> port)
    : fCompiler(compiler)
    , fPortOnly(true)
{
  fCompiler.fCompilerStates.push_front(fCompiler.fState);
  fCompiler.fState =
      CompilerState(compiler.charRegistry(),
                    std::make_shared<ConfigVarRegistry>(compiler.configVarRegistry()),
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

void compileFile(const String& file, bool doParse, bool doCompile, bool doLink,
                 const String& outfileName)
{
  try {
    if (doParse) {
      Compiler compiler(!K(isParsingInterface));
      auto ast = compiler.process(
          std::make_shared<CharPort>(std::make_shared<FilePort>(file, "rb")), file);

      if (doCompile) {
        XmlRenderer out{ std::make_shared<FilePort>(stderr), true };
        out.render(ast);

        hr_assert(ast);
        auto unit = dynamic_cast<CompileUnitNode*>(ast.get());
        hr_assert(unit);

        if (unit) {
          String outExt = makeCompileOutputFileExt(Properties::compileOutFormat());
          String outFile =
              makeOutputFileName(Properties::outdir(), outfileName, file, outExt);

#if 0
          CodeGenerator codegen{ compiler };
          codegen.compileToCode(unit, outFile);
#endif
        }

        if (doLink) {
          // TODO
        }
      }
    }
  }
  catch (const Exception& e) {
    logf(kError, "compilation of '%s' failed: %s", (zstring)StrHelper(file),
         (zstring)StrHelper(e.message()));
  }
}


//--------------------------------------------------------------------------

void parseFiles(const std::vector<String>& files, const String& outputFile)
{
  for (const auto& f : files)
    compileFile(f, K(doParse), !K(doCompile), !K(doLink), outputFile);
}


//--------------------------------------------------------------------------

void compileFiles(const std::vector<String>& files, const String& outputFile)
{
  if (!outputFile.isEmpty() && files.size() > 1)
    logf(kError, "Outputfile and multiple compile files are given.");

  for (const auto& f : files)
    compileFile(f, K(doParse), K(doCompile), !K(doLink), outputFile);
}

}  // namespace herschel
