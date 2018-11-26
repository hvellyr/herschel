/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "ast.hpp"
#include "macro.hpp"
#include "port.hpp"
#include "scope.hpp"
#include "token.hpp"
#include "tokenport.hpp"

#include <memory>


namespace herschel {

class SyntaxException : public Exception {
public:
  SyntaxException(const String& msg)
      : Exception(msg)
  {
  }
};


class PrematureEndOfFileException : public SyntaxException {
public:
  PrematureEndOfFileException()
      : SyntaxException(String("Premature end of file"))
  {
  }
};


class UndefinedSymbolException : public SyntaxException {
public:
  UndefinedSymbolException(const String& sym)
      : SyntaxException(String("Undefined symbol '") + sym + "'")
  {
  }
};


class Compiler {
public:
  Compiler(bool isParsingInterface = false);

  virtual std::shared_ptr<AstNode> process(std::shared_ptr<Port<Char>> port,
                                           const String& srcName);

  std::shared_ptr<CharRegistry> charRegistry() const;
  std::shared_ptr<ConfigVarRegistry> configVarRegistry() const;

  bool importFile(const SrcPos& srcpos, const String& srcName, bool isPublic,
                  std::shared_ptr<Scope> currentScope);
  String lookupFile(const String& srcName, bool isPublic);

  std::shared_ptr<Scope>& referredFunctionCache();

  // predefined symbol tokens to speed up parsing
  static const Token aliasToken;
  static const Token autoToken;
  static const Token charToken;
  static const Token configToken;
  static const Token constToken;
  static const Token deleteToken;
  static const Token enumToken;
  static const Token exitToken;
  static const Token finalToken;
  static const Token genericToken;
  static const Token ignoreToken;
  static const Token includeToken;
  static const Token innerToken;
  static const Token macroToken;
  static const Token measureToken;
  static const Token outerToken;
  static const Token privateToken;
  static const Token publicToken;
  static const Token readonlyToken;
  static const Token recordToken;
  static const Token signalToken;
  static const Token syncToken;
  static const Token transientToken;
  static const Token typeToken;
  static const Token unitToken;


  class PortStackHelper {
  public:
    PortStackHelper(Compiler& compiler, std::shared_ptr<TokenPort> port);
    ~PortStackHelper();

  private:
    Compiler& fCompiler;
    bool fPortOnly;
  };


private:
  friend class FirstPass;
  friend class SecondPass;

  bool isParsingInterface() const;
  std::shared_ptr<Scope> scope() const;

  Token nextToken();
  void unreadToken(const Token& token);

  std::shared_ptr<AstNode> processImpl(std::shared_ptr<Port<Char>> port,
                                       const String& srcName, bool doTrace);
  bool importFileImpl(const SrcPos& srcpos, const String& srcName, const String& absPath,
                      std::shared_ptr<Scope> currentScope, bool preload);

  void importSystemHeaders(const String& avoidPath);
  bool importSystemHeader(const String& header, const String& avoidPath);

  class CompilerState {
  public:
    CompilerState(std::shared_ptr<CharRegistry> charReg,
                  std::shared_ptr<ConfigVarRegistry> configReg,
                  std::shared_ptr<Scope> scope);
    CompilerState(const CompilerState& item);
    CompilerState& operator=(const CompilerState& item);

    std::shared_ptr<TokenPort> fPort;
    Token fToken;
    std::shared_ptr<CharRegistry> fCharRegistry;
    std::shared_ptr<ConfigVarRegistry> fConfigVarRegistry;
    std::shared_ptr<Scope> fScope;
  };


  CompilerState fState;
  std::list<CompilerState> fCompilerStates;
  bool fIsParsingInterface;
  std::shared_ptr<Scope> fReferredFunctionCache;
};


void compileFile(const String& file, bool doParse, bool doCompile, bool doLink,
                 const String& outfileName);

void parseFiles(const std::vector<String>& files, const String& outputfile);

void compileFiles(const std::vector<String>& files, const String& outputfile);

}  // namespace herschel
