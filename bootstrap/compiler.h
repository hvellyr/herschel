/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_compiler_h
#define bootstrap_compiler_h

#include "apt.h"
#include "macro.h"
#include "port.h"
#include "scope.h"
#include "token.h"
#include "tokenport.h"

#include <memory>

namespace herschel
{
  //--------------------------------------------------------------------------

  class SyntaxException : public Exception
  {
  public:
    SyntaxException(const String& msg)
      : Exception(msg)
    { }
  };


  //--------------------------------------------------------------------------

  class PrematureEndOfFileException : public SyntaxException
  {
  public:
    PrematureEndOfFileException()
      : SyntaxException(String("Premature end of file"))
    { }
  };


  //--------------------------------------------------------------------------

  class UndefinedSymbolException : public SyntaxException
  {
  public:
    UndefinedSymbolException(const String& sym)
      : SyntaxException(String("Undefined symbol '") + sym + "'")
    { }
  };


  //--------------------------------------------------------------------------

  class Compiler
  {
  public:
    Compiler(bool isParsingInterface = false);

    virtual std::shared_ptr<AptNode> process(Port<Char>* port,
                                             const String& srcName);

    CharRegistry* charRegistry() const;
    ConfigVarRegistry* configVarRegistry() const;

    bool importFile(const SrcPos& srcpos,
                    const String& srcName, bool isPublic,
                    std::shared_ptr<Scope> currentScope);
    String lookupFile(const String& srcName, bool isPublic);

    std::shared_ptr<Scope>& referredFunctionCache();

    // predefined symbol tokens to speed up parsing
    static const Token aliasToken;
    static const Token allocToken;
    static const Token autoToken;
    static const Token charToken;
    static const Token classToken;
    static const Token configToken;
    static const Token constToken;
    static const Token deleteToken;
    static const Token enumToken;
    static const Token exitToken;
    static const Token finalToken;
    static const Token genericToken;
    static const Token ignoreToken;
    static const Token includeToken;
    static const Token initToken;
    static const Token innerToken;
    static const Token macroToken;
    static const Token measureToken;
    static const Token outerToken;
    static const Token privateToken;
    static const Token publicToken;
    static const Token readonlyToken;
    static const Token signalToken;
    static const Token slotToken;
    static const Token syncToken;
    static const Token transientToken;
    static const Token typeToken;
    static const Token unitToken;


    class PortStackHelper
    {
    public:
      PortStackHelper(Compiler& compiler, TokenPort* port);
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

    std::shared_ptr<AptNode> processImpl(Port<Char>* port, const String& srcName,
                                         bool doTrace);
    bool importFileImpl(const SrcPos& srcpos,
                        const String& srcName, const String& absPath,
                        std::shared_ptr<Scope> currentScope,
                        bool preload);

    void importSystemHeaders(const String& avoidPath);
    bool importSystemHeader(const String& header, const String& avoidPath);

    class CompilerState
    {
    public:
      CompilerState(CharRegistry*      charReg,
                    ConfigVarRegistry* configReg,
                    std::shared_ptr<Scope> scope);
      CompilerState(const CompilerState& item);
      CompilerState& operator=(const CompilerState& item);

      Ptr<TokenPort>         fPort;
      Token                  fToken;
      Ptr<CharRegistry>      fCharRegistry;
      Ptr<ConfigVarRegistry> fConfigVarRegistry;
      std::shared_ptr<Scope> fScope;
    };


    //-------- data members

    CompilerState            fState;
    std::list<CompilerState> fCompilerStates;
    bool                     fIsParsingInterface;
    std::shared_ptr<Scope>   fReferredFunctionCache;
  };


  //--------------------------------------------------------------------------

  void compileFile(const String& file,
                   bool doParse, bool doCompile, bool doLink,
                   const String& outfileName);

  void parseFiles(const std::vector<String>& files,
                  const String& outputfile);

  void compileFiles(const std::vector<String>& files,
                    const String& outputfile);
};

#endif  // bootstrap_compiler_h
