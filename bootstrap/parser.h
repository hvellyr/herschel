/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_parser_h
#define bootstrap_parser_h

#include "refcountable.h"
#include "apt.h"
#include "port.h"
#include "tokenport.h"
#include "token.h"



namespace heather
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

  class Parser : public RefCountable
  {
  public:
    Parser();

    virtual AptNode* parse(Port<Char>* port, const String& srcName);

    CharRegistry* charRegistry() const;
    ConfigVarRegistry* configVarRegistry() const;

    Token importFile(Port<Char>* port, const String& srcName);
    String lookupFile(const String& srcName, bool isPublic);
    Port<Char>* lookupFileAndOpen(const String& srcName, bool isPublic);

    // predefined symbol tokens to speed up parsing
    static const Token aliasToken;
    static const Token charToken;
    static const Token classToken;
    static const Token configToken;
    static const Token constToken;
    static const Token deleteToken;
    static const Token enumToken;
    static const Token exitToken;
    static const Token fluidToken;
    static const Token genericToken;
    static const Token ignoreToken;
    static const Token includeToken;
    static const Token initToken;
    static const Token macroToken;
    static const Token measureToken;
    static const Token signalToken;
    static const Token slotToken;
    static const Token syncToken;
    static const Token typeToken;
    static const Token unitToken;

  private:
    friend class FirstPass;
    friend class SecondPass;

    Token nextToken();

    class ParserState
    {
    public:
      ParserState(CharRegistry* charReg,
                  ConfigVarRegistry* configReg);
      ParserState(const ParserState& item);
      ParserState& operator=(const ParserState& item);

      Ptr<TokenPort>         fPort;
      Token                  fToken;
      Ptr<CharRegistry>      fCharRegistry;
      Ptr<ConfigVarRegistry> fConfigVarRegistry;
    };


    class PortStackHelper
    {
    public:
      PortStackHelper(Parser* parser);
      ~PortStackHelper();

      Parser* fParser;
    };


    //-------- data members

    ParserState            fState;
    std::list<ParserState> fParserStates;
  };
};

#endif  // bootstrap_parser_h
