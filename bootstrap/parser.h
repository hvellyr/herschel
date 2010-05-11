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
#include "pexpr.h"



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

  class UnexpectedTokenException : public SyntaxException
  {
  public:
    UnexpectedTokenException(const Pexpr& t)
      : SyntaxException(String("Unexpected token: ") + t)
    { }

    UnexpectedTokenException(const Pexpr& t, const String& msg)
      : SyntaxException(String("Unexpected token: ") + t + " " + msg)
    { }

    UnexpectedTokenException(const Pexpr& t, const char* msg)
      : SyntaxException(String("Unexpected token: ") + t + " " + msg)
    { }

    UnexpectedTokenException(const Pexpr& t, const Pexpr& expected)
      : SyntaxException(String("Unexpected token: ") + t + " expected: " + expected)
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
    Parser(Parser* parent);

    virtual AptNode* parse(Port<Char>* port);

    CharRegistry* charRegistry() const;
    ConfigVarRegistry* configVarRegistry() const;

  private:
    friend class FirstPass;
    friend class SecondPass;

    Pexpr nextToken();

    //-------- data members

    Ptr<Parser>    fParent;
    Ptr<TokenPort> fPort;
    Pexpr          fToken;

    Ptr<CharRegistry> fCharRegistry;
    Ptr<ConfigVarRegistry> fConfigVarRegistry;
  };
};

#endif  // bootstrap_parser_h
