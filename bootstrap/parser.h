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
    UnexpectedTokenException(const Token& t)
      : SyntaxException(String("Unexpected token: ") + t.c_str())
    { }

    UnexpectedTokenException(const Token& t, const String& msg)
      : SyntaxException(String("Unexpected token: ") + t.c_str() + " " + msg)
    { }

    UnexpectedTokenException(const Token& t, const char* msg)
      : SyntaxException(String("Unexpected token: ") + t.c_str() + " " + msg)
    { }

    UnexpectedTokenException(const Token& t, const Token& expected)
      : SyntaxException(String("Unexpected token: ") + t.c_str() + " expected: " + expected.c_str())
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

  private:
    AptNode* parseTop();
    AptNode* parseModule(bool isModule);
    AptNode* parseExport();
    AptNode* parseImport();
    AptNode* parseDef();
    AptNode* parseCharDef();

    Token nextToken();

    //-------- data members

    Ptr<Parser>    fParent;
    Ptr<TokenPort> fPort;
    Token          fToken;
    std::list<Ptr<AptNode> > fLastModules;

    Ptr<CharRegistry> fCharRegistry;
  };
};

#endif  // bootstrap_parser_h
