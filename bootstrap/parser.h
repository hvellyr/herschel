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
  class UnexpectedTokenException : public Exception
  {
  public:
    UnexpectedTokenException(Token t)
      : Exception(String("Unexpected token: ") + t.c_str())
    { }

    UnexpectedTokenException(Token t, const String& msg)
      : Exception(String("Unexpected token: ") + t.c_str() + " " + msg)
    { }
  };


  class PrematureEndOfFileException : public Exception
  {
  public:
    PrematureEndOfFileException()
      : Exception(String("Premature end of file"))
    { }
  };


  class Parser : public RefCountable
  {
  public:
    Parser();

    virtual AptNode* parse(Port<Char>* port);

  private:
    AptNode* parseTop();
    AptNode* parseModule(bool isModule);

    Token nextToken();

    //-------- data members

    Ptr<TokenPort> fPort;
    Token fCurrentToken;
    std::list<Ptr<AptNode> > fLastModules;
  };
};

#endif  // bootstrap_parser_h
