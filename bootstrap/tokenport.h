/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_tokenport_h
#define bootstrap_tokenport_h

#include <list>

#include "port.h"
#include "tokenizer.h"

namespace heather
{
  //--------------------------------------------------------------------------

  class TokenPort : public Port<Token>
  {
  public:
    virtual size_t write(Token* data, size_t items);
    virtual int write(Token item);

    virtual void flush();

    virtual bool canSetCursor() const;
  };


  //--------------------------------------------------------------------------

  class FileTokenPort : public TokenPort
  {
  public:
    FileTokenPort(Port<Octet>* port);
    FileTokenPort(Port<Char>* port);

    ~FileTokenPort();

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual Token read();

  private:
    void setTokenizer(Tokenizer* tokenizer);

    Ptr<Tokenizer> fTokenizer;
  };


  //--------------------------------------------------------------------------

  class InternalTokenPort : public TokenPort
  {
  public:
    InternalTokenPort(const std::list<Token>& tokens);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual Token read();

  private:
    std::list<Token> fTokens;
  };
};                              // namespace

#endif  // bootstrap_tokenport_h
