/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_tokenport_h
#define bootstrap_tokenport_h

#include <list>

#include "port.h"
#include "tokenizer.h"


namespace herschel
{
  //--------------------------------------------------------------------------

  class TokenPort : public Port<Token>
  {
  public:
    virtual size_t write(const Token* data, size_t items);
    virtual int write(Token item);

    virtual void flush();

    virtual bool canSetCursor() const;
  };


  //--------------------------------------------------------------------------

  class FileTokenPort : public TokenPort
  {
  public:
    FileTokenPort(Port<Octet>* port, const String& srcName,
                  CharRegistry* charRegistry = NULL);
    FileTokenPort(Port<Char>* port, const String& srcName,
                  CharRegistry* charRegistry = NULL);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual Token read();

  private:
    void setTokenizer(Tokenizer* tokenizer);

    String         fSrcName;
    Ptr<Tokenizer> fTokenizer;
  };


  //--------------------------------------------------------------------------

  class InternalTokenPort : public TokenPort
  {
  public:
    InternalTokenPort(const std::list<Token>& tokens);
    InternalTokenPort(const TokenVector& tokens);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual Token read();

  private:
    std::list<Token> fTokens;
  };
};                              // namespace

#endif  // bootstrap_tokenport_h
