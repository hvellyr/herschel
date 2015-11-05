/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "port.h"
#include "tokenizer.h"

#include <list>
#include <memory>


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
    FileTokenPort(std::shared_ptr<Port<Octet>> port, const String& srcName,
                  std::shared_ptr<CharRegistry> charRegistry = nullptr);
    FileTokenPort(std::shared_ptr<Port<Char>> port, const String& srcName,
                  std::shared_ptr<CharRegistry> charRegistry = nullptr);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual Token read();

  private:
    void setTokenizer(std::shared_ptr<Tokenizer> tokenizer);

    String         fSrcName;
    std::shared_ptr<Tokenizer> fTokenizer;
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

