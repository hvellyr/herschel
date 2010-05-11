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

  class TokenPort : public Port<Pexpr>
  {
  public:
    virtual size_t write(Pexpr* data, size_t items);
    virtual int write(Pexpr item);

    virtual void flush();

    virtual bool canSetCursor() const;
  };


  //--------------------------------------------------------------------------

  class FileTokenPort : public TokenPort
  {
  public:
    FileTokenPort(Port<Octet>* port, CharRegistry* charRegistry = NULL);
    FileTokenPort(Port<Char>* port, CharRegistry* charRegistry = NULL);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual Pexpr read();

  private:
    void setTokenizer(Tokenizer* tokenizer);

    Ptr<Tokenizer> fTokenizer;
  };


  //--------------------------------------------------------------------------

  class InternalTokenPort : public TokenPort
  {
  public:
    InternalTokenPort(const std::list<Pexpr>& tokens);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual Pexpr read();

  private:
    std::list<Pexpr> fTokens;
  };
};                              // namespace

#endif  // bootstrap_tokenport_h
