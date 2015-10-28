/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "common.h"

#include <string.h>

#include "tokenport.h"
#include "port.h"
#include "token.h"
#include "tokenizer.h"

using namespace herschel;


//----------------------------------------------------------------------------

size_t
TokenPort::write(const Token* /* data */, size_t /* items */)
{
  throw NotSupportedException(__FUNCTION__);
}


int
TokenPort::write(Token item)
{
  throw NotSupportedException(__FUNCTION__);
}


void
TokenPort::flush()
{
  // NOP
}

bool
TokenPort::canSetCursor() const
{
  return false;
}


//----------------------------------------------------------------------------

FileTokenPort::FileTokenPort(Port<Octet>* port, const String& srcName,
                             CharRegistry* charRegistry)
{
  setTokenizer(new Tokenizer(new CharPort(port), srcName, charRegistry));
}


FileTokenPort::FileTokenPort(Port<Char>* port, const String& srcName,
                             CharRegistry* charRegistry)
{
  setTokenizer(new Tokenizer(port, srcName, charRegistry));
}


void
FileTokenPort::setTokenizer(Tokenizer* tokenizer)
{
  fTokenizer = tokenizer;
}


bool
FileTokenPort::isOpen() const
{
  return fTokenizer != NULL;
}


bool
FileTokenPort::isEof() const
{
  return !hasUnreadData() && (fTokenizer == NULL || fTokenizer->isEof());
}


Token
FileTokenPort::read()
{
  if (fTokenizer == NULL)
    throw PortNotOpenException();

  Token value;
  if (readFromUnreadBuffer(&value, 1) == 1)
    return value;

  return fTokenizer->nextToken();
}


//----------------------------------------------------------------------------

InternalTokenPort::InternalTokenPort(const std::list<Token>& tokens)
{
  fTokens.assign(tokens.begin(), tokens.end());
}


InternalTokenPort::InternalTokenPort(const TokenVector& tokens)
{
  fTokens.assign(tokens.begin(), tokens.end());
}


bool
InternalTokenPort::isOpen() const
{
  return true;
}


bool
InternalTokenPort::isEof() const
{
  return !hasUnreadData() && fTokens.empty();
}


Token
InternalTokenPort::read()
{
  Token value;
  if (readFromUnreadBuffer(&value, 1) == 1)
    return value;

  if (fTokens.empty())
    throw EofException();

  Token t = fTokens.front();
  fTokens.pop_front();
  return t;
}
