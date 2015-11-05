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

FileTokenPort::FileTokenPort(std::shared_ptr<Port<Octet>> port,
                             const String& srcName,
                             std::shared_ptr<CharRegistry> charRegistry)
{
  setTokenizer(std::make_shared<Tokenizer>(
                 std::make_shared<CharPort>(port), srcName, charRegistry));
}


FileTokenPort::FileTokenPort(std::shared_ptr<Port<Char>> port,
                             const String& srcName,
                             std::shared_ptr<CharRegistry> charRegistry)
{
  setTokenizer(std::make_shared<Tokenizer>(port, srcName, charRegistry));
}


void
FileTokenPort::setTokenizer(std::shared_ptr<Tokenizer> tokenizer)
{
  fTokenizer = tokenizer;
}


bool
FileTokenPort::isOpen() const
{
  return fTokenizer != nullptr;
}


bool
FileTokenPort::isEof() const
{
  return !hasUnreadData() && (!fTokenizer || fTokenizer->isEof());
}


Token
FileTokenPort::read()
{
  if (!fTokenizer)
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
