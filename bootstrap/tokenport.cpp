/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
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
TokenPort::write(Token* /* data */, size_t /* items */)
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


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>


TEST(TokenPort)
{
  static const char* fTest =
    "module zero (\"eyestep/zero 1.0:portables\")\n"
    "  export public(*)\n"
    "-- a simple portable class\n"
    "def class Portable<T>(x @ Int) : (Copyable, Comparable)\n"
    "{\n"
    "  slot first : T = x ;\n"
    "  slot data : Octet[]\n"
    "}\n";

  SrcPos sp;
  Ptr<TokenPort> p = new FileTokenPort(new DataPort((Octet*)fTest, ::strlen(fTest)),
                                         String("test"));
  CHECK_EQUAL(p->read(), Token(sp, kModuleId));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("zero")));
  CHECK_EQUAL(p->read(), Token(sp, kParanOpen));
  CHECK_EQUAL(p->read(), Token(sp, kString, String("eyestep/zero 1.0:portables")));
  CHECK_EQUAL(p->read(), Token(sp, kParanClose));

  CHECK_EQUAL(p->read(), Token(sp, kExportId));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("public")));
  CHECK_EQUAL(p->read(), Token(sp, kParanOpen));
  CHECK_EQUAL(p->read(), Token(sp, kMultiply));
  CHECK_EQUAL(p->read(), Token(sp, kParanClose));

  CHECK_EQUAL(p->read(), Token(sp, kDefId));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("class")));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("Portable")));
  CHECK_EQUAL(p->read(), Token(sp, kGenericOpen));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("T")));
  CHECK_EQUAL(p->read(), Token(sp, kGenericClose));
  CHECK_EQUAL(p->read(), Token(sp, kParanOpen));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("x")));
  CHECK_EQUAL(p->read(), Token(sp, kAt));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("Int")));
  CHECK_EQUAL(p->read(), Token(sp, kParanClose));
  CHECK_EQUAL(p->read(), Token(sp, kColon));
  CHECK_EQUAL(p->read(), Token(sp, kParanOpen));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("Copyable")));
  CHECK_EQUAL(p->read(), Token(sp, kComma));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("Comparable")));
  CHECK_EQUAL(p->read(), Token(sp, kParanClose));

  p->unread(Token(sp, kSymbol, "xyz"));
  p->unread(Token(sp, kAssign));
  p->unread(Token(sp, kInt, 1234));
  CHECK_EQUAL(p->read(), Token(sp, kInt, 1234));
  CHECK_EQUAL(p->read(), Token(sp, kAssign));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, "xyz"));
  CHECK(!p->hasUnreadData());

  CHECK_EQUAL(p->read(), Token(sp, kBraceOpen));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("slot")));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("first")));
  CHECK_EQUAL(p->read(), Token(sp, kColon));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("T")));
  CHECK_EQUAL(p->read(), Token(sp, kAssign));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("x")));

  CHECK_EQUAL(p->read(), Token(sp, kSemicolon));

  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("slot")));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("data")));
  CHECK_EQUAL(p->read(), Token(sp, kColon));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("Octet")));
  CHECK_EQUAL(p->read(), Token(sp, kBracketOpen));
  CHECK_EQUAL(p->read(), Token(sp, kBracketClose));
  CHECK_EQUAL(p->read(), Token(sp, kBraceClose));

  CHECK(p->isEof());
}


TEST(InternalTokenPort)
{
  SrcPos sp;

  std::list<Token> tokens;
  tokens.push_back(Token(sp, kSymbol, String("def")));
  tokens.push_back(Token(sp, kSymbol, String("const")));
  tokens.push_back(Token(sp, kSymbol, String("x")));
  tokens.push_back(Token(sp, kAssign));
  tokens.push_back(Token(sp, kRational, Rational(2, 3)));

  Ptr<TokenPort> p = new InternalTokenPort(tokens);

  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("def")));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("const")));
  CHECK_EQUAL(p->read(), Token(sp, kSymbol, String("x")));
  CHECK_EQUAL(p->read(), Token(sp, kAssign));
  CHECK_EQUAL(p->read(), Token(sp, kRational, Rational(2, 3)));

  CHECK(p->isEof());
}

#endif  // #if defined(UNITTESTS)

