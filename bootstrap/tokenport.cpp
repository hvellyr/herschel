/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <string.h>

#include "tokenport.h"
#include "unittests.h"
#include "port.h"
#include "token.h"
#include "tokenizer.h"

using namespace heather;


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

class FileTokenPortUnitTest : public UnitTest
{
public:
  FileTokenPortUnitTest() : UnitTest("TokenPort") {}

  virtual void run()
  {
    SrcPos sp;

    {
      static const char* test =
        "module zero (\"eyestep/zero 1.0:portables\")\n"
        "  export public(*)\n"
        "-- a simple portable class\n"
        "def class Portable<T>(x @ Int) : (Copyable, Comparable)\n"
        "{\n"
        "  slot first : T = x ;\n"
        "  slot data : Octet[]\n"
        "}\n";
      Ptr<TokenPort> p = new FileTokenPort(new DataPort((Octet*)test, ::strlen(test)),
                                           String("test"));

      assert(p->read() == Token(sp, kModuleId));
      assert(p->read() == Token(sp, kSymbol, String("zero")));
      assert(p->read() == Token(sp, kParanOpen));
      assert(p->read() == Token(sp, kString, String("eyestep/zero 1.0:portables")));
      assert(p->read() == Token(sp, kParanClose));

      assert(p->read() == Token(sp, kExportId));
      assert(p->read() == Token(sp, kSymbol, String("public")));
      assert(p->read() == Token(sp, kParanOpen));
      assert(p->read() == Token(sp, kMultiply));
      assert(p->read() == Token(sp, kParanClose));

      assert(p->read() == Token(sp, kDefId));
      assert(p->read() == Token(sp, kSymbol, String("class")));
      assert(p->read() == Token(sp, kSymbol, String("Portable")));
      assert(p->read() == Token(sp, kGenericOpen));
      assert(p->read() == Token(sp, kSymbol, String("T")));
      assert(p->read() == Token(sp, kGenericClose));
      assert(p->read() == Token(sp, kParanOpen));
      assert(p->read() == Token(sp, kSymbol, String("x")));
      assert(p->read() == Token(sp, kAt));
      assert(p->read() == Token(sp, kSymbol, String("Int")));
      assert(p->read() == Token(sp, kParanClose));
      assert(p->read() == Token(sp, kColon));
      assert(p->read() == Token(sp, kParanOpen));
      assert(p->read() == Token(sp, kSymbol, String("Copyable")));
      assert(p->read() == Token(sp, kComma));
      assert(p->read() == Token(sp, kSymbol, String("Comparable")));
      assert(p->read() == Token(sp, kParanClose));

      p->unread(Token(sp, kSymbol, "xyz"));
      p->unread(Token(sp, kAssign));
      p->unread(Token(sp, kInt, 1234));
      assert(p->read() == Token(sp, kInt, 1234));
      assert(p->read() == Token(sp, kAssign));
      assert(p->read() == Token(sp, kSymbol, "xyz"));
      assert(!p->hasUnreadData());

      assert(p->read() == Token(sp, kBraceOpen));
      assert(p->read() == Token(sp, kSymbol, String("slot")));
      assert(p->read() == Token(sp, kSymbol, String("first")));
      assert(p->read() == Token(sp, kColon));
      assert(p->read() == Token(sp, kSymbol, String("T")));
      assert(p->read() == Token(sp, kAssign));
      assert(p->read() == Token(sp, kSymbol, String("x")));

      assert(p->read() == Token(sp, kSemicolon));

      assert(p->read() == Token(sp, kSymbol, String("slot")));
      assert(p->read() == Token(sp, kSymbol, String("data")));
      assert(p->read() == Token(sp, kColon));
      assert(p->read() == Token(sp, kSymbol, String("Octet")));
      assert(p->read() == Token(sp, kBracketOpen));
      assert(p->read() == Token(sp, kBracketClose));
      assert(p->read() == Token(sp, kBraceClose));

      assert(p->isEof());
    }
  }
};

static FileTokenPortUnitTest fileTokenPortUnitTest;


class InternalTokenPortUnitTest : public UnitTest
{
public:
  InternalTokenPortUnitTest() : UnitTest("InternalTokenPort") {}

  virtual void run()
  {
    SrcPos sp;

    std::list<Token> tokens;
    tokens.push_back(Token(sp, kSymbol, String("def")));
    tokens.push_back(Token(sp, kSymbol, String("const")));
    tokens.push_back(Token(sp, kSymbol, String("x")));
    tokens.push_back(Token(sp, kAssign));
    tokens.push_back(Token(sp, kRational, Rational(2, 3)));

    Ptr<TokenPort> p = new InternalTokenPort(tokens);

    assert(p->read() == Token(sp, kSymbol, String("def")));
    assert(p->read() == Token(sp, kSymbol, String("const")));
    assert(p->read() == Token(sp, kSymbol, String("x")));
    assert(p->read() == Token(sp, kAssign));
    assert(p->read() == Token(sp, kRational, Rational(2, 3)));

    assert(p->isEof());
  };
};

static InternalTokenPortUnitTest internalTokenPortUnitTest;

#endif  // #if defined(UNITTESTS)

