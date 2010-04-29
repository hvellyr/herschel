/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "tokenport.h"
#include "unittests.h"

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

FileTokenPort::FileTokenPort(Port<Octet>* port)
{
  setTokenizer(new Tokenizer(new CharPort(port)));
}


FileTokenPort::FileTokenPort(Port<Char>* port)
{
  setTokenizer(new Tokenizer(port));
}


void
FileTokenPort::setTokenizer(Tokenizer* tokenizer)
{
  fTokenizer = tokenizer;
}

FileTokenPort::~FileTokenPort()
{
}


bool
FileTokenPort::isOpen() const
{
  return fTokenizer != NULL;
}


bool
FileTokenPort::isEof() const
{
  return fTokenizer == NULL || fTokenizer->isEof();
}


Token
FileTokenPort::read()
{
  if (fTokenizer == NULL)
    throw PortNotOpenException();
  return fTokenizer->nextToken();
}


//----------------------------------------------------------------------------

InternalTokenPort::InternalTokenPort(const std::list<Token>& tokens)
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
  return fTokens.empty();
}


Token
InternalTokenPort::read()
{
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
    {
      static const char* test =
        "interface zero (\"eyestep/zero 1.0:portables\")\n"
        "  export public(*)\n"
        "-- a simple portable class\n"
        "def class Portable<T>(x @ Int) : (Copyable, Comparable)\n"
        "{\n"
        "  slot first : T = x ;\n"
        "  slot data : Octet[]\n"
        "}\n";
      Ptr<TokenPort> p = new FileTokenPort(new DataPort((Octet*)test, strlen(test)));

      try {
        assert(p->read() == Token(kSymbol, String("interface")));
        assert(p->read() == Token(kSymbol, String("zero")));
        assert(p->read() == Token(kParanOpen));
        assert(p->read() == Token(kString, String("eyestep/zero 1.0:portables")));
        assert(p->read() == Token(kParanClose));

        assert(p->read() == Token(kSymbol, String("export")));
        assert(p->read() == Token(kSymbol, String("public")));
        assert(p->read() == Token(kParanOpen));
        assert(p->read() == Token(kMultiply));
        assert(p->read() == Token(kParanClose));

        assert(p->read() == Token(kSymbol, String("def")));
        assert(p->read() == Token(kSymbol, String("class")));
        assert(p->read() == Token(kSymbol, String("Portable")));
        assert(p->read() == Token(kGenericOpen));
        assert(p->read() == Token(kSymbol, String("T")));
        assert(p->read() == Token(kGenericClose));
        assert(p->read() == Token(kParanOpen));
        assert(p->read() == Token(kSymbol, String("x")));
        assert(p->read() == Token(kAt));
        assert(p->read() == Token(kSymbol, String("Int")));
        assert(p->read() == Token(kParanClose));
        assert(p->read() == Token(kColon));
        assert(p->read() == Token(kParanOpen));
        assert(p->read() == Token(kSymbol, String("Copyable")));
        assert(p->read() == Token(kComma));
        assert(p->read() == Token(kSymbol, String("Comparable")));
        assert(p->read() == Token(kParanClose));

        assert(p->read() == Token(kBraceOpen));
        assert(p->read() == Token(kSymbol, String("slot")));
        assert(p->read() == Token(kSymbol, String("first")));
        assert(p->read() == Token(kColon));
        assert(p->read() == Token(kSymbol, String("T")));
        assert(p->read() == Token(kAssign));
        assert(p->read() == Token(kSymbol, String("x")));

        assert(p->read() == Token(kSemicolon));

        assert(p->read() == Token(kSymbol, String("slot")));
        assert(p->read() == Token(kSymbol, String("data")));
        assert(p->read() == Token(kColon));
        assert(p->read() == Token(kSymbol, String("Octet")));
        assert(p->read() == Token(kBracketOpen));
        assert(p->read() == Token(kBracketClose));
        assert(p->read() == Token(kBraceClose));

        assert(p->isEof());
      }
      catch (const NotationException& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
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
    std::list<Token> tokens;
    tokens.push_back(Token(kSymbol, String("def")));
    tokens.push_back(Token(kSymbol, String("const")));
    tokens.push_back(Token(kSymbol, String("x")));
    tokens.push_back(Token(kAssign));
    tokens.push_back(Token(kRational, 2, 3));

    Ptr<TokenPort> p = new InternalTokenPort(tokens);

    try {
      assert(p->read() == Token(kSymbol, String("def")));
      assert(p->read() == Token(kSymbol, String("const")));
      assert(p->read() == Token(kSymbol, String("x")));
      assert(p->read() == Token(kAssign));
      assert(p->read() == Token(kRational, 2, 3));

      assert(p->isEof());
    }
    catch (const NotationException& ne) {
      fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
    }
  };
};

static InternalTokenPortUnitTest internalTokenPortUnitTest;

#endif  // #if defined(UNITTESTS)

