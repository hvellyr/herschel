/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "tokenport.h"
#include "unittests.h"
#include "pexpr.h"

using namespace heather;


//----------------------------------------------------------------------------

size_t
TokenPort::write(Pexpr* /* data */, size_t /* items */)
{
  throw NotSupportedException(__FUNCTION__);
}


int
TokenPort::write(Pexpr item)
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

FileTokenPort::FileTokenPort(Port<Octet>* port, CharRegistry* charRegistry)
{
  setTokenizer(new Tokenizer(new CharPort(port), charRegistry));
}


FileTokenPort::FileTokenPort(Port<Char>* port, CharRegistry* charRegistry)
{
  setTokenizer(new Tokenizer(port, charRegistry));
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
  return fTokenizer == NULL || fTokenizer->isEof();
}


Pexpr
FileTokenPort::read()
{
  if (fTokenizer == NULL)
    throw PortNotOpenException();
  return fTokenizer->nextToken();
}


//----------------------------------------------------------------------------

InternalTokenPort::InternalTokenPort(const std::list<Pexpr>& tokens)
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


Pexpr
InternalTokenPort::read()
{
  if (fTokens.empty())
    throw EofException();

  Pexpr t = fTokens.front();
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
        assert(p->read() == Pexpr(kSymbol, String("interface")));
        assert(p->read() == Pexpr(kSymbol, String("zero")));
        assert(p->read() == Pexpr(kParanOpen));
        assert(p->read() == Pexpr(kString, String("eyestep/zero 1.0:portables")));
        assert(p->read() == Pexpr(kParanClose));

        assert(p->read() == Pexpr(kSymbol, String("export")));
        assert(p->read() == Pexpr(kSymbol, String("public")));
        assert(p->read() == Pexpr(kParanOpen));
        assert(p->read() == Pexpr(kMultiply));
        assert(p->read() == Pexpr(kParanClose));

        assert(p->read() == Pexpr(kSymbol, String("def")));
        assert(p->read() == Pexpr(kSymbol, String("class")));
        assert(p->read() == Pexpr(kSymbol, String("Portable")));
        assert(p->read() == Pexpr(kGenericOpen));
        assert(p->read() == Pexpr(kSymbol, String("T")));
        assert(p->read() == Pexpr(kGenericClose));
        assert(p->read() == Pexpr(kParanOpen));
        assert(p->read() == Pexpr(kSymbol, String("x")));
        assert(p->read() == Pexpr(kAt));
        assert(p->read() == Pexpr(kSymbol, String("Int")));
        assert(p->read() == Pexpr(kParanClose));
        assert(p->read() == Pexpr(kColon));
        assert(p->read() == Pexpr(kParanOpen));
        assert(p->read() == Pexpr(kSymbol, String("Copyable")));
        assert(p->read() == Pexpr(kComma));
        assert(p->read() == Pexpr(kSymbol, String("Comparable")));
        assert(p->read() == Pexpr(kParanClose));

        assert(p->read() == Pexpr(kBraceOpen));
        assert(p->read() == Pexpr(kSymbol, String("slot")));
        assert(p->read() == Pexpr(kSymbol, String("first")));
        assert(p->read() == Pexpr(kColon));
        assert(p->read() == Pexpr(kSymbol, String("T")));
        assert(p->read() == Pexpr(kAssign));
        assert(p->read() == Pexpr(kSymbol, String("x")));

        assert(p->read() == Pexpr(kSemicolon));

        assert(p->read() == Pexpr(kSymbol, String("slot")));
        assert(p->read() == Pexpr(kSymbol, String("data")));
        assert(p->read() == Pexpr(kColon));
        assert(p->read() == Pexpr(kSymbol, String("Octet")));
        assert(p->read() == Pexpr(kBracketOpen));
        assert(p->read() == Pexpr(kBracketClose));
        assert(p->read() == Pexpr(kBraceClose));

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
    std::list<Pexpr> tokens;
    tokens.push_back(Pexpr(kSymbol, String("def")));
    tokens.push_back(Pexpr(kSymbol, String("const")));
    tokens.push_back(Pexpr(kSymbol, String("x")));
    tokens.push_back(Pexpr(kAssign));
    tokens.push_back(Pexpr(kRational, Rational(2, 3)));

    Ptr<TokenPort> p = new InternalTokenPort(tokens);

    try {
      assert(p->read() == Pexpr(kSymbol, String("def")));
      assert(p->read() == Pexpr(kSymbol, String("const")));
      assert(p->read() == Pexpr(kSymbol, String("x")));
      assert(p->read() == Pexpr(kAssign));
      assert(p->read() == Pexpr(kRational, Rational(2, 3)));

      assert(p->isEof());
    }
    catch (const NotationException& ne) {
      fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
    }
  };
};

static InternalTokenPortUnitTest internalTokenPortUnitTest;

#endif  // #if defined(UNITTESTS)

