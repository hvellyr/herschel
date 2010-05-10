/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "pexpr.h"
#include "str.h"
#include "exception.h"
#include "unittests.h"


namespace heather
{
  //--------------------------------------------------------------------------

  class PunctPexprImpl : public PexprImpl
  {
  public:
    PunctPexprImpl(TokenType type)
      : fType(type)
    { }


    virtual PexprType type() const
    {
      return kPunct;
    }


    virtual bool operator==(const Pexpr& other) const
    {
      return fType == other.punctValue();
    }


    virtual void toPort(Port<Octet>* port) const
    {
      display(port, String("<punct type='") + xmlEncode(Token(fType).toString()) + "'/>");
    }

    //-------- data members
    TokenType fType;
  };


  //--------------------------------------------------------------------------

  class IdPexprImpl : public PexprImpl
  {
  public:
    IdPexprImpl(const String& str)
      : fStr(str)
    { }

    virtual PexprType type() const
    {
      return kId;
    }


    virtual bool operator==(const Pexpr& other) const
    {
      return fStr == other.idValue();
    }


    virtual void toPort(Port<Octet>* port) const
    {
      display(port, String("<id>") + xmlEncode(fStr) + "</id>");
    }

    //-------- data members
    String fStr;
  };


  //--------------------------------------------------------------------------

  class LitPexprImpl : public PexprImpl
  {
  public:
    LitPexprImpl(const Token& token)
      : fToken(token)
    {}

    virtual PexprType type() const
    {
      return kLit;
    }


    virtual bool operator==(const Pexpr& other) const
    {
      return fToken == other.tokenValue();
    }


    virtual void toPort(Port<Octet>* port) const
    {
      String tokstr = xmlEncode(fToken.toString());

      switch (fToken.fType) {
      case kString:
        display(port, String("<lit type='str'>") + xmlEncode(fToken.fStrValue) + "</lit>");
        break;
      case kChar:
        display(port, String("<lit type='char'>") + tokstr + "</lit>");
        break;
      case kKeyarg:
        display(port, String("<lit type='keyarg'>") + tokstr + "</lit>");
        break;
      case kMacroParam:
        display(port, String("<lit type='mparm'>") + tokstr + "</lit>");
        break;
      case kKeyword:
        display(port, String("<lit type='keyw'>") + tokstr + "</lit>");
        break;
      case kBool:
        display(port, String("<lit type='bool'>") + tokstr + "</lit>");
        break;
      case kInteger:
        display(port, String("<lit type='int'>") + tokstr + "</lit>");
        break;
      case kReal:
        display(port, String("<lit type='real'>") + tokstr + "</lit>");
        break;
      case kRational:
        display(port, String("<lit type='ratio'>") + tokstr + "</lit>");
        break;

      default:
        assert(0);
      }
    }

    //-------- data members
    Token fToken;
  };


  //--------------------------------------------------------------------------

  class SeqPexprImpl : public PexprImpl
  {
  public:
    SeqPexprImpl()
    { }

    virtual PexprType type() const
    {
      return kSeq;
    }

    virtual bool operator==(const Pexpr& other) const
    {
      if (fChildren.size() == other.children().size()) {
        for (size_t i = 0; i < fChildren.size(); i++) {
          if (fChildren[i] != other[i])
            return false;
        }
        return true;
      }
      return false;
    }


    virtual PexprImpl* unshare()
    {
      Ptr<SeqPexprImpl> copy = new SeqPexprImpl;
      copy->fChildren.assign(fChildren.begin(), fChildren.end());
      return copy.release();
    }

    virtual void toPort(Port<Octet>* port) const
    {
      display(port, "<seq>");
      for (unsigned int i = 0; i < fChildren.size(); i++)
        fChildren[i].toPort(port);
      display(port, "</seq>");
    }

    //-------- data members
    PexprVector fChildren;
  };


  //--------------------------------------------------------------------------

  class NestedPexprImpl : public SeqPexprImpl
  {
  public:
    NestedPexprImpl(TokenType left, TokenType right)
      : fLeft(left),
        fRight(right)
    { }

    virtual PexprType type() const
    {
      return kNested;
    }


    virtual bool operator==(const Pexpr& other) const
    {
      if (fLeft == other.leftToken() &&
          fRight == other.rightToken())
        return SeqPexprImpl::operator==(other);
      return false;
    }


    virtual void toPort(Port<Octet>* port) const
    {
      display(port, ( String("<nested left='") + xmlEncode(Token(fLeft).toString())
                      + "' right='" + xmlEncode(Token(fRight).toString()) + "'"));
      if (!fChildren.empty()) {
        display(port, ">");
        for (unsigned int i = 0; i < fChildren.size(); i++)
          fChildren[i].toPort(port);
        display(port, "</nested>");
      }
      else
        display(port, "/>");
    }

    //-------- data members
    TokenType fLeft;
    TokenType fRight;
  };
};



//----------------------------------------------------------------------------

using namespace heather;

Pexpr::Pexpr()
  : fImpl(new SeqPexprImpl)
{ }


Pexpr::Pexpr(TokenType left, TokenType right)
  : fImpl(new NestedPexprImpl(left, right))
{ }


Pexpr::Pexpr(TokenType type)
  : fImpl(new PunctPexprImpl(type))
{ }


Pexpr::Pexpr(const String& str)
  : fImpl(new IdPexprImpl(str))
{ }


Pexpr::Pexpr(const char* str)
  : fImpl(new IdPexprImpl(String(str)))
{ }


Pexpr::Pexpr(const Token& token)
  : fImpl(new LitPexprImpl(token))
{ }


Pexpr::Pexpr(const Pexpr& other)
  : fImpl(other.fImpl)
{ }


Pexpr&
Pexpr::operator=(const Pexpr& other)
{
  fImpl = other.fImpl;
  return *this;
}


bool
Pexpr::operator==(const Pexpr& other) const
{
  if (fImpl == other.fImpl)
    return true;
  return ( type() == other.type() &&
           (*fImpl.obj()) == other);
}


bool
Pexpr::operator!=(const Pexpr& other) const
{
  return !(operator==(other));
}


PexprType
Pexpr::type() const
{
  assert(fImpl != NULL);
  return fImpl->type();
}


bool
Pexpr::isEmpty() const
{
  switch (fImpl->type()) {
  case kSeq:
  case kNested:
    return children().empty();
  default:
    return false;
  }
}


bool
Pexpr::isSeq() const
{
  return fImpl != NULL && fImpl->type() == kSeq;
}


bool
Pexpr::isNested() const
{
  return fImpl != NULL && fImpl->type() == kNested;
}

bool
Pexpr::isLit() const
{
  return fImpl != NULL && fImpl->type() == kLit;
}


bool
Pexpr::isId() const
{
  return fImpl != NULL && fImpl->type() == kId;
}


bool
Pexpr::isPunct() const
{
  return fImpl != NULL && fImpl->type() == kPunct;
}


bool
Pexpr::isSet() const
{
  return !isSeq() || !isEmpty();
}


const PexprVector&
Pexpr::children() const
{
  switch (fImpl->type()) {
  case kSeq:
  case kNested:
    return dynamic_cast<const SeqPexprImpl*>(fImpl.obj())->fChildren;

  default:
    throw NotSupportedException(__FUNCTION__);
  }
}


PexprVector&
Pexpr::children()
{
  switch (fImpl->type()) {
  case kSeq:
  case kNested:
    unshare();
    return dynamic_cast<SeqPexprImpl*>(fImpl.obj())->fChildren;

  default:
    throw NotSupportedException(__FUNCTION__);
  }
}


void
Pexpr::unshare()
{
  // Write barrier
  if (fImpl != NULL && fImpl->refCount() > 1)
    fImpl = fImpl->unshare();
}


void
Pexpr::addExpr(const Pexpr& expr)
{
  if (fImpl->type() != kSeq && fImpl->type() != kNested)
    throw NotSupportedException(__FUNCTION__);

  unshare();

  dynamic_cast<SeqPexprImpl*>(fImpl.obj())->fChildren.push_back(expr);
}


Pexpr&
Pexpr::operator<<(const Pexpr& expr)
{
  addExpr(expr);
  return *this;
}


Pexpr&
Pexpr::operator<<(const PexprVector& exprs)
{
  if (fImpl->type() != kSeq && fImpl->type() != kNested)
    throw NotSupportedException(__FUNCTION__);

  unshare();

  SeqPexprImpl* seq = dynamic_cast<SeqPexprImpl*>(fImpl.obj());
  for (PexprVector::const_iterator it = exprs.begin();
       it != exprs.end();
       it++)
  {
    seq->fChildren.push_back(*it);
  }

  return *this;
}


const Pexpr&
Pexpr::operator[](int idx) const
{
  return children()[idx];
}


Pexpr&
Pexpr::operator[](int idx)
{
  unshare();
  return children()[idx];
}


int
Pexpr::count() const
{
  return int(children().size());
}


TokenType
Pexpr::punctValue() const
{
  if (type() != kPunct)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const PunctPexprImpl*>(fImpl.obj())->fType;
}


String
Pexpr::idValue() const
{
  if (type() != kId)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const IdPexprImpl*>(fImpl.obj())->fStr;
}


Token
Pexpr::tokenValue() const
{
  if (type() != kLit)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fToken;
}


bool
Pexpr::boolLitValue() const
{
  if (tokenValue().fType != kBool)
    throw NotSupportedException(__FUNCTION__);
  return tokenValue().fBoolValue;
}


int
Pexpr::intLitValue() const
{
  if (tokenValue().fType != kInteger)
    throw NotSupportedException(__FUNCTION__);
  return tokenValue().fIntValue;
}


double
Pexpr::realLitValue() const
{
  if (tokenValue().fType != kReal)
    throw NotSupportedException(__FUNCTION__);
  return tokenValue().fDoubleValue;
}


Rational
Pexpr::rationalLitValue() const
{
  if (tokenValue().fType != kRational)
    throw NotSupportedException(__FUNCTION__);
  return tokenValue().fRationalValue;
}


String
Pexpr::stringLitValue() const
{
  if (tokenValue().fType != kString)
    throw NotSupportedException(__FUNCTION__);
  return tokenValue().fStrValue;
}


Char
Pexpr::charLitValue() const
{
  if (tokenValue().fType != kChar)
    throw NotSupportedException(__FUNCTION__);
  return Char(tokenValue().fIntValue);
}


TokenType
Pexpr::leftToken() const
{
  if (type() != kNested)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NestedPexprImpl*>(fImpl.obj())->fLeft;
}


TokenType
Pexpr::rightToken() const
{
  if (type() != kNested)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NestedPexprImpl*>(fImpl.obj())->fRight;
}


bool
Pexpr::isBinarySeq(TokenType op) const
{
  return (isSeq() && children().size() == 3 &&
          (*this)[1].isPunct() && (*this)[1].punctValue() == op);
}


bool
Pexpr::isBinarySeq() const
{
  return (isSeq() && children().size() == 3 && (*this)[1].isPunct());
}


OperatorType
Pexpr::binarySeqOperator() const
{
  if (isSeq() && children().size() == 3 && (*this)[1].isPunct())
    return tokenTypeToOperator((*this)[1].punctValue());
  return kOpInvalid;
}


bool
Pexpr::isStringLit() const
{
  return isLit() && tokenValue().fType == kString;
}


bool
Pexpr::isBoolLit() const
{
  return isLit() && tokenValue().fType == kBool;
}


bool
Pexpr::isIntLit() const
{
  return isLit() && tokenValue().fType == kInteger;
}


bool
Pexpr::isRealLit() const
{
  return isLit() && tokenValue().fType == kReal;
}


bool
Pexpr::isRationalLit() const
{
  return isLit() && tokenValue().fType == kRational;
}


bool
Pexpr::isCharLit() const
{
  return isLit() && tokenValue().fType == kChar;
}


bool
Pexpr::isKeyArgLit() const
{
  return (isLit() && tokenValue().fType == kSymbol &&
          tokenValue().fStrValue.length() > 1 &&
          tokenValue().fStrValue[tokenValue().fStrValue.length() - 1] == ':');
}


bool
Pexpr::isSymFuncall() const
{
  return isSeq() && count() == 2 &&
    (*this)[0].isId() && (*this)[1].isNested() &&
    (*this)[1].leftToken() == kParanOpen;
}


bool
Pexpr::isId(const char* sym) const
{
  return isId(String(sym));
}


bool
Pexpr::isId(const String& sym) const
{
  return isId() && dynamic_cast<const IdPexprImpl*>(fImpl.obj())->fStr == sym;
}


void
Pexpr::toPort(Port<Octet>* port) const
{
  fImpl->toPort(port);
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class PexprUnitTest : public UnitTest
{
public:
  PexprUnitTest() : UnitTest("Pexpr") {}

  virtual void run()
  {
    assert(Pexpr(Token(kReal,     3.1415))         == Pexpr(Token(kReal,     3.1415)));
    assert(Pexpr(Token(kInteger,  12345))          == Pexpr(Token(kInteger,  12345)));
    assert(Pexpr(Token(kChar,     0xac00))         == Pexpr(Token(kChar,     0xac00)));
    assert(Pexpr(Token(kString,   "abc"))          == Pexpr(Token(kString,   "abc")));
    assert(Pexpr(Token(kSymbol,   "abc"))          == Pexpr(Token(kSymbol,   "abc")));
    assert(Pexpr(Token(kRational, Rational(7, 4))) == Pexpr(Token(kRational, Rational(7, 4))));

    assert(Pexpr() == Pexpr());
    assert(Pexpr(kParanOpen, kParanClose) == Pexpr(kParanOpen, kParanClose));
    assert(Pexpr() << Pexpr(Token(kInteger, 25)) == Pexpr() << Pexpr(Token(kInteger, 25)));
    assert(( Pexpr(kParanOpen, kParanClose) << Pexpr(Token(kInteger, 25)) ) ==
           ( Pexpr(kParanOpen, kParanClose) << Pexpr(Token(kInteger, 25)) ));

    assert(Pexpr(Token(kReal, 3.1415)).realLitValue() == 3.1415);
    assert(Pexpr(Token(kReal, 1.2345)).tokenValue().fType == kReal);
    assert(Pexpr(Token(kBool, true)).boolLitValue() == true);
    assert(Pexpr(Token(kInteger, 0x10000)).intLitValue() == 0x10000);
    assert(Pexpr(Token(kRational, Rational(23, 27))).rationalLitValue() == Rational(23, 27));

    // assert(Pexpr(Token(kSymbol, "abc")).stringLitValue() == String("abc"));
    assert(Pexpr(Token(kString, String("abc"))).stringLitValue() == String("abc"));
    // assert(Pexpr(Token(kKeyword, String("abc"))).stringLitValue() == String("abc"));
    // assert(Pexpr(Token(kMacroParam, String("abc"))).stringLitValue() == String("abc"));

    // assert(Pexpr(Token(kSymbol, "abc")).stringLitValue() == String("abc"));
    assert(Pexpr(Token(kString, "abc")).stringLitValue() == String("abc"));
    // assert(Pexpr(Token(kKeyword, "abc")).stringLitValue() == String("abc"));
    // assert(Pexpr(Token(kMacroParam, "abc")).stringLitValue() == String("abc"));
    assert(Pexpr(Token(kSymbol, "abc:")).isKeyArgLit());

#define TEST_ASSIGNOP2(_type, _value, _member)          \
    {                                                   \
      Pexpr t = Pexpr(Token(_type, _value));            \
      assert(t.tokenValue().fType == _type &&           \
             t.tokenValue()._member == _value);         \
    }

    TEST_ASSIGNOP2(kReal, 3.1415, fDoubleValue);
    TEST_ASSIGNOP2(kBool, true, fBoolValue);
    TEST_ASSIGNOP2(kInteger, 0x20000, fIntValue);
    TEST_ASSIGNOP2(kRational, Rational(1, 127), fRationalValue);
    TEST_ASSIGNOP2(kString, String("abc"), fStrValue);
#undef TEST_ASSIGNOP2

#define TEST_COPYCTOR2(_type, _value, _member)          \
    {                                                   \
      Pexpr t(Pexpr(Token(_type, _value)));             \
      assert(t.tokenValue().fType == _type &&           \
             t.tokenValue()._member == _value);         \
    }

    TEST_COPYCTOR2(kReal, 3.1415, fDoubleValue);
    TEST_COPYCTOR2(kBool, true, fBoolValue);
    TEST_COPYCTOR2(kInteger, 0x20000, fIntValue);
    TEST_COPYCTOR2(kRational, Rational(1, 127), fRationalValue);
    TEST_COPYCTOR2(kString, String("abc"), fStrValue);
#undef TEST_COPYCTOR2
    
    {
      Pexpr t(Pexpr(Token(kReal, 12.345).setIsImaginary(true)));
      assert(t.tokenValue().fType == kReal &&
             t.tokenValue().fDoubleValue == 12.345 &&
             t.tokenValue().fIsImaginary);
    }
  }
};
static PexprUnitTest tokenUnitTest;


#endif  // #if defined(UNITTESTS)

