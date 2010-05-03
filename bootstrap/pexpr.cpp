/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "pexpr.h"
#include "str.h"
#include "exception.h"


namespace heather
{
  //--------------------------------------------------------------------------

  class PunctPexprImpl : public PexprImpl
  {
  public:
    PunctPexprImpl(TokenType type)
      : fType(type)
    { }

    virtual PexprType type() const { return kPunct; }

    virtual void toPort(Port<Octet>* port) const
    {
      display(port, String("<punct type='") + Token(fType) + "'/>");
    }

    //-------- data members
    TokenType fType;
  };


  class IdPexprImpl : public PexprImpl
  {
  public:
    IdPexprImpl(const String& str)
      : fStr(str)
    { }

    virtual PexprType type() const { return kId; }

    virtual void toPort(Port<Octet>* port) const
    {
      display(port, String("<id sym='") + fStr + "'/>");
    }

    //-------- data members
    String fStr;
  };


  class LitPexprImpl : public PexprImpl
  {
  public:
    LitPexprImpl(const Token& token)
      : fToken(token)
    {}

    virtual PexprType type() const { return kLit; }

    virtual void toPort(Port<Octet>* port) const
    {
      display(port, String("<lit>") + fToken + "</lit>");
    }

    //-------- data members
    Token fToken;
  };


  class SeqPexprImpl : public PexprImpl
  {
  public:
    SeqPexprImpl()
    { }

    virtual PexprType type() const { return kSeq; }

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
    std::vector<Pexpr> fChildren;
  };


  class NestedPexprImpl : public SeqPexprImpl
  {
  public:
    NestedPexprImpl(TokenType left, TokenType right)
      : fLeft(left),
        fRight(right)
    { }

    virtual PexprType type() const { return kNested; }

    virtual void toPort(Port<Octet>* port) const
    {
      display(port, ( String("<nested left='") + Token(fLeft) 
                      + "' right='" + Token(fRight) + "'>"));
      for (unsigned int i = 0; i < fChildren.size(); i++)
        fChildren[i].toPort(port);
      display(port, "</nested>");
    }

    //-------- data members
    TokenType fLeft;
    TokenType fRight;
  };
};


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


const std::vector<Pexpr>&
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


std::vector<Pexpr>&
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
Pexpr::strValue() const
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
Pexpr::isBinarySeq(TokenType op) const
{
  return (isSeq() && children().size() == 3 &&
          (*this)[1].isPunct() && (*this)[1].punctValue() == op);
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
