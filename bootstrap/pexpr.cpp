/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "pexpr.h"
#include "str.h"
#include "exception.h"
#include "unittests.h"
#include "parsertypes.h"


namespace heather
{
  String tokenTypeToString(TokenType type)
  {
    switch (type) {
    case kPlus:              return String("+");
    case kAppend:            return String("++");
    case kMinus:             return String("-");
    case kDivide:            return String("/");
    case kMultiply:          return String("*");
    case kExponent:          return String("**");
    case kFold:              return String("%");
    case kCompare:           return String("<=>");
    case kEqual:             return String("==");
    case kUnequal:           return String("<>");
    case kLess:              return String("<");
    case kLessEqual:         return String("<=");
    case kGreater:           return String(">");
    case kGreaterEqual:      return String(">=");
    case kAssign:            return String("=");
    case kMapTo:             return String("->");
    case kIn:                return String("in");
    case kMod:               return String("mod");
    case kIsa:               return String("isa");
    case kAs:                return String("as");
    case kBy:                return String("by");
    case kLogicalAnd:        return String("and");
    case kLogicalOr:         return String("or");
    case kBitAnd:            return String("AND");
    case kBitOr:             return String("OR");
    case kBitXor:            return String("XOR");
    case kShiftLeft:         return String("<<");
    case kShiftRight:        return String(">>");
    case kParanOpen:         return String("(");
    case kParanClose:        return String(")");
    case kBracketOpen:       return String("[");
    case kBracketClose:      return String("]");
    case kBraceOpen:         return String("{");
    case kBraceClose:        return String("}");
    case kGenericOpen:       return String("<");
    case kGenericClose:      return String(">");
    case kComma:             return String(",");
    case kSemicolon:         return String(";");
    case kColon:             return String(":");
    case kAt:                return String("@");
    case kAmpersand:         return String("&");
    case kPipe:              return String("|");
    case kBackQuote:         return String("`");
    case kQuote:             return String("'");
    case kEllipsis:          return String("...");
    case kRange:             return String("..");
    case kDot:               return String(".");
    case kLiteralVectorOpen: return String("#(");
    case kLiteralArrayOpen:  return String("#[");
    case kSangHash:          return String("##");
    default:
      assert(0);
    }
    return String("??");
  }


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


    String toString() const
    {
      return tokenTypeToString(fType);
    }


    virtual void toPort(Port<Octet>* port) const
    {
      display(port, String("<punct type='") + xmlEncode(toString()) + "'/>");
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


    virtual String toString() const
    {
      return fStr;
    }


    //-------- data members
    String fStr;
  };


  //--------------------------------------------------------------------------

  class LitPexprImpl : public PexprImpl
  {
  public:
    LitPexprImpl(TokenType type, const String& value)
      : fType(type),
        fStrValue(value),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
      { }

    LitPexprImpl(TokenType type, const char* value)
      : fType(type),
        fStrValue(String(value)),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
      { }

    LitPexprImpl(TokenType type, int value)
      : fType(type),
        fBoolValue(false),
        fIntValue(value),
        fDoubleValue(0.0),
        fIsImaginary(false)
      {
        assert(type != kBool);
      }


    LitPexprImpl(TokenType type, bool value)
      : fType(type),
        fBoolValue(value),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
      {
        assert(type == kBool);
      }

    LitPexprImpl(TokenType type, double value)
      : fType(type),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(value),
        fIsImaginary(false)
      { }
    
    LitPexprImpl(TokenType type, const Rational& rat)
      : fType(type),
        fBoolValue(false),
        fIntValue(0),
        fRationalValue(rat),
        fDoubleValue(0.0),
        fIsImaginary(false)
      { }

    virtual PexprType type() const
    {
      return kLit;
    }


    virtual bool operator==(const Pexpr& other) const
    {
      if (fType == other.tokenType()) {
        switch (fType) {
        case kString:
          return fStrValue == other.stringLitValue();
        case kKeyword:
          return fStrValue == other.keywLitValue();

        case kChar:
          return fIntValue == int(other.charLitValue());

        case kBool:
          return fBoolValue == other.boolLitValue();
          
        case kInteger:
          return ( fIntValue == other.intLitValue() &&
                   fIsImaginary == other.isLitImaginary() );
        case kReal:
          return ( fDoubleValue == other.realLitValue() &&
                   fIsImaginary == other.isLitImaginary() );
        case kRational:
          return ( fRationalValue == other.rationalLitValue() &&
                   fIsImaginary == other.isLitImaginary() );

        default:
          return true;
        }
      }
      return false;
    }


    virtual void toPort(Port<Octet>* port) const
    {
      String tokstr = xmlEncode(toString());

      switch (fType) {
      case kString:
        display(port, String("<lit type='str'>") + xmlEncode(fStrValue) + "</lit>");
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

    virtual String toString() const
    {
      switch (fType) {
      case kString:     return String("\"") + fStrValue + "\"";
      case kKeyarg:     return fStrValue + ":";
      case kMacroParam: return String("?") + fStrValue;
      case kKeyword:    return String("#") + fStrValue;
      case kBool:       return fBoolValue ? String("true") : String("false");
      case kInteger:    return ( !fIsImaginary 
                                 ? fromInt(fIntValue) 
                                 : (fromInt(fIntValue) + "i") );
      case kReal:       return ( !fIsImaginary 
                                 ? fromDouble(fDoubleValue)
                                 : (fromDouble(fDoubleValue) + "i") );
      case kRational:
      {
        char buffer[128];
        sprintf(buffer, "%d/%d",
                fRationalValue.numerator(), fRationalValue.denominator());
        if (fIsImaginary)
          strcat(buffer, "i");
        return String(buffer);
      }

      case kChar:
      {
        char buffer[32];
        sprintf(buffer, "\\u0%x;", fIntValue);
        return String(buffer);
      }

      default:
        assert(0);
      }
    }      


    //-------- data members
    TokenType fType;
    String    fStrValue;
    bool      fBoolValue;
    int       fIntValue;
    Rational  fRationalValue;
    double    fDoubleValue;
    bool      fIsImaginary;
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


    virtual String toString() const
    {
      String result;

      result = result + "(";

      for (unsigned int i = 0; i < fChildren.size(); i++) {
        String childstr = fChildren[i].toString();
        result = result + childstr + " ";
      }
      result = result + ")";

      return result;
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
      display(port, ( String("<nested left='") + xmlEncode(tokenTypeToString(fLeft))
                      + "' right='" + xmlEncode(tokenTypeToString(fRight)) + "'"));
      if (!fChildren.empty()) {
        display(port, ">");
        for (unsigned int i = 0; i < fChildren.size(); i++)
          fChildren[i].toPort(port);
        display(port, "</nested>");
      }
      else
        display(port, "/>");
    }


    virtual String toString() const
    {
      String result;

      result = result + "{";

      for (unsigned int i = 0; i < fChildren.size(); i++) {
        String childstr = fChildren[i].toString();
        result = result + childstr + " ";
      }
      result = result + "}";

      return result;
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


Pexpr::Pexpr(TokenType type, const String& str)
{
  if (type == kSymbol)
    fImpl = new IdPexprImpl(str);
  else
    fImpl = new LitPexprImpl(type, str);
}


Pexpr::Pexpr(TokenType type, const char* str)
{
  if (type == kSymbol)
    fImpl = new IdPexprImpl(String(str));
  else
    fImpl = new LitPexprImpl(type, String(str));
}


Pexpr::Pexpr(TokenType type, int value)
  : fImpl(new LitPexprImpl(type, value))
{
}


Pexpr::Pexpr(TokenType type, double value)
  : fImpl(new LitPexprImpl(type, value))
{
}


Pexpr::Pexpr(TokenType type, Rational value)
  : fImpl(new LitPexprImpl(type, value))
{
}


Pexpr::Pexpr(TokenType type, bool value)
  : fImpl(new LitPexprImpl(type, value))
{
}


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


TokenType
Pexpr::tokenType() const
{
  switch (fImpl->type()) {
  case kSeq:    return kSeqToken;
  case kNested: return kNestedToken;
  case kId:     return kSymbol;
  case kLit:    return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fType;
  case kPunct:  return dynamic_cast<const PunctPexprImpl*>(fImpl.obj())->fType;
  }
  return kInvalid;
}



String
Pexpr::toString() const
{
  return fImpl->toString();
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


bool
Pexpr::boolLitValue() const
{
  if (tokenType() != kBool)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fBoolValue;
}


int
Pexpr::intLitValue() const
{
  if (tokenType() != kInteger)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fIntValue;
}


double
Pexpr::realLitValue() const
{
  if (tokenType() != kReal)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fDoubleValue;
}


Rational
Pexpr::rationalLitValue() const
{
  if (tokenType() != kRational)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fRationalValue;
}


bool
Pexpr::isLitImaginary() const
{
  if (type() != kLit)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fIsImaginary;
}


Pexpr&
Pexpr::setIsImaginary(bool value)
{
  if (type() != kLit)
    throw NotSupportedException(__FUNCTION__);
  dynamic_cast<LitPexprImpl*>(fImpl.obj())->fIsImaginary = value;
  return *this;
}


String
Pexpr::stringLitValue() const
{
  if (tokenType() != kString)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fStrValue;
}


String
Pexpr::keywLitValue() const
{
  if (tokenType() != kKeyword)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fStrValue;
}


Char
Pexpr::charLitValue() const
{
  if (tokenType() != kChar)
    throw NotSupportedException(__FUNCTION__);
  return Char(dynamic_cast<const LitPexprImpl*>(fImpl.obj())->fIntValue);
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
  return isLit() && tokenType() == kString;
}


bool
Pexpr::isBoolLit() const
{
  return isLit() && tokenType() == kBool;
}


bool
Pexpr::isIntLit() const
{
  return isLit() && tokenType() == kInteger;
}


bool
Pexpr::isRealLit() const
{
  return isLit() && tokenType() == kReal;
}


bool
Pexpr::isRationalLit() const
{
  return isLit() && tokenType() == kRational;
}


bool
Pexpr::isCharLit() const
{
  return isLit() && tokenType() == kChar;
}


bool
Pexpr::isKeyArgLit() const
{
  return (isId() && 
          idValue().length() > 1 &&
          idValue()[idValue().length() - 1] == ':');
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


//----------------------------------------------------------------------------

String
heather::operator+(const String& one, const Pexpr& two)
{
  return one + two.toString();
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class PexprUnitTest : public UnitTest
{
public:
  PexprUnitTest() : UnitTest("Pexpr") {}

  virtual void run()
  {
    assert(Pexpr(kReal,     3.1415)         == Pexpr(kReal,     3.1415));
    assert(Pexpr(kInteger,  12345)          == Pexpr(kInteger,  12345));
    assert(Pexpr(kChar,     0xac00)         == Pexpr(kChar,     0xac00));
    assert(Pexpr(kString,   "abc")          == Pexpr(kString,   "abc"));
    assert(Pexpr(kSymbol,   "abc")          == Pexpr(kSymbol,   "abc"));
    assert(Pexpr(kRational, Rational(7, 4)) == Pexpr(kRational, Rational(7, 4)));

    assert(Pexpr() == Pexpr());
    assert(Pexpr(kParanOpen, kParanClose) == Pexpr(kParanOpen, kParanClose));
    assert(Pexpr() << Pexpr(kInteger, 25) == Pexpr() << Pexpr(kInteger, 25));
    assert(( Pexpr(kParanOpen, kParanClose) << Pexpr(kInteger, 25) ) ==
           ( Pexpr(kParanOpen, kParanClose) << Pexpr(kInteger, 25) ));

    assert(Pexpr(kReal, 3.1415).realLitValue() == 3.1415);
    assert(Pexpr(kReal, 1.2345).tokenType() == kReal);
    assert(Pexpr(kBool, true).boolLitValue() == true);
    assert(Pexpr(kInteger, 0x10000).intLitValue() == 0x10000);
    assert(Pexpr(kRational, Rational(23, 27)).rationalLitValue() == Rational(23, 27));

    // assert(Pexpr(kSymbol, "abc").idValue() == String("abc"));
    assert(Pexpr(kString, String("abc")).stringLitValue() == String("abc"));
    // assert(Pexpr(kKeyword, String("abc")).idValue() == String("abc"));
    // assert(Pexpr(kMacroParam, String("abc")).idValue() == String("abc"));

    // assert(Pexpr(kSymbol, "abc").idValue() == String("abc"));
    assert(Pexpr(kString, "abc").stringLitValue() == String("abc"));
    // assert(Pexpr(kKeyword, "abc").idValue() == String("abc"));
    // assert(Pexpr(kMacroParam, "abc").idValue() == String("abc"));
    assert(Pexpr(kSymbol, "abc:").isKeyArgLit());

#define TEST_ASSIGNOP2(_type, _value, _member)          \
    {                                                   \
      Pexpr t = Pexpr(_type, _value);                   \
      assert(t.tokenType() == _type &&                  \
             t._member() == _value);                    \
    }

    TEST_ASSIGNOP2(kReal, 3.1415, realLitValue);
    TEST_ASSIGNOP2(kBool, true, boolLitValue);
    TEST_ASSIGNOP2(kInteger, 0x20000, intLitValue);
    TEST_ASSIGNOP2(kRational, Rational(1, 127), rationalLitValue);
    TEST_ASSIGNOP2(kString, String("abc"), stringLitValue);
#undef TEST_ASSIGNOP2

#define TEST_COPYCTOR2(_type, _value, _member)          \
    {                                                   \
      Pexpr t(Pexpr(_type, _value));                    \
      assert(t.tokenType() == _type &&                  \
             t._member() == _value);                    \
    }

    TEST_COPYCTOR2(kReal, 3.1415, realLitValue);
    TEST_COPYCTOR2(kBool, true, boolLitValue);
    TEST_COPYCTOR2(kInteger, 0x20000, intLitValue);
    TEST_COPYCTOR2(kRational, Rational(1, 127), rationalLitValue);
    TEST_COPYCTOR2(kString, String("abc"), stringLitValue);
#undef TEST_COPYCTOR2
    
    {
      Pexpr t(Pexpr(kReal, 12.345).setIsImaginary(true));
      assert(t.tokenType() == kReal &&
             t.realLitValue() == 12.345 &&
             t.isLitImaginary());
    }
  }
};
static PexprUnitTest tokenUnitTest;


#endif  // #if defined(UNITTESTS)

