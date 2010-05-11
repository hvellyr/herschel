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

  class IdTokenImpl : public TokenImpl
  {
  public:
    IdTokenImpl(const String& str)
      : fStr(str)
    { }


    virtual bool operator==(const Token& other) const
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

  class NumberTokenImpl : public TokenImpl
  {
  public:
    NumberTokenImpl(TokenType type, int value)
      : fType(type),
        fBoolValue(false),
        fIntValue(value),
        fDoubleValue(0.0),
        fIsImaginary(false)
      {
        assert(type != kBool);
      }


    NumberTokenImpl(TokenType type, bool value)
      : fType(type),
        fBoolValue(value),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
      {
        assert(type == kBool);
      }

    NumberTokenImpl(TokenType type, double value)
      : fType(type),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(value),
        fIsImaginary(false)
      { }

    NumberTokenImpl(TokenType type, const Rational& rat)
      : fType(type),
        fBoolValue(false),
        fIntValue(0),
        fRationalValue(rat),
        fDoubleValue(0.0),
        fIsImaginary(false)
      { }

    virtual bool operator==(const Token& other) const
    {
      if (fType == other.tokenType()) {
        switch (fType) {
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
      case kChar:
        display(port, String("<lit type='char'>") + tokstr + "</lit>");
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
      case kBool:
        return fBoolValue ? String("true") : String("false");
      case kInteger:
        return ( !fIsImaginary
                 ? fromInt(fIntValue)
                 : (fromInt(fIntValue) + "i") );
      case kReal:
        return ( !fIsImaginary
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
    bool      fBoolValue;
    int       fIntValue;
    Rational  fRationalValue;
    double    fDoubleValue;
    bool      fIsImaginary;
  };


  //--------------------------------------------------------------------------

  class StringTokenImpl : public TokenImpl
  {
  public:
    StringTokenImpl(TokenType type, const String& value)
      : fType(type),
        fStrValue(value)
      { }

    virtual bool operator==(const Token& other) const
    {
      return ( fType == other.tokenType() &&
               fStrValue == other.stringLitValue() );
    }


    virtual void toPort(Port<Octet>* port) const
    {
      String tokstr = xmlEncode(toString());

      switch (fType) {
      case kString:
        display(port, String("<lit type='str'>") + xmlEncode(fStrValue) + "</lit>");
        break;
      case kKeyword:
        display(port, String("<lit type='keyw'>") + tokstr + "</lit>");
        break;
      default:
        assert(0);
      }
    }

    virtual String toString() const
    {
      switch (fType) {
      case kString:     return String("\"") + fStrValue + "\"";
      case kKeyword:    return String("#") + fStrValue;
      default:
        assert(0);
      }
    }


    //-------- data members
    TokenType fType;
    String    fStrValue;
  };


  //--------------------------------------------------------------------------

  class SeqTokenImpl : public TokenImpl
  {
  public:
    SeqTokenImpl()
    { }

    virtual bool operator==(const Token& other) const
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


    virtual TokenImpl* unshare()
    {
      Ptr<SeqTokenImpl> copy = new SeqTokenImpl;
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
    TokenVector fChildren;
  };


  //--------------------------------------------------------------------------

  class NestedTokenImpl : public SeqTokenImpl
  {
  public:
    NestedTokenImpl(TokenType left, TokenType right)
      : fLeft(left),
        fRight(right)
    { }


    virtual bool operator==(const Token& other) const
    {
      if (fLeft == other.leftToken() &&
          fRight == other.rightToken())
        return SeqTokenImpl::operator==(other);
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

Token::Token()
  : fType(kSeqExpr),
    fImpl(new SeqTokenImpl)
{ }


Token::Token(TokenType left, TokenType right)
  : fType(kNestedExpr),
    fImpl(new NestedTokenImpl(left, right))
{ }


Token::Token(TokenType ttype)
  : fType(ttype)
{
  assert(type() == kPunct);
}


Token::Token(const String& str)
  : fType(kSymbol),
    fImpl(new IdTokenImpl(str))
{ }


Token::Token(const char* str)
  : fType(kSymbol),
    fImpl(new IdTokenImpl(String(str)))
{ }


Token::Token(TokenType ttype, const String& str)
  : fType(ttype)
{
  if (ttype == kSymbol || ttype == kKeyarg || ttype == kMacroParam)
    fImpl = new IdTokenImpl(str);
  else
    fImpl = new StringTokenImpl(ttype, str);
  assert(type() == kId || type() == kLit);
}


Token::Token(TokenType ttype, const char* str)
  : fType(ttype)
{
  if (ttype == kSymbol || ttype == kKeyarg || ttype == kMacroParam)
    fImpl = new IdTokenImpl(String(str));
  else
    fImpl = new StringTokenImpl(ttype, String(str));
  assert(type() == kId || type() == kLit);
}


Token::Token(TokenType ttype, int value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value))
{
  assert(type() == kLit);
}


Token::Token(TokenType ttype, double value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value))
{
  assert(type() == kLit);
}


Token::Token(TokenType ttype, Rational value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value))
{
  assert(type() == kLit);
}


Token::Token(TokenType ttype, bool value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value))
{
  assert(type() == kLit);
}


Token::Token(const Token& other)
  : fType(other.fType),
    fImpl(other.fImpl)
{ }


Token&
Token::operator=(const Token& other)
{
  fType = other.fType;
  fImpl = other.fImpl;
  return *this;
}


bool
Token::operator==(const Token& other) const
{
  if (fType == other.fType) {
    if (fImpl == other.fImpl)
      return true;
    return (*fImpl.obj()) == other;
  }
  return false;
}


bool
Token::operator!=(const Token& other) const
{
  return !(operator==(other));
}


TokenType
Token::tokenType() const
{
  return fType;
}



String
Token::toString() const
{
  switch (type()) {
  case kSeq:
  case kNested:
  case kLit:
    assert(fImpl != NULL);
    return fImpl->toString();

  case kId:
    switch (fType) {
    case kSymbol:
      return fImpl->toString();
    case kMacroParam:
      return String("?") + dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr;
    case kKeyarg:
      return dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr + ":";
    default:
      assert(0);
    }

  case kPunct:
    return tokenTypeToString(fType);
  }

  return String("??");
}


ExprType
Token::type() const
{
  switch (fType) {
  case kSeqExpr:                // kSeqExpr
    return kSeq;
  case kNestedExpr:             // kNestedExpr
    return kNested;

  case kPlus:                   // kPunctExpr
  case kMinus:
  case kDivide:
  case kMultiply:
  case kExponent:
  case kFold:
  case kCompare:
  case kEqual:
  case kUnequal:
  case kLess:
  case kLessEqual:
  case kGreater:
  case kGreaterEqual:
  case kAssign:
  case kMapTo:
  case kIn:
  case kMod:
  case kIsa:
  case kAs:
  case kBy:
  case kLogicalAnd:
  case kLogicalOr:
  case kBitAnd:
  case kBitOr:
  case kBitXor:
  case kShiftLeft:
  case kShiftRight:
  case kAppend:
  case kParanOpen:
  case kParanClose:
  case kBracketOpen:
  case kBracketClose:
  case kBraceOpen:
  case kBraceClose:
  case kGenericOpen:
  case kGenericClose:
  case kComma:
  case kSemicolon:
  case kColon:
  case kAt:
  case kAmpersand:
  case kPipe:
  case kBackQuote:
  case kQuote:
  case kEllipsis:
  case kRange:
  case kDot:
  case kLiteralVectorOpen:
  case kLiteralArrayOpen:
  case kSangHash:
  case kEOF:
  case kInvalid:
    return kPunct;

  case kString:                 // kLitExpr
  case kChar:
  case kBool:
  case kInteger:
  case kReal:
  case kRational:
  case kKeyword:
    return kLit;

  case kSymbol:                 // kIdExpr
  case kKeyarg:
  case kMacroParam:
    return kId;

  default:
    fprintf(stderr, "Type: %d\n", fType);
    assert(0);
  }
}


bool
Token::isEmpty() const
{
  switch (type()) {
  case kSeq:
  case kNested:
    return children().empty();
  default:
    return false;
  }
}


bool
Token::isSeq() const
{
  return fType == kSeqExpr && fImpl != NULL;
}


bool
Token::isNested() const
{
  return fType == kNestedExpr && fImpl != NULL;
}

bool
Token::isLit() const
{
  return type() == kLit && fImpl != NULL;
}


bool
Token::isId() const
{
  return type() == kId && fImpl != NULL;
}


bool
Token::isSymbol() const
{
  return fType == kSymbol;
}


bool
Token::isPunct() const
{
  return type() == kPunct;
}


bool
Token::isSet() const
{
  return !isSeq() || !isEmpty();
}


const TokenVector&
Token::children() const
{
  switch (type()) {
  case kSeq:
  case kNested:
    assert(fImpl != NULL);
    return dynamic_cast<const SeqTokenImpl*>(fImpl.obj())->fChildren;

  default:
    throw NotSupportedException(__FUNCTION__);
  }
}


TokenVector&
Token::children()
{
  switch (type()) {
  case kSeq:
  case kNested:
    assert(fImpl != NULL);
    unshare();
    return dynamic_cast<SeqTokenImpl*>(fImpl.obj())->fChildren;

  default:
    throw NotSupportedException(__FUNCTION__);
  }
}


void
Token::unshare()
{
  // Write barrier
  if (fImpl != NULL && fImpl->refCount() > 1)
    fImpl = fImpl->unshare();
}


void
Token::addExpr(const Token& expr)
{
  if (type() != kSeq && type() != kNested)
    throw NotSupportedException(__FUNCTION__);

  unshare();
  dynamic_cast<SeqTokenImpl*>(fImpl.obj())->fChildren.push_back(expr);
}


Token&
Token::operator<<(const Token& expr)
{
  addExpr(expr);
  return *this;
}


Token&
Token::operator<<(const TokenVector& exprs)
{
  if (type() != kSeq && type() != kNested)
    throw NotSupportedException(__FUNCTION__);

  unshare();

  SeqTokenImpl* seq = dynamic_cast<SeqTokenImpl*>(fImpl.obj());
  for (TokenVector::const_iterator it = exprs.begin();
       it != exprs.end();
       it++)
  {
    seq->fChildren.push_back(*it);
  }

  return *this;
}


const Token&
Token::operator[](int idx) const
{
  return children()[idx];
}


Token&
Token::operator[](int idx)
{
  unshare();
  return children()[idx];
}


int
Token::count() const
{
  return int(children().size());
}


TokenType
Token::punctValue() const
{
  if (type() != kPunct)
    throw NotSupportedException(__FUNCTION__);
  return fType;
}


String
Token::idValue() const
{
  if (type() != kId)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr;
}


bool
Token::boolLitValue() const
{
  if (fType != kBool)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fBoolValue;
}


int
Token::intLitValue() const
{
  if (fType != kInteger)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fIntValue;
}


double
Token::realLitValue() const
{
  if (fType != kReal)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fDoubleValue;
}


Rational
Token::rationalLitValue() const
{
  if (fType != kRational)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fRationalValue;
}


bool
Token::isLitImaginary() const
{
  if (type() != kLit && fType != kString && fType != kKeyword)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fIsImaginary;
}


Token&
Token::setIsImaginary(bool value)
{
  if (type() != kLit && fType != kString && fType != kKeyword)
    throw NotSupportedException(__FUNCTION__);
  dynamic_cast<NumberTokenImpl*>(fImpl.obj())->fIsImaginary = value;
  return *this;
}


String
Token::stringLitValue() const
{
  if (fType != kString && fType != kKeyword)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const StringTokenImpl*>(fImpl.obj())->fStrValue;
}


Char
Token::charLitValue() const
{
  if (fType != kChar)
    throw NotSupportedException(__FUNCTION__);
  return Char(dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fIntValue);
}


TokenType
Token::leftToken() const
{
  if (fType != kNestedExpr)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NestedTokenImpl*>(fImpl.obj())->fLeft;
}


TokenType
Token::rightToken() const
{
  if (fType != kNestedExpr)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NestedTokenImpl*>(fImpl.obj())->fRight;
}


bool
Token::isBinarySeq(TokenType op) const
{
  return (isSeq() && children().size() == 3 &&
          (*this)[1].isPunct() && (*this)[1].punctValue() == op);
}


bool
Token::isBinarySeq() const
{
  return (isSeq() && children().size() == 3 && (*this)[1].isPunct());
}


OperatorType
Token::binarySeqOperator() const
{
  if (isSeq() && children().size() == 3 && (*this)[1].isPunct())
    return tokenTypeToOperator((*this)[1].punctValue());
  return kOpInvalid;
}


bool
Token::isStringLit() const
{
  return fType == kString;
}


bool
Token::isBoolLit() const
{
  return fType == kBool;
}


bool
Token::isIntLit() const
{
  return fType == kInteger;
}


bool
Token::isRealLit() const
{
  return fType == kReal;
}


bool
Token::isRationalLit() const
{
  return fType == kRational;
}


bool
Token::isCharLit() const
{
  return fType == kChar;
}


bool
Token::isKeyArgLit() const
{
  return (isId() &&
          idValue().length() > 1 &&
          idValue()[idValue().length() - 1] == ':');
}


bool
Token::isSymFuncall() const
{
  return isSeq() && count() == 2 &&
    (*this)[0].isId() && (*this)[1].isNested() &&
    (*this)[1].leftToken() == kParanOpen;
}


bool
Token::isId(const char* sym) const
{
  return isId(String(sym));
}


bool
Token::isId(const String& sym) const
{
  return isId() && dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr == sym;
}


void
Token::toPort(Port<Octet>* port) const
{
  switch (type()) {
  case kSeq:
  case kNested:
  case kLit:
    assert(fImpl != NULL);
    fImpl->toPort(port);
    break;
  case kId:
    switch (fType) {
    case kSymbol:
      return fImpl->toPort(port);
    case kMacroParam:
      display(port, String("<lit type='mparm'>") +
              dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr + "</lit>");
      break;
    case kKeyarg:
      display(port, String("<lit type='keyarg'>") +
              dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr + "</lit>");
      break;
    default:
      assert(0);
    }
  case kPunct:
    display(port, ( String("<punct type='") +
                    xmlEncode(tokenTypeToString(fType)) + "'/>") );
    break;
  }
}


//----------------------------------------------------------------------------

String
heather::operator+(const String& one, const Token& two)
{
  return one + two.toString();
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class TokenUnitTest : public UnitTest
{
public:
  TokenUnitTest() : UnitTest("Token") {}

  virtual void run()
  {
    assert(Token(kReal,     3.1415)         == Token(kReal,     3.1415));
    assert(Token(kInteger,  12345)          == Token(kInteger,  12345));
    assert(Token(kChar,     0xac00)         == Token(kChar,     0xac00));
    assert(Token(kString,   "abc")          == Token(kString,   "abc"));
    assert(Token(kSymbol,   "abc")          == Token(kSymbol,   "abc"));
    assert(Token(kRational, Rational(7, 4)) == Token(kRational, Rational(7, 4)));

    assert(Token() == Token());
    assert(Token(kParanOpen, kParanClose) == Token(kParanOpen, kParanClose));
    assert(Token() << Token(kInteger, 25) == Token() << Token(kInteger, 25));
    assert(( Token(kParanOpen, kParanClose) << Token(kInteger, 25) ) ==
           ( Token(kParanOpen, kParanClose) << Token(kInteger, 25) ));

    assert(Token(kReal, 3.1415).realLitValue() == 3.1415);
    assert(Token(kReal, 1.2345).tokenType() == kReal);
    assert(Token(kBool, true).boolLitValue() == true);
    assert(Token(kInteger, 0x10000).intLitValue() == 0x10000);
    assert(Token(kRational, Rational(23, 27)).rationalLitValue() == Rational(23, 27));

    // assert(Token(kSymbol, "abc").idValue() == String("abc"));
    assert(Token(kString, String("abc")).stringLitValue() == String("abc"));
    // assert(Token(kKeyword, String("abc")).idValue() == String("abc"));
    // assert(Token(kMacroParam, String("abc")).idValue() == String("abc"));

    // assert(Token(kSymbol, "abc").idValue() == String("abc"));
    assert(Token(kString, "abc").stringLitValue() == String("abc"));
    // assert(Token(kKeyword, "abc").idValue() == String("abc"));
    // assert(Token(kMacroParam, "abc").idValue() == String("abc"));
    assert(Token(kSymbol, "abc:").isKeyArgLit());

#define TEST_ASSIGNOP2(_type, _value, _member)          \
    {                                                   \
      Token t = Token(_type, _value);                   \
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
      Token t(Token(_type, _value));                    \
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
      Token t(Token(kReal, 12.345).setIsImaginary(true));
      assert(t.tokenType() == kReal &&
             t.realLitValue() == 12.345 &&
             t.isLitImaginary());
    }
  }
};
static TokenUnitTest tokenUnitTest;


#endif  // #if defined(UNITTESTS)

