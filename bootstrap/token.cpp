/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <string.h>

#include "token.h"
#include "str.h"
#include "exception.h"
#include "parsertypes.h"
#include "strbuf.h"
#include "symbol.h"

#if defined(UNITTESTS)
#  include <iostream>
#endif


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
    case kMacroOpen:         return String("\343\200\214");
    case kMacroClose:        return String("\343\200\215");
    case kComma:             return String(",");
    case kSemicolon:         return String(";");
    case kColon:             return String(":");
    case kAt:                return String("@");
    case kAmpersand:         return String("&");
    case kPipe:              return String("|");
    case kQuote:             return String("'");
    case kEllipsis:          return String("...");
    case kRange:             return String("..");
    case kDot:               return String(".");
    case kLiteralVectorOpen: return String("#(");
    case kLiteralArrayOpen:  return String("#[");
    case kUnionOpen:         return String("&(");
    case kSangHash:          return String("##");
    case kReference:         return String("^");

    case kEOF:     return String("EOF");
    case kInvalid: return String("INVALID");
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


    virtual bool operator<(const Token& other) const
    {
      assert(other.type() == kId);
      return fStr < dynamic_cast<const IdTokenImpl*>(other.fImpl.obj())->fStr;
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
          return fIntValue == int(other.charValue());

        case kBool:
          return fBoolValue == other.boolValue();

        case kInt:
          return ( fIntValue == other.intValue() &&
                   fIsImaginary == other.isImaginary() );
        case kReal:
          return ( fDoubleValue == other.realValue() &&
                   fIsImaginary == other.isImaginary() );
        case kRational:
          return ( fRationalValue == other.rationalValue() &&
                   fIsImaginary == other.isImaginary() );

        default:
          return true;
        }
      }
      return false;
    }


    virtual bool operator<(const Token& other) const
    {
      if (fType == other.tokenType()) {
        switch (fType) {
        case kChar:
          return fIntValue < int(other.charValue());

        case kBool:
          return fBoolValue < other.boolValue();

        case kInt:
          return ( fIsImaginary == other.isImaginary()
                   ? fIntValue < other.intValue()
                   : false );
        case kReal:
          return ( fIsImaginary == other.isImaginary()
                   ? fDoubleValue < other.realValue()
                   : false );
        case kRational:
          return ( fIsImaginary == other.isImaginary()
                   ? fRationalValue < other.rationalValue()
                   : false );

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
      case kInt:
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
      case kInt:
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
          ::strcat(buffer, "i");
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

      return String();
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
               fStrValue == other.stringValue() );
    }


    virtual bool operator<(const Token& other) const
    {
      return fStrValue < dynamic_cast<const StringTokenImpl*>(
        other.fImpl.obj())->fStrValue;
    }


    virtual void toPort(Port<Octet>* port) const
    {
      String tokstr = xmlEncode(toString());

      switch (fType) {
      case kString:
        display(port, String("<lit type='str'>") + xmlEncode(fStrValue) + "</lit>");
        break;
      case kDocString:
        display(port, String("<lit type='docstr'>") + xmlEncode(fStrValue) + "</lit>");
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
      case kDocString:  return String("~") + fStrValue + "~";
      case kKeyword:    return String("#") + fStrValue;
      default:
        assert(0);
      }
      return String();
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


    virtual bool operator<(const Token& other) const
    {
      return toString() < other.stringValue();
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

      result = result + "%(";

      for (unsigned int i = 0; i < fChildren.size(); i++) {
        String childstr = fChildren[i].toString();
        result = result + childstr + " ";
      }
      result = result + "%)";

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


    virtual bool operator<(const Token& other) const
    {
      return toString() < other.stringValue();
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

      result = result + tokenTypeToString(fLeft);

      for (unsigned int i = 0; i < fChildren.size(); i++) {
        String childstr = fChildren[i].toString();
        result = result + childstr + " ";
      }
      result = result + tokenTypeToString(fRight);

      return result;
    }


    //-------- data members

    TokenType fLeft;
    TokenType fRight;
  };
};



//----------------------------------------------------------------------------

using namespace heather;

Token
Token::newUniqueSymbolToken(const SrcPos& where, const char* prefix)
{
  return Token(where, kSymbol, uniqueName(prefix));
}


Token::Token()
  : fType(kSeqExpr),
    fImpl(new SeqTokenImpl)
{ }


Token::Token(const SrcPos& where, TokenType left, TokenType right)
  : fType(kNestedExpr),
    fImpl(new NestedTokenImpl(left, right)),
    fSrcPos(where)
{ }


Token::Token(const SrcPos& where, TokenType ttype)
  : fType(ttype),
    fSrcPos(where)
{ }


Token::Token(const SrcPos& where, const String& str)
  : fType(kSymbol),
    fImpl(new IdTokenImpl(str)),
    fSrcPos(where)
{ }


Token::Token(const SrcPos& where, const char* str)
  : fType(kSymbol),
    fImpl(new IdTokenImpl(String(str))),
    fSrcPos(where)
{ }


Token::Token(const SrcPos& where, TokenType ttype, const String& str)
  : fType(ttype),
    fSrcPos(where)
{
  if (ttype == kSymbol || ttype == kKeyarg || ttype == kMacroParam ||
      ttype == kMacroParamAsStr)
    fImpl = new IdTokenImpl(str);
  else
    fImpl = new StringTokenImpl(ttype, str);
  assert(type() == kId || type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, const char* str)
  : fType(ttype),
    fSrcPos(where)
{
  if (ttype == kSymbol || ttype == kKeyarg || ttype == kMacroParam ||
      ttype == kMacroParamAsStr)
    fImpl = new IdTokenImpl(String(str));
  else
    fImpl = new StringTokenImpl(ttype, String(str));
  assert(type() == kId || type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, int value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value)),
    fSrcPos(where)
{
  assert(type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, double value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value)),
    fSrcPos(where)
{
  assert(type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, Rational value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value)),
    fSrcPos(where)
{
  assert(type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, bool value)
  : fType(ttype),
    fImpl(new NumberTokenImpl(ttype, value)),
    fSrcPos(where)
{
  assert(type() == kLit);
}


Token::Token(const Token& other)
  : fType(other.fType),
    fImpl(other.fImpl),
    fSrcPos(other.fSrcPos)
{ }


Token&
Token::operator=(const Token& other)
{
  fType = other.fType;
  fImpl = other.fImpl;
  fSrcPos = other.fSrcPos;
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
Token::operator<(const Token& other) const
{
  if (fType == other.fType) {
    switch (type()) {
    case kSeq:
    case kNested:
    case kLit:
      assert(fImpl != NULL);
      return fImpl->operator<(other);
      break;
    case kId:
      switch (fType) {
      case kSymbol:
      case kMacroParam:
      case kMacroParamAsStr:
      case kKeyarg:
        return fImpl->operator<(other);
      case kDefId:
      case kElseId:
      case kEofId:
      case kExportId:
      case kExtendId:
      case kExternId:
      case kForId:
      case kFUNCTIONId:
      case kFunctionId:
      case kIfId:
      case kImportId:
      case kLetId:
      case kMatchId:
      case kModuleId:
      case kNilId:
      case kNotId:
      case kOnId:
      case kReifyId:
      case kSelectId:
      case kThenId:
      case kWhenId:
      case kWhereId:
      case kWhileId:
        return fType < other.fType;
      default:
        assert(0);
      }
      break;

    case kPunct:
      return fType < other.fType;
    }
  }

  return fType < other.fType;
}


bool
Token::operator==(TokenType type) const
{
  return fType == type;
}


bool
Token::operator!=(TokenType type) const
{
  return fType != type;
}


bool
Token::operator!=(const Token& other) const
{
  return !(operator==(other));
}


const SrcPos&
Token::srcpos() const
{
  if (fType == kSeqExpr && !isEmpty())
    return children()[0].srcpos();
  return fSrcPos;
}


Token&
Token::setSrcpos(const SrcPos& srcpos)
{
  fSrcPos = srcpos;
  return *this;
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
    case kMacroParamAsStr:
      return String("?") + dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr;
    case kKeyarg:
      return dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr + ":";

    case kDefId:       return String(MID_DefId);
    case kElseId:      return String(MID_ElseId);
    case kEofId:       return String(MID_EofId);
    case kExportId:    return String(MID_ExportId);
    case kExtendId:    return String(MID_ExtendId);
    case kExternId:    return String(MID_ExternId);
    case kForId:       return String(MID_ForId);
    case kFUNCTIONId:  return String(MID_FUNCTIONId);
    case kFunctionId:  return String(MID_FunctionId);
    case kIfId:        return String(MID_IfId);
    case kImportId:    return String(MID_ImportId);
    case kLetId:       return String(MID_LetId);
    case kMatchId:     return String(MID_MatchId);
    case kModuleId:    return String(MID_ModuleId);
    case kNilId:       return String(MID_NilId);
    case kNotId:       return String(MID_NotId);
    case kOnId:        return String(MID_OnId);
    case kReifyId:     return String(MID_ReifyId);
    case kSelectId:    return String(MID_SelectId);
    case kThenId:      return String(MID_ThenId);
    case kWhenId:      return String(MID_WhenId);
    case kWhereId:     return String(MID_WhereId);
    case kWhileId:     return String(MID_WhileId);

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
  case kMacroOpen:
  case kMacroClose:
  case kComma:
  case kSemicolon:
  case kColon:
  case kAt:
  case kAmpersand:
  case kPipe:
  case kQuote:
  case kEllipsis:
  case kRange:
  case kDot:
  case kLiteralVectorOpen:
  case kLiteralArrayOpen:
  case kUnionOpen:
  case kSangHash:
  case kReference:
  case kEOF:
  case kInvalid:
    return kPunct;

  case kString:                 // kLitExpr
  case kDocString:
  case kChar:
  case kBool:
  case kInt:
  case kReal:
  case kRational:
  case kKeyword:
    return kLit;

  case kSymbol:                 // kIdExpr
  case kKeyarg:
  case kMacroParam:
  case kMacroParamAsStr:
  case kDefId:
  case kElseId:
  case kEofId:
  case kExportId:
  case kExtendId:
  case kExternId:
  case kForId:
  case kFUNCTIONId:
  case kFunctionId:
  case kIfId:
  case kImportId:
  case kLetId:
  case kMatchId:
  case kModuleId:
  case kNilId:
  case kNotId:
  case kOnId:
  case kReifyId:
  case kSelectId:
  case kThenId:
  case kWhenId:
  case kWhereId:
  case kWhileId:
    return kId;

  default:
    fprintf(stderr, "Type: %d\n", fType);
    assert(0);
  }

  return kPunct;
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
Token::isNumber() const
{
  return ( (fType == kInt || fType == kReal || fType == kRational)
           && fImpl != NULL );
}


bool
Token::isId() const
{
  return type() == kId;
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
Token::isOperator() const
{
  return (heather::tokenTypeToOperator(fType) != kOpInvalid);
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


size_t
Token::count() const
{
  return children().size();
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

  switch (fType) {
  case kDefId:
  case kElseId:
  case kEofId:
  case kExportId:
  case kExtendId:
  case kExternId:
  case kForId:
  case kFUNCTIONId:
  case kFunctionId:
  case kIfId:
  case kImportId:
  case kLetId:
  case kMatchId:
  case kModuleId:
  case kNilId:
  case kNotId:
  case kOnId:
  case kReifyId:
  case kSelectId:
  case kThenId:
  case kWhenId:
  case kWhereId:
  case kWhileId:
    return toString();
  case kSymbol:
  case kMacroParam:
  case kMacroParamAsStr:
  case kKeyarg:
    return dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr;
  default:
    assert(0);
  }

  return String();
}


bool
Token::boolValue() const
{
  if (fType != kBool)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fBoolValue;
}


int
Token::intValue() const
{
  if (fType != kInt)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fIntValue;
}


double
Token::realValue() const
{
  if (fType != kReal)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fDoubleValue;
}


Rational
Token::rationalValue() const
{
  if (fType != kRational)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fRationalValue;
}


bool
Token::isImaginary() const
{
  if (type() != kLit &&
      fType != kString && fType != kKeyword && fType != kDocString)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const NumberTokenImpl*>(fImpl.obj())->fIsImaginary;
}


Token&
Token::setIsImaginary(bool value)
{
  if (type() != kLit &&
      fType != kString && fType != kKeyword && fType != kDocString)
    throw NotSupportedException(__FUNCTION__);
  dynamic_cast<NumberTokenImpl*>(fImpl.obj())->fIsImaginary = value;
  return *this;
}


String
Token::stringValue() const
{
  if (fType != kString && fType != kKeyword && fType != kDocString)
    throw NotSupportedException(__FUNCTION__);
  return dynamic_cast<const StringTokenImpl*>(fImpl.obj())->fStrValue;
}


Char
Token::charValue() const
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
          (*this)[1].fType == op);
}


bool
Token::isBinarySeq() const
{
  return (isSeq() && children().size() == 3 && (*this)[1].isOperator());
}


bool
Token::isTernarySeq() const
{
  if (isSeq() && children().size() == 5) {
    return ( ((*this)[1].isPunct() && (*this)[3].isOperator()) ||
             ( (*this)[1] == kThenId && (*this)[3] == kWhileId ) );
  }
  return false;
}


bool
Token::isThenWhileSeq() const
{
  if (isSeq()) {
    if ( (children().size() == 3 && (*this)[1] == kThenId) ||
         (children().size() == 5 && (*this)[1] == kThenId &&
          (*this)[3] == kWhileId) )
      return true;
  }
  return false;
}


bool
Token::isVariableDecl() const
{
  return (tokenType() == kSymbol ||
          ( isSeq() && count() == 3 &&
            (*this)[0] == kSymbol &&
            (*this)[1] == kColon));
}


bool
Token::isRange() const
{
  if (isSeq()) {
    return ( (count() == 3 && (*this)[1] == kRange) ||
             (count() == 5 && (*this)[1] == kRange && (*this)[3] == kBy) );
  }

  return false;
}


OperatorType
Token::binarySeqOperator() const
{
  if (isSeq() && children().size() == 3 && (*this)[1].isPunct())
    return tokenTypeToOperator((*this)[1].punctValue());
  return kOpInvalid;
}


bool
Token::isString() const
{
  return fType == kString;
}


bool
Token::isDocString() const
{
  return fType == kDocString;
}


bool
Token::isBool() const
{
  return fType == kBool;
}


bool
Token::isInt() const
{
  return fType == kInt;
}


bool
Token::isReal() const
{
  return fType == kReal;
}


bool
Token::isRational() const
{
  return fType == kRational;
}


bool
Token::isChar() const
{
  return fType == kChar;
}


bool
Token::isKeyArg() const
{
  return fType == kKeyarg;
}


bool
Token::isNegative() const
{
  switch (fType) {
  case kInt:
    return intValue() < 0;
  case kReal:
    return realValue() < 0;
  case kRational:
    return rationalValue() < Rational(0, 1);
  case kChar:
    return false;

  default:
    assert(0);
  }
  return false;
}


bool
Token::isSymFuncall() const
{
  return isSeq() && count() == 2 &&
    (*this)[0].isSymbol() && (*this)[1].isNested() &&
    (*this)[1].leftToken() == kParanOpen;
}


bool
Token::isConstRange() const
{
  if (isSeq()) {
    const TokenVector& cc = children();

    if (count() == 3) {
      return ( cc[1].fType == kRange && cc[0].isLit() && cc[2].isLit() );
    }
    else if (count() == 5) {
      return ( cc[1].fType == kRange
               && cc[0].isLit() && cc[2].isLit() && cc[4].isLit()
               && cc[3].fType == kBy);
    }
  }

  return false;
}


bool
Token::isQualifiedId() const
{
  if (fType != kSymbol)
    throw NotSupportedException(__FUNCTION__);
  return idValue().indexOf('|') != -1;
}


String
Token::baseName() const
{
  if (fType != kSymbol)
    throw NotSupportedException(__FUNCTION__);

  return heather::baseName(idValue());
}


String
Token::nsName() const
{
  if (fType != kSymbol)
    throw NotSupportedException(__FUNCTION__);

  return heather::nsName(idValue());
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
      display(port, String("<id type='macparm'>") +
              dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr + "</id>");
      break;
    case kMacroParamAsStr:
      display(port, String("<id type='strparm'>") +
              dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr + "</id>");
      break;
    case kKeyarg:
      display(port, String("<id type='keyarg'>") +
              dynamic_cast<const IdTokenImpl*>(fImpl.obj())->fStr + "</id>");
      break;
    case kDefId:
    case kElseId:
    case kEofId:
    case kExportId:
    case kExtendId:
    case kExternId:
    case kForId:
    case kFUNCTIONId:
    case kFunctionId:
    case kIfId:
    case kImportId:
    case kLetId:
    case kMatchId:
    case kModuleId:
    case kNilId:
    case kNotId:
    case kOnId:
    case kReifyId:
    case kSelectId:
    case kThenId:
    case kWhenId:
    case kWhereId:
    case kWhileId:
      display(port, String("<id>") + toString() + "</id>");
      break;
    default:
      assert(0);
    }
    break;

  case kPunct:
    display(port, ( String("<punct type='") +
                    xmlEncode(tokenTypeToString(fType)) + "'/>") );
    break;
  }
}


Token
Token::unwrapSingleton() const
{
  return (isSeq() && children().size() == 1
          ? children()[0]
          : *this);
}


TokenVector
Token::toTokenVector() const
{
  TokenVector vect;
  if (isSet())
    vect.push_back(*this);
  return vect;
}


bool
Token::isCharOrUnitName() const
{
  return (fType == kSymbol ||
          fType == kIn ||
          fType == kLogicalAnd ||
          fType == kLogicalOr ||
          fType == kMod ||
          fType == kBitAnd ||
          fType == kBitOr ||
          fType == kBitXor ||
          fType == kIsa ||
          fType == kAs ||
          fType == kBy ||
          fType == kDefId ||
          fType == kElseId ||
          fType == kEofId ||
          fType == kExportId ||
          fType == kExtendId ||
          fType == kExternId ||
          fType == kForId ||
          fType == kFUNCTIONId ||
          fType == kFunctionId ||
          fType == kIfId ||
          fType == kImportId ||
          fType == kLetId ||
          fType == kMatchId ||
          fType == kModuleId ||
          fType == kNilId ||
          fType == kNotId ||
          fType == kOnId ||
          fType == kReifyId ||
          fType == kSelectId ||
          fType == kThenId ||
          fType == kWhenId ||
          fType == kWhereId ||
          fType == kWhileId);
}


String
Token::macroParamName() const
{
  if (fType != kMacroParam && fType != kMacroParamAsStr)
    throw NotSupportedException(__FUNCTION__);

  String str = idValue();
  int idx = str.indexOf(':');
  if (idx >= 0)
    return str.part(0, idx);
  return str;
}


String
Token::macroParamType() const
{
  if (fType != kMacroParam && fType != kMacroParamAsStr)
    throw NotSupportedException(__FUNCTION__);

  String str = idValue();
  int idx = str.indexOf(':');
  if (idx >= 0)
    return str.part(idx + 1, str.length());
  return String();
}


//----------------------------------------------------------------------------

String
heather::operator+(const String& one, const Token& two)
{
  return one + two.toString();
}


String
heather::operator+(const String& one, const TokenVector& vect)
{
  StringBuffer buf;
  for (TokenVector::const_iterator it = vect.begin();
       it != vect.end();
       it++)
  {
    buf << it->toString() << " ";
  }

  return one + buf.toString();
}


String
heather::operator+(const String& one, const NamedTokenMap& bindings)
{
  StringBuffer buf;
  buf << "{";

  for (NamedTokenMap::const_iterator it = bindings.begin();
       it != bindings.end();
       it++)
  {
    buf << it->first << " -> " << it->second.toString() << "; ";
  }
  buf << "}";

  return one + buf.toString();
}



//----------------------------------------------------------------------------

#if defined(UNITTESTS)

#include <UnitTest++.h>

std::ostream& heather::operator<<(std::ostream& os,const Token& token)
{
  String s = token.toString();
  os << "'" << (const char*)StrHelper(s) << "'";
  return os;
}


std::ostream& heather::operator<<(std::ostream& os, ExprType type)
{
  switch (type) {
  case kSeq:    os << "ExprType::kSeq"; break;
  case kNested: os << "ExprType::kNested"; break;
  case kPunct:  os << "ExprType::kPunct"; break;
  case kLit:    os << "ExprType::kLit"; break;
  case kId:     os << "ExprType::kId"; break;
  }
  return os;
}


SUITE(Token)
{
  TEST(SimpleTokens)
  {
    SrcPos sp;

    CHECK_EQUAL(Token(sp, kReal,     3.1415),         Token(sp, kReal,     3.1415));
    CHECK_EQUAL(Token(sp, kInt,      12345),          Token(sp, kInt,      12345));
    CHECK_EQUAL(Token(sp, kChar,     0xac00),         Token(sp, kChar,     0xac00));
    CHECK_EQUAL(Token(sp, kString,   "abc"),          Token(sp, kString,   "abc"));
    CHECK_EQUAL(Token(sp, kDocString, "abc"),         Token(sp, kDocString, "abc"));
    CHECK_EQUAL(Token(sp, kSymbol,   "abc"),          Token(sp, kSymbol,   "abc"));
    CHECK_EQUAL(Token(sp, kDefId),                    Token(sp, kDefId));
    CHECK_EQUAL(Token(sp, kRational, Rational(7, 4)), Token(sp, kRational, Rational(7, 4)));

    CHECK_EQUAL(Token(sp, kUnionOpen).type(), kPunct);

    CHECK_EQUAL(Token(), Token());
    CHECK_EQUAL(Token(sp, kParanOpen, kParanClose), Token(sp, kParanOpen, kParanClose));
    CHECK_EQUAL(Token(sp, kMacroOpen, kMacroClose), Token(sp, kMacroOpen, kMacroClose));
    CHECK_EQUAL(Token() << Token(sp, kInt, 25),     Token() << Token(sp, kInt, 25));
    CHECK_EQUAL(( Token(sp, kParanOpen, kParanClose) << Token(sp, kInt, 25) ),
                ( Token(sp, kParanOpen, kParanClose) << Token(sp, kInt, 25) ));
    CHECK_EQUAL(( Token(sp, kMacroOpen, kMacroClose) << Token(sp, kInt, 25) ),
                ( Token(sp, kMacroOpen, kMacroClose) << Token(sp, kInt, 25) ));

    CHECK_EQUAL(Token(sp, kReal, 3.1415).realValue(), 3.1415);
    CHECK_EQUAL(Token(sp, kReal, 1.2345).tokenType(), kReal);
    CHECK_EQUAL(Token(sp, kBool, true).boolValue(),   true);
    CHECK_EQUAL(Token(sp, kInt, 0x10000).intValue(),  0x10000);
    CHECK_EQUAL(Token(sp, kRational, Rational(23, 27)).rationalValue(), Rational(23, 27));

    CHECK_EQUAL(Token(sp, kSymbol, "abc").idValue(),                  String("abc"));
    CHECK_EQUAL(Token(sp, kMacroParam, String("abc")).idValue(),      String("abc"));
    CHECK_EQUAL(Token(sp, kMacroParamAsStr, String("abc")).idValue(), String("abc"));
    CHECK_EQUAL(Token(sp, kString, String("abc")).stringValue(),      String("abc"));
    CHECK_EQUAL(Token(sp, kDocString, String("abc")).stringValue(),   String("abc"));
    CHECK_EQUAL(Token(sp, kKeyword, String("abc")).stringValue(),     String("abc"));

    CHECK_EQUAL(Token(sp, kSymbol, "abc").idValue(),          String("abc"));
    CHECK_EQUAL(Token(sp, kMacroParam, "abc").idValue(),      String("abc"));
    CHECK_EQUAL(Token(sp, kMacroParamAsStr, "abc").idValue(), String("abc"));
    CHECK_EQUAL(Token(sp, kString, "abc").stringValue(),      String("abc"));
    CHECK_EQUAL(Token(sp, kDocString, "abc").stringValue(),   String("abc"));
    CHECK_EQUAL(Token(sp, kKeyword, "abc").stringValue(),     String("abc"));
    CHECK(Token(sp, kKeyarg, "abc").isKeyArg());
  }

  TEST(MakeRange)
  {
    SrcPos sp;

#define MAKE_RANGE(_fromty, _fromv, _toty, _tov)  \
    (Token() << Token(sp, _fromty, _fromv)        \
             << Token(sp, kRange)                 \
             << Token(sp, _toty, _tov))
#define MAKE_RANGE_2(_fromty, _fromv, _toty, _tov, _stepty, _stepv) \
    (Token() << Token(sp, _fromty, _fromv)                          \
             << Token(sp, kRange)                                   \
             << Token(sp, _toty, _tov)                              \
             << Token(sp, kBy)                                      \
             << Token(sp, _stepty, _stepv))

    CHECK(MAKE_RANGE(kInt,     0,     kInt,     25).isConstRange());
    CHECK(MAKE_RANGE(kReal,    0.0,   kReal,    25.0).isConstRange());
    CHECK(MAKE_RANGE(kChar,    'a',   kChar,    'z').isConstRange());
    CHECK(MAKE_RANGE(kBool,    false, kBool,    true).isConstRange());
    CHECK(MAKE_RANGE(kString,  "a",   kString,  "z").isConstRange());
    CHECK(MAKE_RANGE(kKeyword, "a",   kKeyword, "z").isConstRange());

    CHECK(MAKE_RANGE_2(kInt,     0,     kInt,     25,   kInt,  2).isConstRange());
    CHECK(MAKE_RANGE_2(kReal,    0.0,   kReal,    25.0, kReal, 0.2).isConstRange());
    CHECK(MAKE_RANGE_2(kChar,    'a',   kChar,    'z',  kInt,  1).isConstRange());
    CHECK(MAKE_RANGE_2(kString,  "a",   kString,  "z",  kInt,  1).isConstRange());
    CHECK(MAKE_RANGE_2(kKeyword, "a",   kKeyword, "z",  kInt,  2).isConstRange());

    CHECK(!(Token(sp, kInt, 5).isConstRange()));
    CHECK(!(Token() << Token(sp, kSymbol, "abc") << Token(sp, kRange)
            << Token(sp, kInt, 27)).isConstRange());
  }

  TEST(AssignOperator2)
  {
    SrcPos sp;

#define TEST_ASSIGNOP2(_type, _value, _member)              \
    {                                                       \
      Token t = Token(sp, _type, _value);                   \
      CHECK_EQUAL(t.tokenType(), _type);                    \
      CHECK_EQUAL(t._member(), _value);                     \
    }

    TEST_ASSIGNOP2(kReal, 3.1415, realValue);
    TEST_ASSIGNOP2(kBool, true, boolValue);
    TEST_ASSIGNOP2(kInt, 0x20000, intValue);
    TEST_ASSIGNOP2(kRational, Rational(1, 127), rationalValue);
    TEST_ASSIGNOP2(kString, String("abc"), stringValue);
#undef TEST_ASSIGNOP2
  }


  TEST(CopyCtor)
  {
    SrcPos sp;

#define TEST_COPYCTOR2(_type, _value, _member)              \
    {                                                       \
      Token t(Token(sp, _type, _value));                    \
      CHECK_EQUAL(t.tokenType(), _type);                    \
      CHECK_EQUAL(t._member(), _value);                     \
    }

    TEST_COPYCTOR2(kReal, 3.1415, realValue);
    TEST_COPYCTOR2(kBool, true, boolValue);
    TEST_COPYCTOR2(kInt, 0x20000, intValue);
    TEST_COPYCTOR2(kRational, Rational(1, 127), rationalValue);
    TEST_COPYCTOR2(kString, String("abc"), stringValue);
#undef TEST_COPYCTOR2
  }


  TEST(Imaginary)
  {
    SrcPos sp;
    Token t(Token(sp, kReal, 12.345).setIsImaginary(true));
    CHECK_EQUAL(t.tokenType(), kReal);
    CHECK_EQUAL(t.realValue(), 12.345);
    CHECK(t.isImaginary());
  }


  TEST(IsQualified1)
  {
    SrcPos sp;
    Token t = Token(sp, kSymbol, "File");
    CHECK(!t.isQualifiedId());
    CHECK_EQUAL(t.baseName(), String("File"));
    CHECK_EQUAL(t.nsName(), String());
  }


  TEST(IsQualified2)
  {
    SrcPos sp;
    Token t = Token(sp, kSymbol, "io|File");
    CHECK(t.isQualifiedId());
    CHECK_EQUAL(t.baseName(), String("File"));
    CHECK_EQUAL(t.nsName(), String("io"));
  }


  TEST(IsQualified3)
  {
    SrcPos sp;
    Token t = Token(sp, kSymbol, "core|rubitz|packs|Package");
    CHECK(t.isQualifiedId());
    CHECK_EQUAL(t.baseName(), String("Package"));
    CHECK_EQUAL(t.nsName(), String("core|rubitz|packs"));
  }


  TEST(UnwrapSingleton)
  {
    SrcPos sp;
    CHECK_EQUAL((Token() << Token(sp, kSymbol, "abc")).unwrapSingleton(),
                Token(sp, kSymbol, "abc"));

    Token t = Token() << Token(sp, kSymbol, "abc")
                      << Token(sp, kInt, 27);
    CHECK_EQUAL(t.unwrapSingleton(), t);

    CHECK_EQUAL(Token().unwrapSingleton(), Token());
    CHECK_EQUAL(Token(sp, kInt, 4).unwrapSingleton(), Token(sp, kInt, 4));
  }


  TEST(MacroNameQualified)
  {
    SrcPos sp;
    Token t = Token(sp, kMacroParam, "abc:name");
    CHECK_EQUAL(t, kMacroParam);
    CHECK_EQUAL(t.macroParamName(), String("abc"));
    CHECK_EQUAL(t.macroParamType(), String("name"));
  }


  TEST(MacroNameWildcard)
  {
    SrcPos sp;
    Token t = Token(sp, kMacroParam, "*:expr");
    CHECK_EQUAL(t, kMacroParam);
    CHECK_EQUAL(t.macroParamName(), String("*"));
    CHECK_EQUAL(t.macroParamType(), String("expr"));
  }


  TEST(MacroNameNameOnly)
  {
    SrcPos sp;
    Token t = Token(sp, kMacroParam, "abc");
    CHECK_EQUAL(t, kMacroParam);
    CHECK_EQUAL(t.macroParamName(), String("abc"));
    CHECK_EQUAL(t.macroParamType(), String());
  }


  TEST(MacroNameAsString)
  {
    SrcPos sp;
    Token t = Token(sp, kMacroParamAsStr, "abc");
    CHECK_EQUAL(t, kMacroParamAsStr);
    CHECK_EQUAL(t.macroParamName(), String("abc"));
    CHECK_EQUAL(t.macroParamType(), String());
  }
}




#endif  // #if defined(UNITTESTS)

