/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "token.hpp"

#include "exception.hpp"
#include "parsertypes.hpp"
#include "predefined.hpp"
#include "str.hpp"
#include "strbuf.hpp"
#include "symbol.hpp"
#include "xmlout.hpp"

#include <string>

#if defined(UNITTESTS)
#  include <iostream>
#endif


namespace herschel {

String tokenTypeToString(TokenType type)
{
  switch (type) {
  case kPlus: return String("+");
  case kConcat: return String("++");
  case kMinus: return String("-");
  case kDivide: return String("/");
  case kMultiply: return String("*");
  case kExponent: return String("**");
  case kFold: return String("%");
  case kCompare: return String("<=>");
  case kEqual: return String("==");
  case kUnequal: return String("<>");
  case kLess: return String("<");
  case kLessEqual: return String("<=");
  case kGreater: return String(">");
  case kGreaterEqual: return String(">=");
  case kAssign: return String("=");
  case kMapTo: return String("->");
  case kIn: return String("in");
  case kMod: return String("mod");
  case kRem: return String("rem");
  case kIsa: return String("isa");
  case kAs: return String("as");
  case kBy: return String("by");
  case kLogicalAnd: return String("and");
  case kLogicalOr: return String("or");
  case kBitAnd: return String("AND");
  case kBitOr: return String("OR");
  case kBitXor: return String("XOR");
  case kShiftLeft: return String("<<");
  case kShiftRight: return String(">>");
  case kParanOpen: return String("(");
  case kParanClose: return String(")");
  case kBracketOpen: return String("[");
  case kBracketClose: return String("]");
  case kBraceOpen: return String("{");
  case kBraceClose: return String("}");
  case kGenericOpen: return String("<");
  case kGenericClose: return String(">");
  case kMacroOpen: return String("\302\253");   // U+00AB / left guillemet
  case kMacroClose: return String("\302\273");  // U+00BB / right guillemet
  case kMacroOpen2: return String("?(");
  case kMacroClose2: return String(")?");
  case kComma: return String(",");
  case kSemicolon: return String(";");
  case kColon: return String(":");
  case kAt: return String("@");
  case kAmpersand: return String("&");
  case kPipe: return String("|");
  case kQuote: return String("'");
  case kEllipsis: return String("...");
  case kRange: return String("..");
  case kDot: return String(".");
  case kLiteralVectorOpen: return String("#(");
  case kLiteralArrayOpen: return String("#[");
  case kSangHash: return String("##");
  case kReference: return String("^");

  case kEOF: return String("EOF");
  case kInvalid: return String("INVALID");
  default: hr_invalid("");
  }
  return String("??");
}


//--------------------------------------------------------------------------

class IdTokenImpl : public TokenImpl {
public:
  IdTokenImpl(const String& str)
      : fStr(str)
  {
  }


  bool operator==(const Token& other) const override { return fStr == other.idValue(); }


  bool operator<(const Token& other) const override
  {
    hr_assert(other.type() == kId);
    return fStr < static_cast<const IdTokenImpl&>(*other.fImpl).fStr;
  }


  void toPort(Port<Octet>& port) const override
  {
    xml::displayTag(port, "id", xmlEncode(fStr));
  }


  String toString() const override { return fStr; }

  String fStr;
};


//--------------------------------------------------------------------------

class NumberTokenImpl : public TokenImpl {
public:
  NumberTokenImpl(TokenType type, int bitwidth, int64_t value)
      : fType(type)
      , fBoolValue(false)
      , fIntValue(value)
      , fDoubleValue(0.0)
      , fIsImaginary(false)
      , fBitWidth(bitwidth)
  {
    hr_assert(type != kBool);
  }


  NumberTokenImpl(TokenType type, bool value)
      : fType(type)
      , fBoolValue(value)
      , fIntValue(0)
      , fDoubleValue(0.0)
      , fIsImaginary(false)
      , fBitWidth(1)
  {
    hr_assert(type == kBool);
  }

  NumberTokenImpl(TokenType type, int bitwidth, double value)
      : fType(type)
      , fBoolValue(false)
      , fIntValue(0)
      , fDoubleValue(value)
      , fIsImaginary(false)
      , fBitWidth(bitwidth)
  {
  }

  NumberTokenImpl(TokenType type, const Rational& rat)
      : fType(type)
      , fBoolValue(false)
      , fIntValue(0)
      , fRationalValue(rat)
      , fDoubleValue(0.0)
      , fIsImaginary(false)
      , fBitWidth(0)
  {
  }


  bool operator==(const Token& other) const override
  {
    if (fType == other.tokenType()) {
      switch (fType) {
      case kChar: return fIntValue == int(other.charValue());

      case kBool: return fBoolValue == other.boolValue();

      case kInt:
      case kUInt:
        return (fIntValue == other.intValue() &&
                //fBitWidth == other.fBitWidth() &&
                fIsImaginary == other.isImaginary());
      case kFloat:
        return (fDoubleValue == other.floatValue() &&
                //fBitWidth == other.fBitWidth() &&
                fIsImaginary == other.isImaginary());
      case kRational:
        return (fRationalValue == other.rationalValue() &&
                fIsImaginary == other.isImaginary());

      default: return true;
      }
    }
    return false;
  }


  bool operator<(const Token& other) const override
  {
    if (fType == other.tokenType()) {
      switch (fType) {
      case kChar: return fIntValue < int(other.charValue());

      case kBool: return fBoolValue < other.boolValue();

      case kInt:
      case kUInt:
        return (fIsImaginary == other.isImaginary() ? fIntValue < other.intValue()
                                                    : false);
      case kFloat:
        return (fIsImaginary == other.isImaginary() ? fDoubleValue < other.floatValue()
                                                    : false);
      case kRational:
        return (fIsImaginary == other.isImaginary()
                    ? fRationalValue < other.rationalValue()
                    : false);

      default: return true;
      }
    }
    return false;
  }


  static zstring intTypeTag(int bitwidth)
  {
    switch (bitwidth) {
    case 8: return "type='int8'";
    case 16: return "type='int16'";
    case 32: return "type='int'";
    case 64: return "type='int64'";
    }
    hr_invalid("");
    return "";
  }

  static zstring uintTypeTag(int bitwidth)
  {
    switch (bitwidth) {
    case 8: return "type='uint8'";
    case 16: return "type='uint16'";
    case 32: return "type='uint'";
    case 64: return "type='uint64'";
    }
    hr_invalid("");
    return "";
  }

  static zstring floatTypeTag(int bitwidth)
  {
    switch (bitwidth) {
    case 32: return "type='float32'";
    case 64: return "type='float'";
    }
    hr_invalid("");
    return "";
  }

  void toPort(Port<Octet>& port) const override
  {
    switch (fType) {
    case kChar: xml::displayTagAttr(port, "lit", "type='char'", toString()); break;
    case kBool: xml::displayTagAttr(port, "lit", "type='bool'", toString()); break;
    case kInt: xml::displayTagAttr(port, "lit", intTypeTag(fBitWidth), toString()); break;
    case kUInt:
      xml::displayTagAttr(port, "lit", uintTypeTag(fBitWidth), toString());
      break;
    case kFloat:
      xml::displayTagAttr(port, "lit", floatTypeTag(fBitWidth), toString());
      break;
    case kRational: xml::displayTagAttr(port, "lit", "type='ratio'", toString()); break;

    default: hr_invalid("");
    }
  }


  String toString() const override
  {
    switch (fType) {
    case kBool: return fBoolValue ? String("true") : String("false");
    case kInt:
    case kUInt:
      // TODO 64bit
      return (!fIsImaginary ? fromInt(fIntValue) : (fromInt(fIntValue) + "i"));
    case kFloat:
      // TODO float/double
      return (!fIsImaginary ? fromDouble(fDoubleValue)
                            : (fromDouble(fDoubleValue) + "i"));
    case kRational: {
      char buffer[128];
      sprintf(buffer, "%d/%d", fRationalValue.numerator(), fRationalValue.denominator());
      if (fIsImaginary)
        ::strcat(buffer, "i");
      return String(buffer);
    }

    case kChar: {
      char buffer[32];
      sprintf(buffer, "\\u0%x;", int(fIntValue));
      return String(buffer);
    }

    default: hr_invalid("");
    }

    return String();
  }

  TokenType fType;
  bool fBoolValue;
  int64_t fIntValue;
  Rational fRationalValue;
  double fDoubleValue;
  bool fIsImaginary;
  int fBitWidth;
};


//--------------------------------------------------------------------------

class StringTokenImpl : public TokenImpl {
public:
  StringTokenImpl(TokenType type, const String& value)
      : fType(type)
      , fStrValue(value)
  {
  }

  bool operator==(const Token& other) const override
  {
    return (fType == other.tokenType() && fStrValue == other.stringValue());
  }


  bool operator<(const Token& other) const override
  {
    return fStrValue < static_cast<const StringTokenImpl&>(*other.fImpl).fStrValue;
  }


  void toPort(Port<Octet>& port) const override
  {
    switch (fType) {
    case kString: xml::displayTagAttr(port, "lit", "type='str'", fStrValue); break;
    case kDocString: xml::displayTagAttr(port, "lit", "type='docstr'", fStrValue); break;
    case kKeyword: xml::displayTagAttr(port, "lit", "type='keyw'", toString()); break;
    default: hr_invalid("");
    }
  }


  String toString() const override
  {
    switch (fType) {
    case kString: return String("\"") + fStrValue + "\"";
    case kDocString: return String("~") + fStrValue + "~";
    case kKeyword: return String("#") + fStrValue;
    default: hr_invalid("");
    }
    return String();
  }

  TokenType fType;
  String fStrValue;
};


//--------------------------------------------------------------------------

class SeqTokenImpl : public TokenImpl {
public:
  SeqTokenImpl() {}

  bool operator==(const Token& other) const override
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


  bool operator<(const Token& other) const override
  {
    return toString() < other.stringValue();
  }


  std::shared_ptr<TokenImpl> unshare(std::shared_ptr<TokenImpl> impl) const override
  {
    auto& seq = static_cast<SeqTokenImpl&>(*impl);
    auto copy = std::make_shared<SeqTokenImpl>();
    copy->fChildren.assign(seq.fChildren.begin(), seq.fChildren.end());
    return copy;
  }


  void toPort(Port<Octet>& port) const override
  {
    xml::displayOpenTag(port, "seq");
    for (unsigned int i = 0; i < fChildren.size(); i++)
      fChildren[i].toPort(port);
    xml::displayCloseTag(port, "seq");
  }


  String toString() const override
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

  TokenVector fChildren;
};


//--------------------------------------------------------------------------

class NestedTokenImpl : public SeqTokenImpl {
public:
  NestedTokenImpl(TokenType left, TokenType right)
      : fLeft(left)
      , fRight(right)
  {
  }


  bool operator==(const Token& other) const override
  {
    if (fLeft == other.leftToken() && fRight == other.rightToken())
      return SeqTokenImpl::operator==(other);
    return false;
  }


  bool operator<(const Token& other) const override
  {
    return toString() < other.stringValue();
  }


  void toPort(Port<Octet>& port) const override
  {
    StringBuffer attrs;
    attrs << "left='" << xmlEncode(tokenTypeToString(fLeft)) << "'"
          << " right='" << xmlEncode(tokenTypeToString(fRight)) << "'";

    if (!fChildren.empty()) {
      xml::displayOpenTagAttrs(port, "nested", StrHelper(attrs.toString()));
      for (unsigned int i = 0; i < fChildren.size(); i++)
        fChildren[i].toPort(port);
      xml::displayCloseTag(port, "nested");
    }
    else
      xml::displayEmptyTagAttrs(port, "nested", StrHelper(attrs.toString()));
  }


  String toString() const override
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

  TokenType fLeft;
  TokenType fRight;
};


//----------------------------------------------------------------------------

Token Token::newUniqueSymbolToken(const SrcPos& where, zstring prefix)
{
  return Token(where, kSymbol, uniqueName(prefix));
}


Token::Token()
    : fType(kSeqExpr)
    , fImpl(new SeqTokenImpl)
{
}


Token::Token(const SrcPos& where, TokenType left, TokenType right)
    : fType(kNestedExpr)
    , fImpl(new NestedTokenImpl(left, right))
    , fSrcPos(where)
{
}


Token::Token(const SrcPos& where, TokenType ttype)
    : fType(ttype)
    , fSrcPos(where)
{
}


Token::Token(const SrcPos& where, const String& str)
    : fType(kSymbol)
    , fImpl(new IdTokenImpl(str))
    , fSrcPos(where)
{
}


Token::Token(const SrcPos& where, zstring str)
    : fType(kSymbol)
    , fImpl(new IdTokenImpl(String(str)))
    , fSrcPos(where)
{
}


Token::Token(const SrcPos& where, TokenType ttype, const String& str)
    : fType(ttype)
    , fSrcPos(where)
{
  if (ttype == kSymbol || ttype == kKeyarg || ttype == kMacroParam ||
      ttype == kMacroParamAsStr)
    fImpl.reset(new IdTokenImpl(str));
  else
    fImpl.reset(new StringTokenImpl(ttype, str));
  hr_assert(type() == kId || type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, zstring str)
    : fType(ttype)
    , fSrcPos(where)
{
  if (ttype == kSymbol || ttype == kKeyarg || ttype == kMacroParam ||
      ttype == kMacroParamAsStr)
    fImpl.reset(new IdTokenImpl(String(str)));
  else
    fImpl.reset(new StringTokenImpl(ttype, String(str)));
  hr_assert(type() == kId || type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, int value)
    : fType(ttype)
    , fImpl(new NumberTokenImpl(ttype, 32, int64_t(value)))
    , fSrcPos(where)
{
  hr_assert(type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, double value)
    : fType(ttype)
    , fImpl(new NumberTokenImpl(ttype, 64, value))
    , fSrcPos(where)
{
  hr_assert(type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, Rational value)
    : fType(ttype)
    , fImpl(new NumberTokenImpl(ttype, value))
    , fSrcPos(where)
{
  hr_assert(type() == kLit);
}


Token::Token(const SrcPos& where, TokenType ttype, bool value)
    : fType(ttype)
    , fImpl(new NumberTokenImpl(ttype, value))
    , fSrcPos(where)
{
  hr_assert(type() == kLit);
}


Token::Token(const Token& other)
    : fType(other.fType)
    , fImpl(other.fImpl)
    , fSrcPos(other.fSrcPos)
{
}


Token Token::newInt(const SrcPos& where, int bitwidth, int64_t value)
{
  Token token;
  token.fSrcPos = where;
  token.fType = kInt;
  token.fImpl.reset(new NumberTokenImpl(kInt, bitwidth, value));
  return token;
}


Token Token::newUInt(const SrcPos& where, int bitwidth, uint64_t value)
{
  Token token;
  token.fSrcPos = where;
  token.fType = kUInt;
  token.fImpl.reset(new NumberTokenImpl(kInt, bitwidth, int64_t(value)));
  return token;
}


Token& Token::operator=(const Token& other)
{
  fType = other.fType;
  fImpl = other.fImpl;
  fSrcPos = other.fSrcPos;
  return *this;
}


bool Token::operator==(const Token& other) const
{
  if (fType == other.fType) {
    if (fImpl == other.fImpl)
      return true;
    return (*fImpl) == other;
  }
  return false;
}


bool Token::operator<(const Token& other) const
{
  if (fType == other.fType) {
    switch (type()) {
    case kSeq:
    case kNested:
    case kLit:
      hr_assert(fImpl);
      return fImpl->operator<(other);
      break;
    case kId:
      switch (fType) {
      case kSymbol:
      case kMacroParam:
      case kMacroParamAsStr:
      case kKeyarg: return fImpl->operator<(other);
      case kDefId:
      case kElseId:
      case kEofId:
      case kExportId:
      case kWithId:
      case kExternId:
      case kForId:
      case kFUNCTIONId:
      case kFunctionId:
      case kIfId:
      case kImportId:
      case kLetId:
      case kMatchId:
      case kModuleId:
      case kNamespaceId:
      case kNilId:
      case kNotId:
      case kNsId:
      case kSelectId:
      case kWhenId:
      case kWhereId:
      case kWhileId: return fType < other.fType;
      default: hr_invalid("");
      }
      break;

    case kPunct: return fType < other.fType;
    }
  }

  return fType < other.fType;
}


bool Token::operator==(TokenType type) const
{
  return fType == type;
}


bool Token::operator!=(TokenType type) const
{
  return fType != type;
}


bool Token::operator!=(const Token& other) const
{
  return !(operator==(other));
}


const SrcPos& Token::srcpos() const
{
  if (fType == kSeqExpr && !isEmpty())
    return children()[0].srcpos();
  return fSrcPos;
}


Token& Token::setSrcpos(const SrcPos& srcpos)
{
  fSrcPos = srcpos;
  return *this;
}


TokenType Token::tokenType() const
{
  return fType;
}


String Token::toString() const
{
  switch (type()) {
  case kSeq:
  case kNested:
  case kLit: hr_assert(fImpl); return fImpl->toString();

  case kId:
    switch (fType) {
    case kSymbol: return fImpl->toString();
    case kMacroParam:
    case kMacroParamAsStr:
      return String("?") + static_cast<const IdTokenImpl&>(*fImpl).fStr;
    case kKeyarg: return static_cast<const IdTokenImpl&>(*fImpl).fStr + ":";

    case kDefId: return String(MID_defid);
    case kElseId: return String(MID_elseid);
    case kEofId: return String(MID_eofid);
    case kExportId: return String(MID_exportid);
    case kExternId: return String(MID_externid);
    case kFUNCTIONId: return String(MID_FUNCTIONid);
    case kForId: return String(MID_forid);
    case kFunctionId: return String(MID_functionid);
    case kIfId: return String(MID_ifid);
    case kImportId: return String(MID_importid);
    case kLetId: return String(MID_letid);
    case kMatchId: return String(MID_matchid);
    case kModuleId: return String(MID_moduleid);
    case kNamespaceId: return String(MID_namespaceid);
    case kNilId: return String(MID_nilid);
    case kNotId: return String(MID_notid);
    case kNsId: return String(MID_nsid);
    case kSelectId: return String(MID_selectid);
    case kWhenId: return String(MID_whenid);
    case kWhereId: return String(MID_whereid);
    case kWhileId: return String(MID_whileid);
    case kWithId: return String(MID_withid);

    default: hr_invalid("");
    }

  case kPunct: return tokenTypeToString(fType);
  }

  return String("??");
}


ExprType Token::type() const
{
  switch (fType) {
  case kSeqExpr:  // kSeqExpr
    return kSeq;
  case kNestedExpr:  // kNestedExpr
    return kNested;

  case kPlus:  // kPunctExpr
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
  case kRem:
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
  case kConcat:
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
  case kMacroOpen2:
  case kMacroClose2:
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
  case kSangHash:
  case kReference:
  case kEOF:
  case kInvalid: return kPunct;

  case kString:  // kLitExpr
  case kDocString:
  case kChar:
  case kBool:
  case kInt:
  case kUInt:
  case kFloat:
  case kRational:
  case kKeyword: return kLit;

  case kDefId:
  case kElseId:
  case kEofId:
  case kExportId:
  case kExternId:
  case kFUNCTIONId:
  case kForId:
  case kFunctionId:
  case kIfId:
  case kImportId:
  case kKeyarg:
  case kLetId:
  case kMacroParam:
  case kMacroParamAsStr:
  case kMatchId:
  case kModuleId:
  case kNamespaceId:
  case kNilId:
  case kNotId:
  case kNsId:
  case kSelectId:
  case kSymbol:  // kIdExpr
  case kWhenId:
  case kWhereId:
  case kWhileId:
  case kWithId: return kId;

  default: fprintf(stderr, "Type: %d\n", fType); hr_invalid("");
  }

  return kPunct;
}


bool Token::isEmpty() const
{
  switch (type()) {
  case kSeq:
  case kNested: return children().empty();
  default: return false;
  }
}


bool Token::isSeq() const
{
  return fType == kSeqExpr && fImpl;
}


bool Token::isNested() const
{
  return fType == kNestedExpr && fImpl;
}


bool Token::isLit() const
{
  return type() == kLit && fImpl;
}


bool Token::isNumber() const
{
  return ((fType == kInt || fType == kUInt || fType == kFloat || fType == kRational) &&
          fImpl);
}


bool Token::isId() const
{
  return type() == kId;
}


bool Token::isSymbol() const
{
  return fType == kSymbol;
}


bool Token::isPunct() const
{
  return type() == kPunct;
}


bool Token::isOperator() const
{
  return (herschel::tokenTypeToOperator(fType) != kOpInvalid);
}


bool Token::isSet() const
{
  return !isSeq() || !isEmpty();
}


const TokenVector& Token::children() const
{
  switch (type()) {
  case kSeq:
  case kNested:
    hr_assert(fImpl);
    return static_cast<const SeqTokenImpl&>(*fImpl).fChildren;

  default: throw NotSupportedException(__FUNCTION__);
  }
}


TokenVector& Token::children()
{
  switch (type()) {
  case kSeq:
  case kNested:
    hr_assert(fImpl);
    unshare();
    return static_cast<SeqTokenImpl&>(*fImpl).fChildren;

  default: throw NotSupportedException(__FUNCTION__);
  }
}


void Token::unshare()
{
  // Write barrier
  if (fImpl && fImpl.use_count() > 1)
    fImpl = fImpl->unshare(fImpl);
}


void Token::addExpr(const Token& expr)
{
  if (type() != kSeq && type() != kNested)
    throw NotSupportedException(__FUNCTION__);

  unshare();
  static_cast<SeqTokenImpl&>(*fImpl).fChildren.push_back(expr);
}


Token& Token::operator<<(const Token& expr)
{
  addExpr(expr);
  return *this;
}


Token& Token::operator<<(const TokenVector& exprs)
{
  if (type() != kSeq && type() != kNested)
    throw NotSupportedException(__FUNCTION__);

  unshare();

  SeqTokenImpl& seq = static_cast<SeqTokenImpl&>(*fImpl);
  for (const auto& tok : exprs) {
    seq.fChildren.push_back(tok);
  }

  return *this;
}


const Token& Token::operator[](int idx) const
{
  return children()[idx];
}


Token& Token::operator[](int idx)
{
  unshare();
  return children()[idx];
}


size_t Token::count() const
{
  return children().size();
}


TokenType Token::punctValue() const
{
  if (type() != kPunct)
    throw NotSupportedException(__FUNCTION__);
  return fType;
}


String Token::idValue() const
{
  if (type() != kId)
    throw NotSupportedException(__FUNCTION__);

  switch (fType) {
  case kDefId:
  case kElseId:
  case kEofId:
  case kExportId:
  case kExternId:
  case kForId:
  case kFUNCTIONId:
  case kFunctionId:
  case kIfId:
  case kImportId:
  case kLetId:
  case kMatchId:
  case kModuleId:
  case kNamespaceId:
  case kNilId:
  case kNotId:
  case kNsId:
  case kSelectId:
  case kWhenId:
  case kWhereId:
  case kWhileId:
  case kWithId: return toString();
  case kSymbol:
  case kMacroParam:
  case kMacroParamAsStr:
  case kKeyarg: return static_cast<const IdTokenImpl&>(*fImpl).fStr;
  default: hr_invalid("");
  }

  return String();
}


bool Token::boolValue() const
{
  if (fType != kBool)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NumberTokenImpl&>(*fImpl).fBoolValue;
}


int64_t Token::intValue() const
{
  if (fType != kInt && fType != kUInt)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NumberTokenImpl&>(*fImpl).fIntValue;
}


int Token::bitwidth() const
{
  if (fType != kInt && fType != kUInt && fType != kFloat)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NumberTokenImpl&>(*fImpl).fBitWidth;
}


double Token::floatValue() const
{
  if (fType != kFloat)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NumberTokenImpl&>(*fImpl).fDoubleValue;
}


Rational Token::rationalValue() const
{
  if (fType != kRational)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NumberTokenImpl&>(*fImpl).fRationalValue;
}


bool Token::isImaginary() const
{
  if (type() != kLit && fType != kString && fType != kKeyword && fType != kDocString)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NumberTokenImpl&>(*fImpl).fIsImaginary;
}


Token& Token::setIsImaginary(bool value)
{
  if (type() != kLit && fType != kString && fType != kKeyword && fType != kDocString)
    throw NotSupportedException(__FUNCTION__);
  static_cast<NumberTokenImpl&>(*fImpl).fIsImaginary = value;
  return *this;
}


String Token::stringValue() const
{
  if (fType != kString && fType != kKeyword && fType != kDocString)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const StringTokenImpl&>(*fImpl).fStrValue;
}


Char Token::charValue() const
{
  if (fType != kChar)
    throw NotSupportedException(__FUNCTION__);
  return Char(static_cast<const NumberTokenImpl&>(*fImpl).fIntValue);
}


TokenType Token::leftToken() const
{
  if (fType != kNestedExpr)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NestedTokenImpl&>(*fImpl).fLeft;
}


TokenType Token::rightToken() const
{
  if (fType != kNestedExpr)
    throw NotSupportedException(__FUNCTION__);
  return static_cast<const NestedTokenImpl&>(*fImpl).fRight;
}


bool Token::isBinarySeq(TokenType op) const
{
  return (isSeq() && children().size() == 3 && (*this)[1].fType == op);
}


bool Token::isBinarySeq() const
{
  return (isSeq() && children().size() == 3 && (*this)[1].isOperator());
}


bool Token::isTernarySeq() const
{
  if (isSeq() && children().size() == 5) {
    return (*this)[1].isPunct() && (*this)[3].isOperator();
  }
  return false;
}


bool Token::isVariableDecl() const
{
  return (tokenType() == kSymbol ||
          (isSeq() && count() == 3 && (*this)[0] == kSymbol && (*this)[1] == kColon));
}


bool Token::isRange() const
{
  if (isSeq()) {
    return ((count() == 3 && (*this)[1] == kRange) ||
            (count() == 5 && (*this)[1] == kRange && (*this)[3] == kBy));
  }

  return false;
}


OperatorType Token::binarySeqOperator() const
{
  if (isSeq() && children().size() == 3 && (*this)[1].isPunct())
    return tokenTypeToOperator((*this)[1].punctValue());
  return kOpInvalid;
}


bool Token::isString() const
{
  return fType == kString;
}


bool Token::isDocString() const
{
  return fType == kDocString;
}


bool Token::isBool() const
{
  return fType == kBool;
}


bool Token::isInt() const
{
  return fType == kInt || fType == kUInt;
}


bool Token::isFloat() const
{
  return fType == kFloat;
}


bool Token::isRational() const
{
  return fType == kRational;
}


bool Token::isChar() const
{
  return fType == kChar;
}


bool Token::isKeyArg() const
{
  return fType == kKeyarg;
}


bool Token::isNegative() const
{
  switch (fType) {
  case kInt: return intValue() < 0;
  case kUInt: return false;
  case kFloat: return floatValue() < 0;
  case kRational: return rationalValue() < Rational(0, 1);
  case kChar: return false;

  default: hr_invalid("");
  }
  return false;
}


bool Token::isSymFuncall() const
{
  return isSeq() && count() == 2 && (*this)[0].isSymbol() && (*this)[1].isNested() &&
         (*this)[1].leftToken() == kParanOpen;
}


bool Token::isConstRange() const
{
  if (isSeq()) {
    const TokenVector& cc = children();

    if (count() == 3) {
      return (cc[1].fType == kRange && cc[0].isLit() && cc[2].isLit());
    }
    else if (count() == 5) {
      return (cc[1].fType == kRange && cc[0].isLit() && cc[2].isLit() && cc[4].isLit() &&
              cc[3].fType == kBy);
    }
  }

  return false;
}


bool Token::isQualifiedId() const
{
  if (fType != kSymbol)
    throw NotSupportedException(__FUNCTION__);
  return herschel::isQualified(idValue());
}


bool Token::isIdWithNamespace() const
{
  if (fType != kSymbol)
    throw NotSupportedException(__FUNCTION__);
  return herschel::hasNamespace(idValue());
}


String Token::baseName() const
{
  if (fType != kSymbol)
    throw NotSupportedException(__FUNCTION__);

  return herschel::baseName(idValue());
}


String Token::nsName() const
{
  if (fType != kSymbol)
    throw NotSupportedException(__FUNCTION__);

  return herschel::nsName(idValue());
}


void Token::toPort(Port<Octet>& port) const
{
  switch (type()) {
  case kSeq:
  case kNested:
  case kLit:
    hr_assert(fImpl);
    fImpl->toPort(port);
    break;
  case kId:
    switch (fType) {
    case kSymbol: return fImpl->toPort(port);
    case kMacroParam:
      xml::displayTagAttr(port, "id", "type='macparm'",
                          static_cast<const IdTokenImpl&>(*fImpl).fStr);
      break;
    case kMacroParamAsStr:
      xml::displayTagAttr(port, "id", "type='strparm'",
                          static_cast<const IdTokenImpl&>(*fImpl).fStr);
      break;
    case kKeyarg:
      xml::displayTagAttr(port, "id", "type='keyarg'",
                          static_cast<const IdTokenImpl&>(*fImpl).fStr);
      break;
    case kDefId:
    case kElseId:
    case kEofId:
    case kExportId:
    case kExternId:
    case kForId:
    case kFUNCTIONId:
    case kFunctionId:
    case kIfId:
    case kImportId:
    case kLetId:
    case kMatchId:
    case kModuleId:
    case kNamespaceId:
    case kNilId:
    case kNotId:
    case kNsId:
    case kSelectId:
    case kWhenId:
    case kWhereId:
    case kWhileId:
    case kWithId: xml::displayTag(port, "id", toString()); break;
    default: hr_invalid("");
    }
    break;

  case kPunct:
    xml::displayEmptyTagAttrs(
        port, "punct",
        StrHelper(String("type='") + xmlEncode(tokenTypeToString(fType)) + "'"));
    break;
  }
}


Token Token::unwrapSingleton() const
{
  return (isSeq() && children().size() == 1 ? children()[0] : *this);
}


TokenVector Token::toTokenVector() const
{
  TokenVector vect;
  if (isSet())
    vect.push_back(*this);
  return vect;
}


bool Token::isCharOrUnitName() const
{
  return (
      fType == kSymbol || fType == kIn || fType == kLogicalAnd || fType == kLogicalOr ||
      fType == kMod || fType == kRem || fType == kBitAnd || fType == kBitOr ||
      fType == kBitXor || fType == kIsa || fType == kAs || fType == kBy ||
      fType == kDefId || fType == kElseId || fType == kEofId || fType == kExportId ||
      fType == kWithId || fType == kNamespaceId || fType == kNsId || fType == kExternId ||
      fType == kForId || fType == kFUNCTIONId || fType == kFunctionId || fType == kIfId ||
      fType == kImportId || fType == kLetId || fType == kMatchId || fType == kModuleId ||
      fType == kNilId || fType == kNotId || fType == kSelectId || fType == kWhenId ||
      fType == kWhereId || fType == kWhileId);
}


String Token::macroParamName() const
{
  if (fType != kMacroParam && fType != kMacroParamAsStr)
    throw NotSupportedException(__FUNCTION__);

  String str = idValue();
  int idx = str.indexOf(':');
  if (idx >= 0)
    return str.part(0, idx);
  return str;
}


String Token::macroParamType() const
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

String operator+(const String& one, const Token& two)
{
  return one + two.toString();
}


String operator+(const String& one, const TokenVector& vect)
{
  StringBuffer buf;
  for (TokenVector::const_iterator it = vect.begin(); it != vect.end(); it++) {
    buf << it->toString() << " ";
  }

  return one + buf.toString();
}


String operator+(const String& one, const NamedTokenMap& bindings)
{
  StringBuffer buf;
  buf << "{";

  for (NamedTokenMap::const_iterator it = bindings.begin(); it != bindings.end(); it++) {
    buf << it->first << " -> " << it->second.toString() << "; ";
  }
  buf << "}";

  return one + buf.toString();
}


zstring operatorName(OperatorType type)
{
  switch (type) {
  case kOpConcat: return "++";
  case kOpAs: return "as";
  case kOpAssign: return "=";
  case kOpBitAnd: return "AND";
  case kOpBitOr: return "OR";
  case kOpBitXor: return "XOR";
  case kOpBy: return "by";
  case kOpCompare: return "<=>";
  case kOpDivide: return "/";
  case kOpEqual: return "==";
  case kOpExponent: return "**";
  case kOpFold: return "%";
  case kOpGreater: return ">";
  case kOpGreaterEqual: return ">=";
  case kOpIn: return "in";
  case kOpIsa: return "isa";
  case kOpLess: return "<";
  case kOpLessEqual: return "<=";
  case kOpLogicalAnd: return "and";
  case kOpLogicalOr: return "or";
  case kOpMapTo: return "->";
  case kOpMinus: return "-";
  case kOpMod: return "mod";
  case kOpRem: return "rem";
  case kOpMultiply: return "*";
  case kOpPlus: return "+";
  case kOpRange: return "..";
  case kOpShiftLeft: return "<<";
  case kOpShiftRight: return ">>";
  case kOpUnequal: return "<>";

  case kOpInvalid: hr_invalid("");
  }

  return nullptr;
}


//----------------------------------------------------------------------------

#if defined(UNITTESTS)

std::ostream& operator<<(std::ostream& os, const Token& token)
{
  String s = token.toString();
  os << "'" << (zstring)StrHelper(s) << "'";
  return os;
}


std::ostream& operator<<(std::ostream& os, ExprType type)
{
  switch (type) {
  case kSeq: os << "ExprType::kSeq"; break;
  case kNested: os << "ExprType::kNested"; break;
  case kPunct: os << "ExprType::kPunct"; break;
  case kLit: os << "ExprType::kLit"; break;
  case kId: os << "ExprType::kId"; break;
  }
  return os;
}

#endif

}  // namespace herschel
