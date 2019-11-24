/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "tokenizer.hpp"

#include "errcodes.hpp"
#include "log.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "registry.hpp"
#include "str.hpp"
#include "strbuf.hpp"

#include <cstring>

namespace herschel {


Tokenizer::Tokenizer(std::shared_ptr<Port<Char>> port, const String& srcName,
                     std::shared_ptr<CharRegistry> charRegistry)
    : fPort(port)
    , fSrcName(srcName)
    , fLineCount(1)
    , fCC(0xffff)
    , fNextCharIsGenericOpen(false)
    , fInGenericContext(0)
    , fCharRegistry(charRegistry)
{
  hr_assert(port);

  nextChar();
}


bool Tokenizer::isEof() const
{
  return !fPort || fPort->isEof();
}


bool Tokenizer::isInCharRange(Char c, Char from, Char to) const
{
  return c >= from && c <= to;
}


bool Tokenizer::isWhitespace(Char c) const
{
  return (c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\f' || c == '\v');
}


bool Tokenizer::isDigit(Char c) const
{
  return isInCharRange(c, '0', '9');
}


bool Tokenizer::isHexDigit(Char c) const
{
  return isInCharRange(c, '0', '9') || isInCharRange(c, 'a', 'f') ||
         isInCharRange(c, 'A', 'F');
}


bool Tokenizer::isAlpha(Char c) const
{
  return isInCharRange(c, 'a', 'z') || isInCharRange(c, 'A', 'Z');
}


bool Tokenizer::isAlphaSpec(Char c) const
{
  return (c == '-' || c == '_' || c == '*' || c == '+' || c == '%' || c == '?' ||
          c == '!' || c == '/');
}


bool Tokenizer::isSymbolChar(Char c) const
{
  return isAlpha(c) || isDigit(c) || isAlphaSpec(c);
}


bool Tokenizer::isDelimiter(Char c) const
{
  return (isWhitespace(c) || c == '"' || c == '\'' || c == '(' || c == ')' || c == '[' ||
          c == ']' || c == '{' || c == '}' || c == 0x300c || c == 0x300d || c == '.' ||
          c == ',' || c == ';' || c == '#' || c == '@' || c == '^' || c == '|');
}


bool Tokenizer::isEOL(Char c) const
{
  return c == '\n' || c == '\r';
}


int Tokenizer::nextChar()
{
  if (fCC == EOF)
    throw AnnotatedEofException(srcpos());

  try {
    int c = fPort->read();
    if (isEOL(c))
      fLineCount++;
    fCC = c;
  }
  catch (const EofException&) {
    fCC = EOF;
  }
  return fCC;
}


void Tokenizer::scanUntilDelimiter()
{
  while (fCC != EOF && !isDelimiter(fCC))
    nextChar();
}


SrcPos Tokenizer::srcpos() const
{
  return SrcPos(fSrcName, fLineCount);
}


void Tokenizer::readCommentLine()
{
  while (!isEOL(fCC))
    nextChar();
}


Token Tokenizer::readIdentifier(const SrcPos& startPos, const String& prefix,
                                TokenType type, bool acceptGenerics)
{
  StringBuffer idBuffer(prefix);

  try {
    while (!isDelimiter(fCC)) {
      if (fCC == '-') {
        nextChar();

        if (fCC == '-') {
          fPort->unread(fCC);
          break;
        }
        else {
          idBuffer << Char('-');
          continue;
        }
      }

      if (acceptGenerics) {
        if (fCC == '<') {
          fNextCharIsGenericOpen = true;
          break;
        }
        else if (fCC == '>')
          break;
      }
      idBuffer << Char(fCC);
      nextChar();
    }
  }
  catch (const EofException&) {
  }

  String identifier = idBuffer.toString();
  if (type == kSymbol && identifier.endsWith(String(":")))
    return Token(startPos, kKeyarg, identifier.part(0, identifier.length() - 1));
  else
    return Token(startPos, type, identifier);
}


Token Tokenizer::makeTokenAndNext(const SrcPos& where, TokenType type)
{
  Token t = Token(where, type);
  nextChar();
  return t;
}


struct ReservedId {
  String fName;
  TokenType fType;
};

Token Tokenizer::readSymbolOrOperator(bool acceptGenerics)
{
  SrcPos startPos = srcpos();
  Char currentChar = fCC;
  nextChar();
  if (isDelimiter(fCC)) {
    switch (currentChar) {
    case '+': return Token(startPos, kPlus);
    case '/': return Token(startPos, kDivide);
    case '*': return Token(startPos, kMultiply);
    case '%': return Token(startPos, kFold);
    case '<': return Token(startPos, kLess);
    case '>': return Token(startPos, kGreater);
    case '=': return Token(startPos, kAssign);
    default: return Token(startPos, String() + currentChar);
    }
  }

  Token token = readIdentifier(startPos, String() + currentChar, kSymbol, acceptGenerics);
  if (token.tokenType() == kSymbol) {
    if (token.idValue() == String("true"))
      return Token(startPos, kBool, true);
    else if (token.idValue() == String("false"))
      return Token(startPos, kBool, false);

    static const ReservedId reservedIds[] = {
        {String("**"), kExponent},
        {String("<=>"), kCompare},
        {String("=="), kEqual},
        {String("<>"), kUnequal},
        {String("<="), kLessEqual},
        {String(">="), kGreaterEqual},
        {String("in"), kIn},
        {String("and"), kLogicalAnd},
        {String("or"), kLogicalOr},
        {String("mod"), kMod},
        {String("rem"), kRem},
        {String("AND"), kBitAnd},
        {String("OR"), kBitOr},
        {String("XOR"), kBitXor},
        {String("<<"), kShiftLeft},
        {String(">>"), kShiftRight},
        {String("isa"), kIsa},
        {String("as"), kAs},
        {String("by"), kBy},
        {String("++"), kConcat},
        {String(MID_FUNCTIONid), kFUNCTIONId},
        {String(MID_defid), kDefId},
        {String(MID_elseid), kElseId},
        {String(MID_eofid), kEofId},
        {String(MID_exportid), kExportId},
        {String(MID_externid), kExternId},
        {String(MID_forid), kForId},
        {String(MID_functionid), kFunctionId},
        {String(MID_ifid), kIfId},
        {String(MID_importid), kImportId},
        {String(MID_includeid), kIncludeId},
        {String(MID_letid), kLetId},
        {String(MID_libraryid), kLibraryId},
        {String(MID_matchid), kMatchId},
        {String(MID_moduleid), kModuleId},
        {String(MID_namespaceid), kNamespaceId},
        {String(MID_nilid), kNilId},
        {String(MID_notid), kNotId},
        {String(MID_nsid), kNsId},
        {String(MID_selectid), kSelectId},
        {String(MID_whenid), kWhenId},
        {String(MID_whereid), kWhereId},
        {String(MID_whileid), kWhileId},
        {String(MID_withid), kWithId},

        {String(), kInvalid},  // sentinel
    };

    String tokenid = token.idValue();
    for (const ReservedId* p = &reservedIds[0]; p->fType != kInvalid; p++) {
      if (tokenid == p->fName)
        return Token(startPos, p->fType);
    }
  }
  return token;
}


String Tokenizer::readIntNumberPart(bool acceptHex)
{
  StringBuffer result;

  while ((acceptHex && isHexDigit(fCC)) || isDigit(fCC)) {
    result << Char(fCC);
    nextChar();
  }

  return result.toString();
}


Token Tokenizer::toInt(const SrcPos& startPos, const String& token, int radix,
                       int bitwidth, bool isUnsigned, int sign)
{
  String tmptok;
  if (!isUnsigned && sign < 0)
    tmptok = String("-") + token;
  else
    tmptok = token;

  if (isUnsigned) {
    uint64_t tmp64 = tmptok.toUInt64(radix);

    switch (bitwidth) {
    case 8:
      if (tmp64 > UINT8_MAX)
        HR_LOG(kError, srcpos(), E_BadNumberNotation) << "Number is out of range";
      return Token::newUInt(startPos, bitwidth, int64_t(uint8_t(tmp64)));

    case 16:
      if (tmp64 > UINT16_MAX)
        HR_LOG(kError, srcpos(), E_BadNumberNotation) << "Number is out of range";
      return Token::newUInt(startPos, bitwidth, int64_t(uint16_t(tmp64)));

    case 32:
      if (tmp64 > UINT32_MAX)
        HR_LOG(kError, srcpos(), E_BadNumberNotation) << "Number is out of range";
      return Token::newUInt(startPos, bitwidth, int64_t(uint32_t(tmp64)));

    case 64: return Token::newUInt(startPos, bitwidth, int64_t(tmp64));
    }
  }
  else {
    int64_t tmp64 = tmptok.toInt64(radix);

    switch (bitwidth) {
    case 8:
      if (tmp64 < INT8_MIN || tmp64 > INT8_MAX)
        HR_LOG(kError, srcpos(), E_BadNumberNotation) << "Number is out of range";
      return Token::newInt(startPos, bitwidth, int64_t(int8_t(tmp64)));

    case 16:
      if (tmp64 < INT16_MIN || tmp64 > INT16_MAX)
        HR_LOG(kError, srcpos(), E_BadNumberNotation) << "Number is out of range";
      return Token::newInt(startPos, bitwidth, int64_t(int16_t(tmp64)));

    case 32:
      if (tmp64 < INT32_MIN || tmp64 > INT32_MAX)
        HR_LOG(kError, srcpos(), E_BadNumberNotation) << "Number is out of range";
      return Token::newInt(startPos, bitwidth, int64_t(int32_t(tmp64)));

    case 64: return Token::newInt(startPos, bitwidth, tmp64);
    }
  }

  hr_invalid("bad number");
  return Token();
}


Token Tokenizer::readNumber(const SrcPos& startPos, int sign)
{
  TokenType type = kInt;

  String first = readIntNumberPart(K(acceptHex));
  String second;
  String exponent;
  int expSign = 1;
  int radix = 10;
  bool isImaginary = false;
  bool isUnsigned = false;
  int bitwidth = 32;

  hr_assert(!first.isEmpty());

  if (fCC == '.') {
    type = kFloat;
    nextChar();
    second = readIntNumberPart(!K(acceptHex));

    if (fCC == 'e' || fCC == 'E') {
      nextChar();
      if (fCC == '-' || fCC == '+') {
        expSign = fCC == '-' ? -1 : 1;
        nextChar();
        exponent = readIntNumberPart(!K(acceptHex));
      }
      else {
        HR_LOG(kError, srcpos(), E_BadNumberNotation)
            << "bad scientific notation: \\u0x" << hex(fCC);
        scanUntilDelimiter();
      }
    }
  }
  else if (fCC == '/') {
    type = kRational;
    nextChar();
    second = readIntNumberPart(K(acceptHex));
  }

  Token suffix = readIdentifier(srcpos(), String(), kSymbol, !K(acceptGenerics));
  if (suffix.type() != kId) {
    HR_LOG(kError, startPos, E_BadNumberNotation) << "unexpected number type annotation";
  }
  else {
    String id = suffix.idValue();
    int idx = 0;
    int len = suffix.idValue().length();

    if (idx < len) {
      if (id[idx] == 'h' || id[idx] == 'H') {
        if (type != kInt)
          HR_LOG(kError, srcpos(), E_BadNumberNotation)
              << "hexadecimal notation for unappropriate number type";
        else
          radix = 16;
        idx++;
      }
      else if (id[idx] == 'q' || id[idx] == 'Q') {
        if (type != kInt)
          HR_LOG(kError, srcpos(), E_BadNumberNotation)
              << "hexadecimal notation for unappropriate number type";
        else
          radix = 8;
        idx++;
      }
    }
    if (idx < len) {
      if (id[idx] == 'y' || id[idx] == 'Y') {
        if (type != kInt)
          HR_LOG(kError, srcpos(), E_BadNumberNotation)
              << "binary notation for unappropriate number type";
        else
          radix = 2;
        idx++;
      }
    }

    if (idx < len) {
      if (id[idx] == 'u' || id[idx] == 'U') {
        if (type != kInt) {
          HR_LOG(kError, srcpos(), E_BadNumberNotation)
              << "unsigned notation for unappropriate number type";
        }
        else {
          isUnsigned = true;
          if (sign < 0)
            HR_LOG(kError, srcpos(), E_BadNumberNotation)
                << "negative unsigned number.  Sign ignored";
        }
        idx++;
      }
    }

    if (idx < len) {
      if (id[idx] == 'l' || id[idx] == 'L') {
        idx++;
        if (type == kInt || type == kFloat)
          bitwidth = 64;
        else
          HR_LOG(kError, srcpos(), E_BadNumberNotation)
              << "long number notation for unappropriate number type";
      }
      else if (id[idx] == 's' || id[idx] == 'S') {
        idx++;
        if (type == kInt)
          bitwidth = 16;
        else
          HR_LOG(kError, srcpos(), E_BadNumberNotation)
              << "long number notation for unappropriate number type";
      }
      else if (id[idx] == 't' || id[idx] == 'T') {
        idx++;
        if (type == kInt)
          bitwidth = 8;
        else
          HR_LOG(kError, srcpos(), E_BadNumberNotation)
              << "long number notation for unappropriate number type";
      }
    }

    if (idx < len) {
      if (id[idx] == 'i' || id[idx] == 'I') {
        idx++;
        isImaginary = true;
      }
    }

    if (idx < len) {
      // there're more chars in the annotation than expected.
      HR_LOG(kError, srcpos(), E_BadNumberNotation)
          << "unexpected number type annotation: " << id[idx];
    }
  }


  Token token;
  switch (type) {
  case kInt: token = toInt(startPos, first, radix, bitwidth, isUnsigned, sign); break;

  case kFloat: {
    StringBuffer tmp(first);
    tmp << "." << second;
    if (!exponent.isEmpty())
      tmp << "e" << (expSign < 0 ? '-' : '+') << exponent;

    // TODO bitwidth
    token = Token(startPos, kFloat, tmp.toString().toDouble() * sign);
  } break;

  case kRational: {
    int fval = first.toInt(10);
    int sval = second.toInt(10);
    token = Token(startPos, kRational, Rational(fval * sign, sval));
  } break;

  default: hr_invalid("");
  }

  token.setIsImaginary(isImaginary);

  return token;
}


Token Tokenizer::readNumericCharacter(const SrcPos& startPos, bool needsTerminator)
{
  Token token = readNumber(startPos, 1);
  if (token.isInt()) {
    int readc = token.intValue();

    Token ct = Token(startPos, kChar, readc);
    if (needsTerminator) {
      if (fCC == ';') {
        nextChar();
        return ct;
      }
      else if (fCC == EOF) {
        HR_LOG(kError, startPos, E_UnterminatedChar) << "file ended before char end";
        return ct;
      }
      else {
        HR_LOG(kError, startPos, E_UnterminatedChar) << "unterminated char";
      }
    }
    else
      return Token(startPos, kChar, readc);
  }
  else {
    HR_LOG(kError, startPos, E_BadCharNotation)
        << "expected integer notation for codepoint";
    return Token(startPos, kChar, 0xffff);
  }

  return Token();
}


Char Tokenizer::mapCharNameToChar(const SrcPos& startPos, const String& charnm)
{
  if (charnm == String("sp") || charnm == String("space"))
    return Char(' ');
  else if (charnm == String("nl") || charnm == String("newline"))
    return Char('\n');
  else if (charnm == String("cr") || charnm == String("return"))
    return Char('\r');
  else if (charnm == String("ht") || charnm == String("tab"))
    return Char('\t');
  else if (charnm == String("esc") || charnm == String("escape"))
    return Char('\e');
  else if (fCharRegistry) {
    int cp;
    if (fCharRegistry->lookup(charnm, &cp)) {
      if (cp >= 0 && cp <= 0x10ffff)
        return Char(cp);
    }
  }

  HR_LOG(kError, startPos, E_UnknownCharName) << "Unknown char name: " << charnm;
  return 0xffff;
}


Token Tokenizer::translateChar(const SrcPos& startPos, const String& charnm)
{
  Char c = mapCharNameToChar(startPos, charnm);
  return Token(startPos, kChar, c);
}


Token Tokenizer::readSymbolCharacter(const SrcPos& startPos, bool needsTerminator)
{
  Token sym = readIdentifier(startPos, String(), kSymbol, !K(acceptGenerics));
  if (sym.type() != kId) {
    HR_LOG(kError, startPos, E_ExpectedCharName) << "expected character symbol";
    return sym;
  }

  Token ct = (sym.idValue().length() == 1 ? Token(startPos, kChar, sym.idValue()[0])
                                          : translateChar(startPos, sym.idValue()));
  if (needsTerminator) {
    if (fCC == ';') {
      nextChar();
      return ct;
    }
    else if (fCC == EOF) {
      HR_LOG(kError, startPos, E_UnterminatedChar) << "file ended before char end";
      return ct;
    }
    else {
      HR_LOG(kError, startPos, E_UnterminatedChar) << "unterminated char";
    }
  }

  return ct;
}


Token Tokenizer::readNamedCharacter(const SrcPos& startPos, bool needsTerminator)
{
  if (isAlpha(fCC) || isDigit(fCC))
    return readSymbolCharacter(startPos, needsTerminator);
  int c = fCC;
  nextChar();
  return Token(startPos, kChar, c);
}


Token Tokenizer::readCharacter(const SrcPos& startPos, bool needsTerminator)
{
  if (fCC == 'u') {
    nextChar();
    if (isDigit(fCC))
      return readNumericCharacter(startPos, needsTerminator);
    else if (!isDelimiter(fCC)) {
      HR_LOG(kError, startPos, E_UnterminatedChar) << "expected numerical char notation";
      scanUntilDelimiter();
      // assume any character, simply to allow continue parsing
      return Token(startPos, kChar, 0xffff);
    }
    else
      return Token(startPos, kChar, 'u');
  }
  else if (!isWhitespace(fCC))
    return readNamedCharacter(startPos, needsTerminator);
  else {
    HR_LOG(kError, startPos, E_UnterminatedChar) << "incomplete char notation";
    return Token(startPos, kChar, 0xffff);
  }
}


Token Tokenizer::readString(const SrcPos& startPos, int endChar, TokenType type)
{
  StringBuffer result;

  try {
    for (;;) {
      if (fCC == endChar) {
        nextChar();
        return Token(startPos, type, result.toString());
      }
      else if (fCC == '\\') {
        SrcPos charSp = srcpos();
        nextChar();
        Token ct = readCharacter(charSp, K(needsTerminator));
        if (ct == kChar)
          result << ct.charValue();
      }
      else {
        result << Char(fCC);
        nextChar();
      }
    }
  }
  catch (const AnnotatedEofException& ae) {
    HR_LOG(kError, ae.srcpos(), E_UnterminatedString)
        << "File ended before end of string.  Began on line " << startPos.lineNumber();
    HR_LOG(kError, startPos), "String started here";
    throw;
  }

  return Token();
}


Token Tokenizer::nextToken()
{
  Token t = nextTokenImpl();
  if (Properties::isTraceTokenizer()) {
    printf("%s ", (zstring)StrHelper(t.toString()));
    fflush(stdout);
  }
  return t;
}


Token Tokenizer::nextTokenImpl()
{
  SrcPos beginSrcpos;

  for (;;) {
    if (fCC == EOF)
      return Token(srcpos(), kEOF);

    beginSrcpos = srcpos();
    switch (fCC) {
      // whitespace
    case ' ':
    case '\n':
    case '\r':
    case '\t':
    case '\f':
    case '\v': nextChar(); continue;

    case '(': return makeTokenAndNext(srcpos(), kParanOpen);
    case ')':
      nextChar();
      if (fCC == '?') {
        nextChar();
        return Token(beginSrcpos, kMacroClose2);
      }
      return Token(beginSrcpos, kParanClose);

    case '[': return makeTokenAndNext(srcpos(), kBracketOpen);
    case ']': return makeTokenAndNext(srcpos(), kBracketClose);
    case '{': return makeTokenAndNext(srcpos(), kBraceOpen);
    case '}':
      return makeTokenAndNext(srcpos(), kBraceClose);

      // utf8: c2 ab | \302\253 | left guillemet
    case 0x00ab:
      return makeTokenAndNext(srcpos(), kMacroOpen);
      // utf8: c2 bb | \302\273 | right guillemet
    case 0x00bb: return makeTokenAndNext(srcpos(), kMacroClose);

    case ',': return makeTokenAndNext(srcpos(), kComma);
    case ';': return makeTokenAndNext(srcpos(), kSemicolon);
    case ':': return makeTokenAndNext(srcpos(), kColon);
    case '@': return makeTokenAndNext(srcpos(), kAt);
    case '|': return makeTokenAndNext(srcpos(), kPipe);
    case '\'': return makeTokenAndNext(srcpos(), kQuote);
    case '^': return makeTokenAndNext(srcpos(), kReference);

    case '&': return makeTokenAndNext(srcpos(), kAmpersand);

    case '+':
    case '/':
    case '*':
    case '%':
    case '=': return readSymbolOrOperator(K(acceptGenerics));
    case '<':
      if (fNextCharIsGenericOpen) {
        fNextCharIsGenericOpen = false;
        fInGenericContext++;
        return makeTokenAndNext(srcpos(), kGenericOpen);
      }
      return readSymbolOrOperator(!K(acceptGenerics));

    case '>':
      if (fInGenericContext > 0) {
        fInGenericContext--;
        return makeTokenAndNext(srcpos(), kGenericClose);
      }
      return readSymbolOrOperator(!K(acceptGenerics));

    case '-':
      nextChar();
      switch (fCC) {
      case '-': readCommentLine(); continue;

      case '>':
        nextChar();
        if (isSymbolChar(fCC))
          return readIdentifier(beginSrcpos, String("->"), kSymbol, K(acceptGenerics));
        else
          return Token(beginSrcpos, kMapTo);
      default:
        if (isDigit(fCC))
          return readNumber(beginSrcpos, -1);
        else
          return Token(beginSrcpos, kMinus);
      }

    case '?':
      nextChar();
      if (fCC == '"') {
        nextChar();
        Token param =
            readIdentifier(beginSrcpos, String(), kMacroParamAsStr, !K(acceptGenerics));
        if (fCC != '"') {
          HR_LOG(kError, srcpos(), E_MissingApos) << "Missing \" in ?\"-notation";
        }
        else
          nextChar();
        if (param.idValue().isEmpty()) {
          HR_LOG(kError, beginSrcpos, E_BadMacroPattern) << "empty macro parameter";
          return Token();
        }

        return param;
      }
      else if (fCC == '(') {
        nextChar();
        return Token(beginSrcpos, kMacroOpen2);
      }
      return readIdentifier(beginSrcpos, String(), kMacroParam, !K(acceptGenerics));

    case '.':
      nextChar();
      if (fCC == '.') {
        nextChar();
        if (fCC == '.')
          return makeTokenAndNext(beginSrcpos, kEllipsis);
        else
          return Token(beginSrcpos, kRange);
      }
      else
        return Token(beginSrcpos, kDot);

    case '#':
      nextChar();
      switch (fCC) {
      case '[': return makeTokenAndNext(beginSrcpos, kLiteralArrayOpen);
      case '(': return makeTokenAndNext(beginSrcpos, kLiteralVectorOpen);
      case '#': return makeTokenAndNext(beginSrcpos, kSangHash);
      default:
        if (isSymbolChar(fCC))
          return readIdentifier(beginSrcpos, String(), kKeyword, !K(acceptGenerics));
        else {
          HR_LOG(kError, beginSrcpos, E_BadHashNotation)
              << "Unknown #-notation: \\u0x" << hex(fCC);
          continue;
        }
      }
      break;

    case '"': nextChar(); return readString(beginSrcpos, '"', kString);

    case '~': nextChar(); return readString(beginSrcpos, '~', kDocString);

    case '\\': {
      nextChar();
      Token ct = readCharacter(beginSrcpos, !K(needsTerminator));
      if (ct.isSet())
        return ct;
      continue;
    }

    default:
      if (isAlpha(fCC) || isAlphaSpec(fCC))
        return readSymbolOrOperator(K(acceptGenerics));
      else if (isDigit(fCC))
        return readNumber(beginSrcpos, 1);
      else {
        HR_LOG(kError, beginSrcpos, E_UnexpectedChar)
            << "unexpected char: \\0x" << hex(fCC);
        nextChar();
      }
    }
  }

  return Token();
}

}  // namespace herschel
