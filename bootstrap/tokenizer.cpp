/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "str.h"
#include "tokenizer.h"
#include "properties.h"
#include "unittests.h"
#include "registry.h"


using namespace heather;


Tokenizer::Tokenizer(Port<Char>* port, CharRegistry* charRegistry)
  : fPort(port),
    fLineCount(1),
    fCC(EOF),
    fNextCharIsGenericOpen(false),
    fInGenericContext(0),
    fCharRegistry(charRegistry)
{
  assert(port != NULL);

  nextChar();
}


bool
Tokenizer::isEof() const
{
  return fPort == NULL || fPort->isEof();
}


bool
Tokenizer::isInCharRange(Char c, Char from, Char to) const
{
  return c >= from && c <= to;
}


bool
Tokenizer::isWhitespace(Char c) const
{
  return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}


bool
Tokenizer::isDigit(Char c) const
{
  return isInCharRange(c, '0', '9');
}


bool
Tokenizer::isHexDigit(Char c) const
{
  return isInCharRange(c, '0', '9')
    || isInCharRange(c, 'a', 'f')
    || isInCharRange(c, 'A', 'F');
}


bool
Tokenizer::isAlpha(Char c) const
{
  return isInCharRange(c, 'a', 'z') || isInCharRange(c, 'A', 'Z');
}


bool
Tokenizer::isAlphaSpec(Char c) const
{
  return c == '-' || c == '_' || c == '*' || c == '+'
    || c == '%' || c == '?' || c == '!' || c == '/';
}


bool
Tokenizer::isSymbolChar(Char c) const
{
  return isAlpha(c) || isDigit(c) || isAlphaSpec(c);
}


bool
Tokenizer::isDelimiter(Char c) const
{
  return isWhitespace(c) ||
    c == '"' || c == '\'' ||
    c == '(' || c == ')' ||
    c == '[' || c == ']' ||
    c == '{' || c == '}' ||
    c == '.' || c == ',' || c == ';' || c == '#' || c == '@' ||
    c == '|';
}


bool
Tokenizer::isEOL(Char c) const
{
  return c == '\n' || c == '\r';
}


int
Tokenizer::nextChar()
{
  try {
    int c = fPort->read();
    if (c == '\n' || c == '\r')
      fLineCount++;
    fCC = c;
    return c;
  }
  catch (const EofException& ) {
    fCC = EOF;
    throw;
  }
}


void
Tokenizer::parseError(const String& msg) throw (NotationException)
{
  throw NotationException(fLineCount, msg);
}


void
Tokenizer::readCommentLine()
{
  while (!isEOL(fCC))
    nextChar();
}


Pexpr
Tokenizer::readIdentifier(const String& prefix, TokenType type,
                          bool acceptGenerics)
{
  String identifier = prefix;

  try {
    while (!isDelimiter(fCC)) {
      if (acceptGenerics) {
        if (fCC == '<') {
          fNextCharIsGenericOpen = true;
          break;
        }
        else if (fCC == '>')
          break;
      }
      identifier = identifier + Char(fCC);
      nextChar();
    }
  }
  catch (const EofException& ) {
  }

  if (type == kSymbol && identifier.endsWith(String(":")))
    return Pexpr(kKeyarg, identifier.part(0, identifier.length() - 1));
  else
    return Pexpr(type, identifier);
}


Pexpr
Tokenizer::makeTokenAndNext(TokenType type)
{
  Pexpr t = Pexpr(type);
  nextChar();
  return t;
}


Pexpr
Tokenizer::readSymbolOrOperator(bool acceptGenerics)
{
  Char currentChar = fCC;
  nextChar();
  if (isDelimiter(fCC)) {
    switch (currentChar) {
    case '+': return Pexpr(kPlus);
    case '/': return Pexpr(kDivide);
    case '*': return Pexpr(kMultiply);
    case '%': return Pexpr(kFold);
    case '<': return Pexpr(kLess);
    case '>': return Pexpr(kGreater);
    case '=': return Pexpr(kAssign);
    default:
      return Pexpr(String() + currentChar);
    }
  }

  Pexpr token = readIdentifier(String() + currentChar, kSymbol, acceptGenerics);
  if (token.tokenType() == kSymbol) {
    if (token.idValue() == String("**"))
      return Pexpr(kExponent);
    else if (token.idValue() == String("<=>"))
      return Pexpr(kCompare);
    else if (token.idValue() == String("=="))
      return Pexpr(kEqual);
    else if (token.idValue() == String("<>"))
      return Pexpr(kUnequal);
    else if (token.idValue() == String("<>"))
      return Pexpr(kUnequal);
    else if (token.idValue() == String("<="))
      return Pexpr(kLessEqual);
    else if (token.idValue() == String(">="))
      return Pexpr(kGreaterEqual);
    else if (token.idValue() == String("in"))
      return Pexpr(kIn);
    else if (token.idValue() == String("and"))
      return Pexpr(kLogicalAnd);
    else if (token.idValue() == String("or"))
      return Pexpr(kLogicalOr);
    else if (token.idValue() == String("mod"))
      return Pexpr(kMod);
    else if (token.idValue() == String("AND"))
      return Pexpr(kBitAnd);
    else if (token.idValue() == String("OR"))
      return Pexpr(kBitOr);
    else if (token.idValue() == String("XOR"))
      return Pexpr(kBitXor);
    else if (token.idValue() == String("<<"))
      return Pexpr(kShiftLeft);
    else if (token.idValue() == String(">>"))
      return Pexpr(kShiftRight);
    else if (token.idValue() == String("isa"))
      return Pexpr(kIsa);
    else if (token.idValue() == String("as"))
      return Pexpr(kAs);
    else if (token.idValue() == String("by"))
      return Pexpr(kBy);
    else if (token.idValue() == String("true"))
      return Pexpr(kBool, true);
    else if (token.idValue() == String("false"))
      return Pexpr(kBool, false);
    else if (token.idValue() == String("++"))
      return Pexpr(kAppend);
  }
  return token;
}


String
Tokenizer::readIntNumberPart(bool acceptHex)
{
  String result;

  while ((acceptHex && isHexDigit(fCC)) ||
         isDigit(fCC)) {
    result = result + Char(fCC);
    nextChar();
  }

  return result;
}


Pexpr
Tokenizer::readNumber(int sign)
{
  TokenType type = kInteger;

  String first = readIntNumberPart(true);
  String second;
  String exponent;
  int expSign = 1;
  int radix = 10;
  bool isImaginary = false;

  if (fCC == '.') {
    type = kReal;
    nextChar();
    second = readIntNumberPart(false);

    if (fCC == 'e' || fCC == 'E') {
      nextChar();
      if (fCC == '-') {
        expSign = -1;
      }
      else if (fCC == '+') {
        expSign = 1;
      }
      else
        parseError(String("bad scientific notation: ") + Char(fCC));

      nextChar();
      exponent = readIntNumberPart(false);
    }
  }
  else if (fCC == '/') {
    type = kRational;
    nextChar();
    second = readIntNumberPart(true);
  }

  if (fCC == 'h' || fCC == 'H') {
    if (type != kInteger)
      parseError(String("hexadecimal notation for unappropriate number type"));
    nextChar();
    radix = 16;
  }
  else if (fCC == 't' || fCC == 'T') {
    if (type != kInteger)
      parseError(String("hexadecimal notation for unappropriate number type"));
    nextChar();
    radix = 8;
  }
  if (fCC == 'y' || fCC == 'Y') {
    if (type != kInteger)
      parseError(String("hexadecimal notation for unappropriate number type"));
    nextChar();
    radix = 2;
  }

  if (fCC == 'i' || fCC == 'I') {
    nextChar();
    isImaginary = true;
  }

  Pexpr token;
  switch (type) {
  case kInteger:
    token = Pexpr(kInteger, first.toInt(radix) * sign);
    break;

  case kReal:
    {
      String tmp = first + "." + second;
      if (!exponent.isEmpty())
        tmp = tmp + "e" + (expSign < 0 ? '-' : '+') + exponent;

      token = Pexpr(kReal, tmp.toDouble() * sign);
    }
    break;

  case kRational:
    {
      int fval = first.toInt(10);
      int sval = second.toInt(10);
      token = Pexpr(kRational, Rational(fval * sign, sval));
    }
    break;

  default:
    assert(0);
  }

  token.setIsImaginary(isImaginary);

  return token;
}


Pexpr
Tokenizer::readNumericCharacter(bool needsTerminator)
{
  Pexpr token = readNumber(1);
  if (token.isIntLit()) {
    int readc = token.intLitValue();

    if (needsTerminator) {
      if (fCC == ';') {
        nextChar();
        return Pexpr(kChar, readc);
      }
      else
        parseError(String("unterminated char"));
    }
    else
      return Pexpr(kChar, readc);
  }
  else
    parseError(String("unterminated char"));

  return Pexpr();
}


Char
Tokenizer::mapCharNameToChar(const String& charnm)
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
  else if (fCharRegistry != NULL) {
    int cp;
    if (fCharRegistry->lookup(charnm, &cp)) {
      if (cp >= 0 && cp <= 0x10ffff)
        return Char(cp);
    }
  }

  parseError(String("Unknown char name: ") + charnm);
  return ' ';
}


Pexpr
Tokenizer::translateChar(const String& charnm)
{
  Char c = mapCharNameToChar(charnm);
  return Pexpr(kChar, c);
}


Pexpr
Tokenizer::readSymbolCharacter(bool needsTerminator)
{
  Pexpr sym = readIdentifier(String(), kSymbol, false); // don't accept generics
  if (sym.type() != kId)
    parseError(String("expected character symbol"));

  Pexpr ct = (sym.idValue().length() == 1
              ? Pexpr(kChar, sym.idValue()[0])
              : translateChar(sym.idValue()));
  if (needsTerminator) {
    if (fCC == ';') {
      nextChar();
      return ct;
    }
    else
      parseError(String("unterminated char"));
  }

  return ct;
}


Pexpr
Tokenizer::readNamedCharacter(bool needsTerminator)
{
  if (isWhitespace(fCC))
    parseError(String("unterminated char notation"));
  else if (isAlpha(fCC) || isDigit(fCC))
    return readSymbolCharacter(needsTerminator);
  int c = fCC;
  nextChar();
  return Pexpr(kChar, c);
}


Pexpr
Tokenizer::readCharacter(bool needsTerminator)
{
  if (fCC == 'u') {
    nextChar();
    return readNumericCharacter(needsTerminator);
  }
  return readNamedCharacter(needsTerminator);
}



Pexpr
Tokenizer::readString()
{
  String result;
  int prevlc = fLineCount;

  try {
    for ( ; ; ) {
      if (fCC == '"') {
        nextChar();
        return Pexpr(kString, result);
      }
      else if (fCC == '\\') {
        nextChar();
        Pexpr ct = readCharacter(true); // needs terminator
        if (ct.tokenType() == kChar)
          result = result + ct.charLitValue();
        else
          parseError(String("Char expected"));
      }
      else {
        result = result + Char(fCC);
        nextChar();
      }
    }
  }
  catch (const EofException& ) {
    parseError(String("unfinished string, began at line ") + prevlc);
    throw;
  }

  // todo
  return Pexpr();
}


Pexpr
Tokenizer::nextToken()
{
  Pexpr t = nextTokenImpl();
  if (Properties::isTraceTokenizer()) {
    printf("%s ", (const char*)StrHelper(t.toString()));
    fflush(stdout);
  }
  return t;
}


Pexpr
Tokenizer::nextTokenImpl()
{
  for ( ; ; ) {
    if (fCC == EOF)
      return Pexpr(kEOF);

    switch (fCC) {
    case ' ': case '\n': case '\r': case '\t':
      nextChar();
      continue;

    case '(': return makeTokenAndNext(kParanOpen);
    case ')': return makeTokenAndNext(kParanClose);
    case '[': return makeTokenAndNext(kBracketOpen);
    case ']': return makeTokenAndNext(kBracketClose);
    case '{': return makeTokenAndNext(kBraceOpen);
    case '}': return makeTokenAndNext(kBraceClose);

    case ',': return makeTokenAndNext(kComma);
    case ';': return makeTokenAndNext(kSemicolon);
    case ':': return makeTokenAndNext(kColon);

    case '@': return makeTokenAndNext(kAt);
    case '&': return makeTokenAndNext(kAmpersand);
    case '|': return makeTokenAndNext(kPipe);
    case '\'': return makeTokenAndNext(kQuote);
    case '`': return makeTokenAndNext(kBackQuote);

    case '+': case '/': case '*': case '%': case '=':
      return readSymbolOrOperator(true);
    case '<':
      if (fNextCharIsGenericOpen) {
        fNextCharIsGenericOpen = false;
        fInGenericContext++;
        return makeTokenAndNext(kGenericOpen);
      }
      return readSymbolOrOperator(false);

    case '>':
      if (fInGenericContext > 0) {
        fInGenericContext--;
        return makeTokenAndNext(kGenericClose);
      }
      return readSymbolOrOperator(false);

    case '-':
      nextChar();
      switch (fCC) {
      case '-':
        readCommentLine();
        continue;
      case '>':
        nextChar();
        if (isSymbolChar(fCC))
          return readIdentifier(String("->"), kSymbol, true);
        else
          return Pexpr(kMapTo);
      default:
        if (isDigit(fCC))
          return readNumber(-1);
        else if (isSymbolChar(fCC))
          return readIdentifier(String("-"), kSymbol, true);
        else
          return Pexpr(kMinus);
      }

    case '?':
      nextChar();
      return readIdentifier(String(), kMacroParam, false);

    case '.':
      nextChar();
      if (fCC == '.') {
        nextChar();
        if (fCC == '.')
          return makeTokenAndNext(kEllipsis);
        else
          return Pexpr(kRange);
      }
      else
        return Pexpr(kDot);

    case '#':
      nextChar();
      switch (fCC) {
      case '[': return makeTokenAndNext(kLiteralArrayOpen);
      case '(': return makeTokenAndNext(kLiteralVectorOpen);
      case '#': return makeTokenAndNext(kSangHash);
      default:
        if (isSymbolChar(fCC))
          return readIdentifier(String(), kKeyword, false);
        else
          parseError(String("unexpected #-notation: ") + Char(fCC));
      }
      break;

    case '"':
      nextChar();
      return readString();

    case '\\':
      nextChar();
      return readCharacter(false); // need a terminator?

    default:
      if (isAlpha(fCC) || isAlphaSpec(fCC))
        return readSymbolOrOperator(true);
      else if (isDigit(fCC))
        return readNumber(1);
      else
        parseError(String("unexpected char: ") + Char(fCC));
    }
  }

  return Pexpr();
}


//----------------------------------------------------------------------------

// String
// Token::toString() const
// {
//   switch (fType) {
//   case kEOF:              return String("EOF");
//   case kInvalid:          return String("INVALID");
//   case kPlus:             return String("+");
//   case kAppend:           return String("++");
//   case kMinus:            return String("-");
//   case kDivide:           return String("/");
//   case kMultiply:         return String("*");
//   case kExponent:         return String("**");
//   case kFold:             return String("%");
//   case kCompare:          return String("<=>");
//   case kEqual:            return String("==");
//   case kUnequal:          return String("<>");
//   case kLess:             return String("<");
//   case kLessEqual:        return String("<=");
//   case kGreater:          return String(">");
//   case kGreaterEqual:     return String(">=");
//   case kAssign:           return String("=");
//   case kMapTo:            return String("->");
//   case kIn:               return String("in");
//   case kMod:              return String("mod");
//   case kIsa:              return String("isa");
//   case kAs:               return String("as");
//   case kBy:               return String("by");
//   case kLogicalAnd:       return String("and");
//   case kLogicalOr:        return String("or");
//   case kBitAnd:           return String("AND");
//   case kBitOr:            return String("OR");
//   case kBitXor:           return String("XOR");
//   case kShiftLeft:        return String("<<");
//   case kShiftRight:       return String(">>");

//   case kString:           return String("\"") + fStrValue + "\"";
//   case kSymbol:           return fStrValue;
//   case kKeyarg:           return fStrValue + ":";
//   case kMacroParam:       return String("?") + fStrValue;
//   case kKeyword:          return String("#") + fStrValue;

//   case kChar:
//     {
//       char buffer[32];
//       sprintf(buffer, "\\u0%x;", fIntValue);
//       return String(buffer);
//     }

//   case kBool:
//     return fBoolValue ? String("true") : String("false");
//   case kInteger:
//     return !fIsImaginary ? fromInt(fIntValue) : (fromInt(fIntValue) + "i");
//   case kReal:
//     return !fIsImaginary ? fromDouble(fDoubleValue) : (fromDouble(fDoubleValue) + "i");
//   case kRational:
//     {
//       char buffer[128];
//       sprintf(buffer, "%d/%d",
//               fRationalValue.numerator(), fRationalValue.denominator());
//       if (fIsImaginary)
//         strcat(buffer, "i");
//       return String(buffer);
//     }

//   case kParanOpen:          return String("(");
//   case kParanClose:         return String(")");
//   case kBracketOpen:        return String("[");
//   case kBracketClose:       return String("]");
//   case kBraceOpen:          return String("{");
//   case kBraceClose:         return String("}");
//   case kGenericOpen:        return String("<");
//   case kGenericClose:       return String(">");
//   case kComma:              return String(",");
//   case kSemicolon:          return String(";");
//   case kColon:              return String(":");
//   case kAt:                 return String("@");
//   case kAmpersand:          return String("&");
//   case kPipe:               return String("|");
//   case kBackQuote:          return String("`");
//   case kQuote:              return String("'");
//   case kEllipsis:           return String("...");
//   case kRange:              return String("..");
//   case kDot:                return String(".");

//   case kLiteralVectorOpen:  return String("#(");
//   case kLiteralArrayOpen:   return String("#[");
//   case kSangHash:           return String("##");
//   };

//   return String("??");
// }




#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class TokenizerUnitTest : public UnitTest
{
public:
  TokenizerUnitTest() : UnitTest("Tokenizer") {}

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

      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try {
        assert(tnz.nextToken() == Pexpr(String("interface")));
        assert(tnz.nextToken() == Pexpr(String("zero")));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(kString, String("eyestep/zero 1.0:portables")));
        assert(tnz.nextToken() == Pexpr(kParanClose));

        assert(tnz.nextToken() == Pexpr(String("export")));
        assert(tnz.nextToken() == Pexpr(String("public")));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(kMultiply));
        assert(tnz.nextToken() == Pexpr(kParanClose));

        assert(tnz.nextToken() == Pexpr(String("def")));
        assert(tnz.nextToken() == Pexpr(String("class")));
        assert(tnz.nextToken() == Pexpr(String("Portable")));
        assert(tnz.nextToken() == Pexpr(kGenericOpen));
        assert(tnz.nextToken() == Pexpr(String("T")));
        assert(tnz.nextToken() == Pexpr(kGenericClose));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(String("x")));
        assert(tnz.nextToken() == Pexpr(kAt));
        assert(tnz.nextToken() == Pexpr(String("Int")));
        assert(tnz.nextToken() == Pexpr(kParanClose));
        assert(tnz.nextToken() == Pexpr(kColon));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(String("Copyable")));
        assert(tnz.nextToken() == Pexpr(kComma));
        assert(tnz.nextToken() == Pexpr(String("Comparable")));
        assert(tnz.nextToken() == Pexpr(kParanClose));

        assert(tnz.nextToken() == Pexpr(kBraceOpen));
        assert(tnz.nextToken() == Pexpr(String("slot")));
        assert(tnz.nextToken() == Pexpr(String("first")));
        assert(tnz.nextToken() == Pexpr(kColon));
        assert(tnz.nextToken() == Pexpr(String("T")));
        assert(tnz.nextToken() == Pexpr(kAssign));
        assert(tnz.nextToken() == Pexpr(String("x")));

        assert(tnz.nextToken() == Pexpr(kSemicolon));

        assert(tnz.nextToken() == Pexpr(String("slot")));
        assert(tnz.nextToken() == Pexpr(String("data")));
        assert(tnz.nextToken() == Pexpr(kColon));
        assert(tnz.nextToken() == Pexpr(String("Octet")));
        assert(tnz.nextToken() == Pexpr(kBracketOpen));
        assert(tnz.nextToken() == Pexpr(kBracketClose));
        assert(tnz.nextToken() == Pexpr(kBraceClose));
      }
      catch (const NotationException& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "true false\n"
        "12345 0aaaah 0aBcDeFh 07123t 101101y 1y 2t 3h 4\n"
        "12.34 0.12345e+10 123.45e+7 12.3456e-5 -3.1415\n"
        "2/3 120/33 1/1024\n"
        "5i  3.1415i\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try {
        assert(tnz.nextToken() == Pexpr(kBool, true));
        assert(tnz.nextToken() == Pexpr(kBool, false));
        assert(tnz.nextToken() == Pexpr(kInteger, 12345));
        assert(tnz.nextToken() == Pexpr(kInteger, 0xaaaa));
        assert(tnz.nextToken() == Pexpr(kInteger, 0xabcdef));
        assert(tnz.nextToken() == Pexpr(kInteger, 07123));
        assert(tnz.nextToken() == Pexpr(kInteger, 45));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kInteger, 3));
        assert(tnz.nextToken() == Pexpr(kInteger, 4));

        assert(tnz.nextToken() == Pexpr(kReal, 12.34));
        assert(tnz.nextToken() == Pexpr(kReal, 0.12345e+10));
        assert(tnz.nextToken() == Pexpr(kReal, 0.12345e+10)); // normalized 123.45e+7
        assert(tnz.nextToken() == Pexpr(kReal, 0.000123456)); // normalized
        assert(tnz.nextToken() == Pexpr(kReal, -3.1415));

        assert(tnz.nextToken() == Pexpr(kRational, Rational(2, 3)));
        assert(tnz.nextToken() == Pexpr(kRational, Rational(120, 33)));
        assert(tnz.nextToken() == Pexpr(kRational, Rational(1, 1024)));
        assert(tnz.nextToken() == Pexpr(kInteger, 5).setIsImaginary(true));
        assert(tnz.nextToken() == Pexpr(kReal, 3.1415).setIsImaginary(true));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "\\space  \\u60h  \\( \\newline \\cr\n"
        "\"hello,\\nl;world!\"  \"\\esc;\\u61h;\\(\\;;\"\n"
        "\\ga \\gong";
      Ptr<CharRegistry> cr = new CharRegistry;
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    cr);
      cr->registerValue(String("ga"), 0xac00);
      cr->registerValue(String("gong"), 0xacf5);

      try {
        assert(tnz.nextToken() == Pexpr(kChar, 0x20));
        assert(tnz.nextToken() == Pexpr(kChar, 0x60));
        assert(tnz.nextToken() == Pexpr(kChar, '('));
        assert(tnz.nextToken() == Pexpr(kChar, 0x0a));
        assert(tnz.nextToken() == Pexpr(kChar, 0x0d));

        assert(tnz.nextToken() == Pexpr(kString, String("hello,\nworld!")));
        assert(tnz.nextToken() == Pexpr(kString, String("\033a(;;")));

        assert(tnz.nextToken() == Pexpr(kChar, 0xac00));
        assert(tnz.nextToken() == Pexpr(kChar, 0xacf5));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "def f(args : &(String, Uri, Boolean)[] ...) ...\n"
        "def f(arg: _x = 0 .. 20 by 2)\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try {
        assert(tnz.nextToken() == Pexpr(String("def")));
        assert(tnz.nextToken() == Pexpr(String("f")));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(String("args")));
        assert(tnz.nextToken() == Pexpr(kColon));
        assert(tnz.nextToken() == Pexpr(kAmpersand));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(String("String")));
        assert(tnz.nextToken() == Pexpr(kComma));
        assert(tnz.nextToken() == Pexpr(String("Uri")));
        assert(tnz.nextToken() == Pexpr(kComma));
        assert(tnz.nextToken() == Pexpr(String("Boolean")));
        assert(tnz.nextToken() == Pexpr(kParanClose));
        assert(tnz.nextToken() == Pexpr(kBracketOpen));
        assert(tnz.nextToken() == Pexpr(kBracketClose));
        assert(tnz.nextToken() == Pexpr(kEllipsis));
        assert(tnz.nextToken() == Pexpr(kParanClose));
        assert(tnz.nextToken() == Pexpr(kEllipsis));

        assert(tnz.nextToken() == Pexpr(String("def")));
        assert(tnz.nextToken() == Pexpr(String("f")));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(kKeyarg, String("arg")));
        assert(tnz.nextToken() == Pexpr(String("_x")));
        assert(tnz.nextToken() == Pexpr(kAssign));
        assert(tnz.nextToken() == Pexpr(kInteger, 0));
        assert(tnz.nextToken() == Pexpr(kRange));
        assert(tnz.nextToken() == Pexpr(kInteger, 20));
        assert(tnz.nextToken() == Pexpr(kBy));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kParanClose));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "#abc #delft\n"
        "#[1, 2] #[]\n"
        "#(1 -> 2) #()\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try {
        assert(tnz.nextToken() == Pexpr(kKeyword, String("abc")));
        assert(tnz.nextToken() == Pexpr(kKeyword, String("delft")));

        assert(tnz.nextToken() == Pexpr(kLiteralArrayOpen));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kComma));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kBracketClose));

        assert(tnz.nextToken() == Pexpr(kLiteralArrayOpen));
        assert(tnz.nextToken() == Pexpr(kBracketClose));

        assert(tnz.nextToken() == Pexpr(kLiteralVectorOpen));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kMapTo));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kParanClose));

        assert(tnz.nextToken() == Pexpr(kLiteralVectorOpen));
        assert(tnz.nextToken() == Pexpr(kParanClose));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "Buffer<Int>()[i] < 10 and true or false\n"
        "T<S<Y>>  T<S<Y> >  a < b\n"
        "val << 5 val >> 2\n"
        "2 < 1  2 <= 1  2 > 1  2 >= 1  2 <=> 1  2 <> 1  2 == 1\n"
        "a + b  \"a\" ++ \"b\" a - b  a * b  a / b  a ** 2  a mod 5\n"
        "1 XOR 2  1 OR 2  1 AND 2\n"
        "1 % 2  1 -> 2  1 in 2  1 isa Number  1 as Octet\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try {
        assert(tnz.nextToken() == Pexpr(String("Buffer")));
        assert(tnz.nextToken() == Pexpr(kGenericOpen));
        assert(tnz.nextToken() == Pexpr(String("Int")));
        assert(tnz.nextToken() == Pexpr(kGenericClose));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(kParanClose));
        assert(tnz.nextToken() == Pexpr(kBracketOpen));
        assert(tnz.nextToken() == Pexpr(String("i")));
        assert(tnz.nextToken() == Pexpr(kBracketClose));
        assert(tnz.nextToken() == Pexpr(kLess));
        assert(tnz.nextToken() == Pexpr(kInteger, 10));
        assert(tnz.nextToken() == Pexpr(kLogicalAnd));
        assert(tnz.nextToken() == Pexpr(kBool, true));
        assert(tnz.nextToken() == Pexpr(kLogicalOr));
        assert(tnz.nextToken() == Pexpr(kBool, false));

        assert(tnz.nextToken() == Pexpr(String("T")));
        assert(tnz.nextToken() == Pexpr(kGenericOpen));
        assert(tnz.nextToken() == Pexpr(String("S")));
        assert(tnz.nextToken() == Pexpr(kGenericOpen));
        assert(tnz.nextToken() == Pexpr(String("Y")));
        assert(tnz.nextToken() == Pexpr(kGenericClose));
        assert(tnz.nextToken() == Pexpr(kGenericClose));

        assert(tnz.nextToken() == Pexpr(String("T")));
        assert(tnz.nextToken() == Pexpr(kGenericOpen));
        assert(tnz.nextToken() == Pexpr(String("S")));
        assert(tnz.nextToken() == Pexpr(kGenericOpen));
        assert(tnz.nextToken() == Pexpr(String("Y")));
        assert(tnz.nextToken() == Pexpr(kGenericClose));
        assert(tnz.nextToken() == Pexpr(kGenericClose));

        assert(tnz.nextToken() == Pexpr(String("a")));
        assert(tnz.nextToken() == Pexpr(kLess));
        assert(tnz.nextToken() == Pexpr(String("b")));

        assert(tnz.nextToken() == Pexpr(String("val")));
        assert(tnz.nextToken() == Pexpr(kShiftLeft));
        assert(tnz.nextToken() == Pexpr(kInteger, 5));
        assert(tnz.nextToken() == Pexpr(String("val")));
        assert(tnz.nextToken() == Pexpr(kShiftRight));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));

        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kLess));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));

        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kLessEqual));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));

        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kGreater));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));

        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kGreaterEqual));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));

        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kCompare));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));

        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kUnequal));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));

        assert(tnz.nextToken() == Pexpr(kInteger, 2));
        assert(tnz.nextToken() == Pexpr(kEqual));
        assert(tnz.nextToken() == Pexpr(kInteger, 1));

        assert(tnz.nextToken() == Pexpr(String("a")));
        assert(tnz.nextToken() == Pexpr(kPlus));
        assert(tnz.nextToken() == Pexpr(String("b")));

        assert(tnz.nextToken() == Pexpr(kString, String("a")));
        assert(tnz.nextToken() == Pexpr(kAppend));
        assert(tnz.nextToken() == Pexpr(kString, String("b")));

        assert(tnz.nextToken() == Pexpr(String("a")));
        assert(tnz.nextToken() == Pexpr(kMinus));
        assert(tnz.nextToken() == Pexpr(String("b")));

        assert(tnz.nextToken() == Pexpr(String("a")));
        assert(tnz.nextToken() == Pexpr(kMultiply));
        assert(tnz.nextToken() == Pexpr(String("b")));

        assert(tnz.nextToken() == Pexpr(String("a")));
        assert(tnz.nextToken() == Pexpr(kDivide));
        assert(tnz.nextToken() == Pexpr(String("b")));

        assert(tnz.nextToken() == Pexpr(String("a")));
        assert(tnz.nextToken() == Pexpr(kExponent));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));

        assert(tnz.nextToken() == Pexpr(String("a")));
        assert(tnz.nextToken() == Pexpr(kMod));
        assert(tnz.nextToken() == Pexpr(kInteger, 5));


        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kBitXor));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));

        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kBitOr));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));

        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kBitAnd));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));


        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kFold));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));

        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kMapTo));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));

        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kIn));
        assert(tnz.nextToken() == Pexpr(kInteger, 2));

        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kIsa));
        assert(tnz.nextToken() == Pexpr(String("Number")));

        assert(tnz.nextToken() == Pexpr(kInteger, 1));
        assert(tnz.nextToken() == Pexpr(kAs));
        assert(tnz.nextToken() == Pexpr(String("Octet")));

      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "io|File  self.io|val.display\n"
        "f('T)  12`mm\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try {
        assert(tnz.nextToken() == Pexpr(String("io")));
        assert(tnz.nextToken() == Pexpr(kPipe));
        assert(tnz.nextToken() == Pexpr(String("File")));

        assert(tnz.nextToken() == Pexpr(String("self")));
        assert(tnz.nextToken() == Pexpr(kDot));
        assert(tnz.nextToken() == Pexpr(String("io")));
        assert(tnz.nextToken() == Pexpr(kPipe));
        assert(tnz.nextToken() == Pexpr(String("val")));
        assert(tnz.nextToken() == Pexpr(kDot));
        assert(tnz.nextToken() == Pexpr(String("display")));

        assert(tnz.nextToken() == Pexpr(String("f")));
        assert(tnz.nextToken() == Pexpr(kParanOpen));
        assert(tnz.nextToken() == Pexpr(kQuote));
        assert(tnz.nextToken() == Pexpr(String("T")));
        assert(tnz.nextToken() == Pexpr(kParanClose));

        assert(tnz.nextToken() == Pexpr(kInteger, 12));
        assert(tnz.nextToken() == Pexpr(kBackQuote));
        assert(tnz.nextToken() == Pexpr(String("mm")));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "##  ?val:name";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try {
        assert(tnz.nextToken() == Pexpr(kSangHash));
        assert(tnz.nextToken() == Pexpr(kMacroParam, String("val:name")));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }
  }
};

static TokenizerUnitTest tokenizerUnitTest;

#endif  // #if defined(UNITTESTS)

