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


Token
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
    return Token(kKeyarg, identifier.part(0, identifier.length() - 1));
  else
    return Token(type, identifier);
}


Token
Tokenizer::makeTokenAndNext(TokenType type)
{
  Token t = Token(type);
  nextChar();
  return t;
}


Token
Tokenizer::readSymbolOrOperator(bool acceptGenerics)
{
  Char currentChar = fCC;
  nextChar();
  if (isDelimiter(fCC)) {
    switch (currentChar) {
    case '+': return Token(kPlus);
    case '/': return Token(kDivide);
    case '*': return Token(kMultiply);
    case '%': return Token(kFold);
    case '<': return Token(kLess);
    case '>': return Token(kGreater);
    case '=': return Token(kAssign);
    default:
      return Token(String() + currentChar);
    }
  }

  Token token = readIdentifier(String() + currentChar, kSymbol, acceptGenerics);
  if (token.tokenType() == kSymbol) {
    if (token.idValue() == String("**"))
      return Token(kExponent);
    else if (token.idValue() == String("<=>"))
      return Token(kCompare);
    else if (token.idValue() == String("=="))
      return Token(kEqual);
    else if (token.idValue() == String("<>"))
      return Token(kUnequal);
    else if (token.idValue() == String("<>"))
      return Token(kUnequal);
    else if (token.idValue() == String("<="))
      return Token(kLessEqual);
    else if (token.idValue() == String(">="))
      return Token(kGreaterEqual);
    else if (token.idValue() == String("in"))
      return Token(kIn);
    else if (token.idValue() == String("and"))
      return Token(kLogicalAnd);
    else if (token.idValue() == String("or"))
      return Token(kLogicalOr);
    else if (token.idValue() == String("mod"))
      return Token(kMod);
    else if (token.idValue() == String("AND"))
      return Token(kBitAnd);
    else if (token.idValue() == String("OR"))
      return Token(kBitOr);
    else if (token.idValue() == String("XOR"))
      return Token(kBitXor);
    else if (token.idValue() == String("<<"))
      return Token(kShiftLeft);
    else if (token.idValue() == String(">>"))
      return Token(kShiftRight);
    else if (token.idValue() == String("isa"))
      return Token(kIsa);
    else if (token.idValue() == String("as"))
      return Token(kAs);
    else if (token.idValue() == String("by"))
      return Token(kBy);
    else if (token.idValue() == String("true"))
      return Token(kBool, true);
    else if (token.idValue() == String("false"))
      return Token(kBool, false);
    else if (token.idValue() == String("++"))
      return Token(kAppend);
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


Token
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

  Token token;
  switch (type) {
  case kInteger:
    token = Token(kInteger, first.toInt(radix) * sign);
    break;

  case kReal:
    {
      String tmp = first + "." + second;
      if (!exponent.isEmpty())
        tmp = tmp + "e" + (expSign < 0 ? '-' : '+') + exponent;

      token = Token(kReal, tmp.toDouble() * sign);
    }
    break;

  case kRational:
    {
      int fval = first.toInt(10);
      int sval = second.toInt(10);
      token = Token(kRational, Rational(fval * sign, sval));
    }
    break;

  default:
    assert(0);
  }

  token.setIsImaginary(isImaginary);

  return token;
}


Token
Tokenizer::readNumericCharacter(bool needsTerminator)
{
  Token token = readNumber(1);
  if (token.isIntLit()) {
    int readc = token.intLitValue();

    if (needsTerminator) {
      if (fCC == ';') {
        nextChar();
        return Token(kChar, readc);
      }
      else
        parseError(String("unterminated char"));
    }
    else
      return Token(kChar, readc);
  }
  else
    parseError(String("unterminated char"));

  return Token();
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


Token
Tokenizer::translateChar(const String& charnm)
{
  Char c = mapCharNameToChar(charnm);
  return Token(kChar, c);
}


Token
Tokenizer::readSymbolCharacter(bool needsTerminator)
{
  Token sym = readIdentifier(String(), kSymbol, false); // don't accept generics
  if (sym.type() != kId)
    parseError(String("expected character symbol"));

  Token ct = (sym.idValue().length() == 1
              ? Token(kChar, sym.idValue()[0])
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


Token
Tokenizer::readNamedCharacter(bool needsTerminator)
{
  if (isWhitespace(fCC))
    parseError(String("unterminated char notation"));
  else if (isAlpha(fCC) || isDigit(fCC))
    return readSymbolCharacter(needsTerminator);
  int c = fCC;
  nextChar();
  return Token(kChar, c);
}


Token
Tokenizer::readCharacter(bool needsTerminator)
{
  if (fCC == 'u') {
    nextChar();
    return readNumericCharacter(needsTerminator);
  }
  return readNamedCharacter(needsTerminator);
}



Token
Tokenizer::readString()
{
  String result;
  int prevlc = fLineCount;

  try {
    for ( ; ; ) {
      if (fCC == '"') {
        nextChar();
        return Token(kString, result);
      }
      else if (fCC == '\\') {
        nextChar();
        Token ct = readCharacter(true); // needs terminator
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
  return Token();
}


Token
Tokenizer::nextToken()
{
  Token t = nextTokenImpl();
  if (Properties::isTraceTokenizer()) {
    printf("%s ", (const char*)StrHelper(t.toString()));
    fflush(stdout);
  }
  return t;
}


Token
Tokenizer::nextTokenImpl()
{
  for ( ; ; ) {
    if (fCC == EOF)
      return Token(kEOF);

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
          return Token(kMapTo);
      default:
        if (isDigit(fCC))
          return readNumber(-1);
        else if (isSymbolChar(fCC))
          return readIdentifier(String("-"), kSymbol, true);
        else
          return Token(kMinus);
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
          return Token(kRange);
      }
      else
        return Token(kDot);

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

  return Token();
}


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
        assert(tnz.nextToken() == Token(String("interface")));
        assert(tnz.nextToken() == Token(String("zero")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kString, String("eyestep/zero 1.0:portables")));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(String("export")));
        assert(tnz.nextToken() == Token(String("public")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kMultiply));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(String("def")));
        assert(tnz.nextToken() == Token(String("class")));
        assert(tnz.nextToken() == Token(String("Portable")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(String("T")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(String("x")));
        assert(tnz.nextToken() == Token(kAt));
        assert(tnz.nextToken() == Token(String("Int")));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(String("Copyable")));
        assert(tnz.nextToken() == Token(kComma));
        assert(tnz.nextToken() == Token(String("Comparable")));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(kBraceOpen));
        assert(tnz.nextToken() == Token(String("slot")));
        assert(tnz.nextToken() == Token(String("first")));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(String("T")));
        assert(tnz.nextToken() == Token(kAssign));
        assert(tnz.nextToken() == Token(String("x")));

        assert(tnz.nextToken() == Token(kSemicolon));

        assert(tnz.nextToken() == Token(String("slot")));
        assert(tnz.nextToken() == Token(String("data")));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(String("Octet")));
        assert(tnz.nextToken() == Token(kBracketOpen));
        assert(tnz.nextToken() == Token(kBracketClose));
        assert(tnz.nextToken() == Token(kBraceClose));
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
        assert(tnz.nextToken() == Token(kBool, true));
        assert(tnz.nextToken() == Token(kBool, false));
        assert(tnz.nextToken() == Token(kInteger, 12345));
        assert(tnz.nextToken() == Token(kInteger, 0xaaaa));
        assert(tnz.nextToken() == Token(kInteger, 0xabcdef));
        assert(tnz.nextToken() == Token(kInteger, 07123));
        assert(tnz.nextToken() == Token(kInteger, 45));
        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kInteger, 3));
        assert(tnz.nextToken() == Token(kInteger, 4));

        assert(tnz.nextToken() == Token(kReal, 12.34));
        assert(tnz.nextToken() == Token(kReal, 0.12345e+10));
        assert(tnz.nextToken() == Token(kReal, 0.12345e+10)); // normalized 123.45e+7
        assert(tnz.nextToken() == Token(kReal, 0.000123456)); // normalized
        assert(tnz.nextToken() == Token(kReal, -3.1415));

        assert(tnz.nextToken() == Token(kRational, Rational(2, 3)));
        assert(tnz.nextToken() == Token(kRational, Rational(120, 33)));
        assert(tnz.nextToken() == Token(kRational, Rational(1, 1024)));
        assert(tnz.nextToken() == Token(kInteger, 5).setIsImaginary(true));
        assert(tnz.nextToken() == Token(kReal, 3.1415).setIsImaginary(true));
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
        assert(tnz.nextToken() == Token(kChar, 0x20));
        assert(tnz.nextToken() == Token(kChar, 0x60));
        assert(tnz.nextToken() == Token(kChar, '('));
        assert(tnz.nextToken() == Token(kChar, 0x0a));
        assert(tnz.nextToken() == Token(kChar, 0x0d));

        assert(tnz.nextToken() == Token(kString, String("hello,\nworld!")));
        assert(tnz.nextToken() == Token(kString, String("\033a(;;")));

        assert(tnz.nextToken() == Token(kChar, 0xac00));
        assert(tnz.nextToken() == Token(kChar, 0xacf5));
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
        assert(tnz.nextToken() == Token(String("def")));
        assert(tnz.nextToken() == Token(String("f")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(String("args")));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(kAmpersand));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(String("String")));
        assert(tnz.nextToken() == Token(kComma));
        assert(tnz.nextToken() == Token(String("Uri")));
        assert(tnz.nextToken() == Token(kComma));
        assert(tnz.nextToken() == Token(String("Boolean")));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kBracketOpen));
        assert(tnz.nextToken() == Token(kBracketClose));
        assert(tnz.nextToken() == Token(kEllipsis));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kEllipsis));

        assert(tnz.nextToken() == Token(String("def")));
        assert(tnz.nextToken() == Token(String("f")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kKeyarg, String("arg")));
        assert(tnz.nextToken() == Token(String("_x")));
        assert(tnz.nextToken() == Token(kAssign));
        assert(tnz.nextToken() == Token(kInteger, 0));
        assert(tnz.nextToken() == Token(kRange));
        assert(tnz.nextToken() == Token(kInteger, 20));
        assert(tnz.nextToken() == Token(kBy));
        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kParanClose));
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
        assert(tnz.nextToken() == Token(kKeyword, String("abc")));
        assert(tnz.nextToken() == Token(kKeyword, String("delft")));

        assert(tnz.nextToken() == Token(kLiteralArrayOpen));
        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kComma));
        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kBracketClose));

        assert(tnz.nextToken() == Token(kLiteralArrayOpen));
        assert(tnz.nextToken() == Token(kBracketClose));

        assert(tnz.nextToken() == Token(kLiteralVectorOpen));
        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kMapTo));
        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(kLiteralVectorOpen));
        assert(tnz.nextToken() == Token(kParanClose));
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
        assert(tnz.nextToken() == Token(String("Buffer")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(String("Int")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kBracketOpen));
        assert(tnz.nextToken() == Token(String("i")));
        assert(tnz.nextToken() == Token(kBracketClose));
        assert(tnz.nextToken() == Token(kLess));
        assert(tnz.nextToken() == Token(kInteger, 10));
        assert(tnz.nextToken() == Token(kLogicalAnd));
        assert(tnz.nextToken() == Token(kBool, true));
        assert(tnz.nextToken() == Token(kLogicalOr));
        assert(tnz.nextToken() == Token(kBool, false));

        assert(tnz.nextToken() == Token(String("T")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(String("S")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(String("Y")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kGenericClose));

        assert(tnz.nextToken() == Token(String("T")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(String("S")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(String("Y")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kGenericClose));

        assert(tnz.nextToken() == Token(String("a")));
        assert(tnz.nextToken() == Token(kLess));
        assert(tnz.nextToken() == Token(String("b")));

        assert(tnz.nextToken() == Token(String("val")));
        assert(tnz.nextToken() == Token(kShiftLeft));
        assert(tnz.nextToken() == Token(kInteger, 5));
        assert(tnz.nextToken() == Token(String("val")));
        assert(tnz.nextToken() == Token(kShiftRight));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kLess));
        assert(tnz.nextToken() == Token(kInteger, 1));

        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kLessEqual));
        assert(tnz.nextToken() == Token(kInteger, 1));

        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kGreater));
        assert(tnz.nextToken() == Token(kInteger, 1));

        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kGreaterEqual));
        assert(tnz.nextToken() == Token(kInteger, 1));

        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kCompare));
        assert(tnz.nextToken() == Token(kInteger, 1));

        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kUnequal));
        assert(tnz.nextToken() == Token(kInteger, 1));

        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kEqual));
        assert(tnz.nextToken() == Token(kInteger, 1));

        assert(tnz.nextToken() == Token(String("a")));
        assert(tnz.nextToken() == Token(kPlus));
        assert(tnz.nextToken() == Token(String("b")));

        assert(tnz.nextToken() == Token(kString, String("a")));
        assert(tnz.nextToken() == Token(kAppend));
        assert(tnz.nextToken() == Token(kString, String("b")));

        assert(tnz.nextToken() == Token(String("a")));
        assert(tnz.nextToken() == Token(kMinus));
        assert(tnz.nextToken() == Token(String("b")));

        assert(tnz.nextToken() == Token(String("a")));
        assert(tnz.nextToken() == Token(kMultiply));
        assert(tnz.nextToken() == Token(String("b")));

        assert(tnz.nextToken() == Token(String("a")));
        assert(tnz.nextToken() == Token(kDivide));
        assert(tnz.nextToken() == Token(String("b")));

        assert(tnz.nextToken() == Token(String("a")));
        assert(tnz.nextToken() == Token(kExponent));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(String("a")));
        assert(tnz.nextToken() == Token(kMod));
        assert(tnz.nextToken() == Token(kInteger, 5));


        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kBitXor));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kBitOr));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kBitAnd));
        assert(tnz.nextToken() == Token(kInteger, 2));


        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kFold));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kMapTo));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kIn));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kIsa));
        assert(tnz.nextToken() == Token(String("Number")));

        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kAs));
        assert(tnz.nextToken() == Token(String("Octet")));

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
        assert(tnz.nextToken() == Token(String("io")));
        assert(tnz.nextToken() == Token(kPipe));
        assert(tnz.nextToken() == Token(String("File")));

        assert(tnz.nextToken() == Token(String("self")));
        assert(tnz.nextToken() == Token(kDot));
        assert(tnz.nextToken() == Token(String("io")));
        assert(tnz.nextToken() == Token(kPipe));
        assert(tnz.nextToken() == Token(String("val")));
        assert(tnz.nextToken() == Token(kDot));
        assert(tnz.nextToken() == Token(String("display")));

        assert(tnz.nextToken() == Token(String("f")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kQuote));
        assert(tnz.nextToken() == Token(String("T")));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(kInteger, 12));
        assert(tnz.nextToken() == Token(kBackQuote));
        assert(tnz.nextToken() == Token(String("mm")));
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
        assert(tnz.nextToken() == Token(kSangHash));
        assert(tnz.nextToken() == Token(kMacroParam, String("val:name")));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }
  }
};

static TokenizerUnitTest tokenizerUnitTest;

#endif  // #if defined(UNITTESTS)

