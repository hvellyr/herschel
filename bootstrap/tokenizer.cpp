/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "registry.h"
#include "str.h"
#include "strbuf.h"
#include "tokenizer.h"
#include "unittests.h"


using namespace heather;


Tokenizer::Tokenizer(Port<Char>* port, const String& srcName,
                     CharRegistry* charRegistry)
  : fPort(port),
    fSrcName(srcName),
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
    throw AnnotatedEofException(srcpos());
  }
}


SrcPos
Tokenizer::srcpos() const
{
  return SrcPos(fSrcName, fLineCount);
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
Tokenizer::readIdentifier(const SrcPos& startPos,
                          const String& prefix, TokenType type,
                          bool acceptGenerics)
{
  StringBuffer idBuffer(prefix);

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
      idBuffer << Char(fCC);
      nextChar();
    }
  }
  catch (const EofException& ) {
  }

  String identifier = idBuffer.toString();
  if (type == kSymbol && identifier.endsWith(String(":")))
    return Token(startPos,
                 kKeyarg, identifier.part(0, identifier.length() - 1));
  else
    return Token(startPos, type, identifier);
}


Token
Tokenizer::makeTokenAndNext(const SrcPos& where, TokenType type)
{
  Token t = Token(where, type);
  nextChar();
  return t;
}


Token
Tokenizer::readSymbolOrOperator(bool acceptGenerics)
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
    default:
      return Token(startPos, String() + currentChar);
    }
  }

  Token token = readIdentifier(startPos,
                               String() + currentChar, kSymbol,
                               acceptGenerics);
  if (token.tokenType() == kSymbol) {
    if (token.idValue() == String("**"))
      return Token(startPos, kExponent);
    else if (token.idValue() == String("<=>"))
      return Token(startPos, kCompare);
    else if (token.idValue() == String("=="))
      return Token(startPos, kEqual);
    else if (token.idValue() == String("<>"))
      return Token(startPos, kUnequal);
    else if (token.idValue() == String("<>"))
      return Token(startPos, kUnequal);
    else if (token.idValue() == String("<="))
      return Token(startPos, kLessEqual);
    else if (token.idValue() == String(">="))
      return Token(startPos, kGreaterEqual);
    else if (token.idValue() == String("in"))
      return Token(startPos, kIn);
    else if (token.idValue() == String("and"))
      return Token(startPos, kLogicalAnd);
    else if (token.idValue() == String("or"))
      return Token(startPos, kLogicalOr);
    else if (token.idValue() == String("mod"))
      return Token(startPos, kMod);
    else if (token.idValue() == String("AND"))
      return Token(startPos, kBitAnd);
    else if (token.idValue() == String("OR"))
      return Token(startPos, kBitOr);
    else if (token.idValue() == String("XOR"))
      return Token(startPos, kBitXor);
    else if (token.idValue() == String("<<"))
      return Token(startPos, kShiftLeft);
    else if (token.idValue() == String(">>"))
      return Token(startPos, kShiftRight);
    else if (token.idValue() == String("isa"))
      return Token(startPos, kIsa);
    else if (token.idValue() == String("as"))
      return Token(startPos, kAs);
    else if (token.idValue() == String("by"))
      return Token(startPos, kBy);
    else if (token.idValue() == String("true"))
      return Token(startPos, kBool, true);
    else if (token.idValue() == String("false"))
      return Token(startPos, kBool, false);
    else if (token.idValue() == String("++"))
      return Token(startPos, kAppend);
  }
  return token;
}


String
Tokenizer::readIntNumberPart(bool acceptHex)
{
  StringBuffer result;

  while ((acceptHex && isHexDigit(fCC)) ||
         isDigit(fCC)) {
    result << Char(fCC);
    nextChar();
  }

  return result.toString();
}


Token
Tokenizer::readNumber(const SrcPos& startPos, int sign)
{
  TokenType type = kInt;

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
      if (fCC == '-')
        expSign = -1;
      else if (fCC == '+')
        expSign = 1;
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
    if (type != kInt)
      parseError(String("hexadecimal notation for unappropriate number type"));
    nextChar();
    radix = 16;
  }
  else if (fCC == 't' || fCC == 'T') {
    if (type != kInt)
      parseError(String("hexadecimal notation for unappropriate number type"));
    nextChar();
    radix = 8;
  }
  if (fCC == 'y' || fCC == 'Y') {
    if (type != kInt)
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
  case kInt:
    token = Token(startPos, kInt, first.toInt(radix) * sign);
    break;

  case kReal:
    {
      StringBuffer tmp(first);
      tmp << "." << second;
      if (!exponent.isEmpty())
        tmp << "e" << (expSign < 0 ? '-' : '+') << exponent;

      token = Token(startPos, kReal, tmp.toString().toDouble() * sign);
    }
    break;

  case kRational:
    {
      int fval = first.toInt(10);
      int sval = second.toInt(10);
      token = Token(startPos, kRational, Rational(fval * sign, sval));
    }
    break;

  default:
    assert(0);
  }

  token.setIsImaginary(isImaginary);

  return token;
}


Token
Tokenizer::readNumericCharacter(const SrcPos& startPos, bool needsTerminator)
{
  Token token = readNumber(startPos, 1);
  if (token.isInt()) {
    int readc = token.intValue();

    if (needsTerminator) {
      if (fCC == ';') {
        nextChar();
        return Token(startPos, kChar, readc);
      }
      else
        parseError(String("unterminated char"));
    }
    else
      return Token(startPos, kChar, readc);
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
Tokenizer::translateChar(const SrcPos& startPos, const String& charnm)
{
  Char c = mapCharNameToChar(charnm);
  return Token(startPos, kChar, c);
}


Token
Tokenizer::readSymbolCharacter(const SrcPos& startPos, bool needsTerminator)
{
  Token sym = readIdentifier(startPos, String(), kSymbol,
                             false /* don't accept generics */);
  if (sym.type() != kId)
    parseError(String("expected character symbol"));

  Token ct = (sym.idValue().length() == 1
              ? Token(startPos, kChar, sym.idValue()[0])
              : translateChar(startPos, sym.idValue()));
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
Tokenizer::readNamedCharacter(const SrcPos& startPos, bool needsTerminator)
{
  if (isWhitespace(fCC))
    parseError(String("unterminated char notation"));
  else if (isAlpha(fCC) || isDigit(fCC))
    return readSymbolCharacter(startPos, needsTerminator);
  int c = fCC;
  nextChar();
  return Token(startPos, kChar, c);
}


Token
Tokenizer::readCharacter(const SrcPos& startPos, bool needsTerminator)
{
  if (fCC == 'u') {
    nextChar();
    return readNumericCharacter(startPos, needsTerminator);
  }
  return readNamedCharacter(startPos, needsTerminator);
}



Token
Tokenizer::readString(const SrcPos& startPos)
{
  StringBuffer result;
  int prevlc = fLineCount;

  try {
    for ( ; ; ) {
      if (fCC == '"') {
        nextChar();
        return Token(startPos, kString, result.toString());
      }
      else if (fCC == '\\') {
        SrcPos charSp = srcpos();
        nextChar();
        Token ct = readCharacter(charSp, true /* needs terminator */);
        if (ct == kChar)
          result << ct.charValue();
        else
          parseError(String("Char expected"));
      }
      else {
        result << Char(fCC);
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
  SrcPos beginSrcpos;

  for ( ; ; ) {
    if (fCC == EOF)
      return Token(srcpos(), kEOF);

    beginSrcpos = srcpos();
    switch (fCC) {
    case ' ': case '\n': case '\r': case '\t':
      nextChar();
      continue;

    case '(': return makeTokenAndNext(srcpos(), kParanOpen);
    case ')': return makeTokenAndNext(srcpos(), kParanClose);
    case '[': return makeTokenAndNext(srcpos(), kBracketOpen);
    case ']': return makeTokenAndNext(srcpos(), kBracketClose);
    case '{': return makeTokenAndNext(srcpos(), kBraceOpen);
    case '}': return makeTokenAndNext(srcpos(), kBraceClose);

    case ',': return makeTokenAndNext(srcpos(), kComma);
    case ';': return makeTokenAndNext(srcpos(), kSemicolon);
    case ':': return makeTokenAndNext(srcpos(), kColon);

    case '@': return makeTokenAndNext(srcpos(), kAt);
    case '&': return makeTokenAndNext(srcpos(), kAmpersand);
    case '|': return makeTokenAndNext(srcpos(), kPipe);
    case '\'': return makeTokenAndNext(srcpos(), kQuote);
    case '`': return makeTokenAndNext(srcpos(), kBackQuote);

    case '+': case '/': case '*': case '%': case '=':
      return readSymbolOrOperator(true);
    case '<':
      if (fNextCharIsGenericOpen) {
        fNextCharIsGenericOpen = false;
        fInGenericContext++;
        return makeTokenAndNext(srcpos(), kGenericOpen);
      }
      return readSymbolOrOperator(false);

    case '>':
      if (fInGenericContext > 0) {
        fInGenericContext--;
        return makeTokenAndNext(srcpos(), kGenericClose);
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
          return readIdentifier(beginSrcpos, String("->"), kSymbol, true);
        else
          return Token(beginSrcpos, kMapTo);
      default:
        if (isDigit(fCC))
          return readNumber(beginSrcpos, -1);
        else if (isSymbolChar(fCC))
          return readIdentifier(beginSrcpos, String("-"), kSymbol, true);
        else
          return Token(beginSrcpos, kMinus);
      }

    case '?':
      nextChar();
      return readIdentifier(beginSrcpos, String(), kMacroParam, false);

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
          return readIdentifier(beginSrcpos, String(), kKeyword, false);
        else {
          errorf(beginSrcpos, E_BadHashNotation,
                 "Unknown #-notation: %C", Char(fCC));
          continue;
        }
      }
      break;

    case '"':
      nextChar();
      return readString(beginSrcpos);

    case '\\':
      nextChar();
      return readCharacter(beginSrcpos, false); // need a terminator?

    default:
      if (isAlpha(fCC) || isAlphaSpec(fCC))
        return readSymbolOrOperator(true);
      else if (isDigit(fCC))
        return readNumber(beginSrcpos, 1);
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
    SrcPos sp;

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

      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    String("n.n."));

      try {
        assert(tnz.nextToken() == Token(sp, String("interface")));
        assert(tnz.nextToken() == Token(sp, String("zero")));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, kString, String("eyestep/zero 1.0:portables")));
        assert(tnz.nextToken() == Token(sp, kParanClose));

        assert(tnz.nextToken() == Token(sp, String("export")));
        assert(tnz.nextToken() == Token(sp, String("public")));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, kMultiply));
        assert(tnz.nextToken() == Token(sp, kParanClose));

        assert(tnz.nextToken() == Token(sp, String("def")));
        assert(tnz.nextToken() == Token(sp, String("class")));
        assert(tnz.nextToken() == Token(sp, String("Portable")));
        assert(tnz.nextToken() == Token(sp, kGenericOpen));
        assert(tnz.nextToken() == Token(sp, String("T")));
        assert(tnz.nextToken() == Token(sp, kGenericClose));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, String("x")));
        assert(tnz.nextToken() == Token(sp, kAt));
        assert(tnz.nextToken() == Token(sp, String("Int")));
        assert(tnz.nextToken() == Token(sp, kParanClose));
        assert(tnz.nextToken() == Token(sp, kColon));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, String("Copyable")));
        assert(tnz.nextToken() == Token(sp, kComma));
        assert(tnz.nextToken() == Token(sp, String("Comparable")));
        assert(tnz.nextToken() == Token(sp, kParanClose));

        assert(tnz.nextToken() == Token(sp, kBraceOpen));
        assert(tnz.nextToken() == Token(sp, String("slot")));
        assert(tnz.nextToken() == Token(sp, String("first")));
        assert(tnz.nextToken() == Token(sp, kColon));
        assert(tnz.nextToken() == Token(sp, String("T")));
        assert(tnz.nextToken() == Token(sp, kAssign));
        assert(tnz.nextToken() == Token(sp, String("x")));

        assert(tnz.nextToken() == Token(sp, kSemicolon));

        assert(tnz.nextToken() == Token(sp, String("slot")));
        assert(tnz.nextToken() == Token(sp, String("data")));
        assert(tnz.nextToken() == Token(sp, kColon));
        assert(tnz.nextToken() == Token(sp, String("Octet")));
        assert(tnz.nextToken() == Token(sp, kBracketOpen));
        assert(tnz.nextToken() == Token(sp, kBracketClose));
        assert(tnz.nextToken() == Token(sp, kBraceClose));
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
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    String("n.n."));

      try {
        assert(tnz.nextToken() == Token(sp, kBool, true));
        assert(tnz.nextToken() == Token(sp, kBool, false));
        assert(tnz.nextToken() == Token(sp, kInt, 12345));
        assert(tnz.nextToken() == Token(sp, kInt, 0xaaaa));
        assert(tnz.nextToken() == Token(sp, kInt, 0xabcdef));
        assert(tnz.nextToken() == Token(sp, kInt, 07123));
        assert(tnz.nextToken() == Token(sp, kInt, 45));
        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kInt, 3));
        assert(tnz.nextToken() == Token(sp, kInt, 4));

        assert(tnz.nextToken() == Token(sp, kReal, 12.34));
        assert(tnz.nextToken() == Token(sp, kReal, 0.12345e+10));
        assert(tnz.nextToken() == Token(sp, kReal, 0.12345e+10)); // normalized 123.45e+7
        assert(tnz.nextToken() == Token(sp, kReal, 0.000123456)); // normalized
        assert(tnz.nextToken() == Token(sp, kReal, -3.1415));

        assert(tnz.nextToken() == Token(sp, kRational, Rational(2, 3)));
        assert(tnz.nextToken() == Token(sp, kRational, Rational(120, 33)));
        assert(tnz.nextToken() == Token(sp, kRational, Rational(1, 1024)));
        assert(tnz.nextToken() == Token(sp, kInt, 5).setIsImaginary(true));
        assert(tnz.nextToken() == Token(sp, kReal, 3.1415).setIsImaginary(true));
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
                    String("n.n."), cr);
      cr->registerValue(String("ga"), 0xac00);
      cr->registerValue(String("gong"), 0xacf5);

      try {
        assert(tnz.nextToken() == Token(sp, kChar, 0x20));
        assert(tnz.nextToken() == Token(sp, kChar, 0x60));
        assert(tnz.nextToken() == Token(sp, kChar, '('));
        assert(tnz.nextToken() == Token(sp, kChar, 0x0a));
        assert(tnz.nextToken() == Token(sp, kChar, 0x0d));

        assert(tnz.nextToken() == Token(sp, kString, String("hello,\nworld!")));
        assert(tnz.nextToken() == Token(sp, kString, String("\033a(;;")));

        assert(tnz.nextToken() == Token(sp, kChar, 0xac00));
        assert(tnz.nextToken() == Token(sp, kChar, 0xacf5));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "def f(args : &(String, Uri, Boolean)[] ...) ...\n"
        "def f(arg: _x = 0 .. 20 by 2)\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    String("n.n."));

      try {
        assert(tnz.nextToken() == Token(sp, String("def")));
        assert(tnz.nextToken() == Token(sp, String("f")));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, String("args")));
        assert(tnz.nextToken() == Token(sp, kColon));
        assert(tnz.nextToken() == Token(sp, kAmpersand));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, String("String")));
        assert(tnz.nextToken() == Token(sp, kComma));
        assert(tnz.nextToken() == Token(sp, String("Uri")));
        assert(tnz.nextToken() == Token(sp, kComma));
        assert(tnz.nextToken() == Token(sp, String("Boolean")));
        assert(tnz.nextToken() == Token(sp, kParanClose));
        assert(tnz.nextToken() == Token(sp, kBracketOpen));
        assert(tnz.nextToken() == Token(sp, kBracketClose));
        assert(tnz.nextToken() == Token(sp, kEllipsis));
        assert(tnz.nextToken() == Token(sp, kParanClose));
        assert(tnz.nextToken() == Token(sp, kEllipsis));

        assert(tnz.nextToken() == Token(sp, String("def")));
        assert(tnz.nextToken() == Token(sp, String("f")));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, kKeyarg, String("arg")));
        assert(tnz.nextToken() == Token(sp, String("_x")));
        assert(tnz.nextToken() == Token(sp, kAssign));
        assert(tnz.nextToken() == Token(sp, kInt, 0));
        assert(tnz.nextToken() == Token(sp, kRange));
        assert(tnz.nextToken() == Token(sp, kInt, 20));
        assert(tnz.nextToken() == Token(sp, kBy));
        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kParanClose));
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
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    String("n.n."));

      try {
        assert(tnz.nextToken() == Token(sp, kKeyword, String("abc")));
        assert(tnz.nextToken() == Token(sp, kKeyword, String("delft")));

        assert(tnz.nextToken() == Token(sp, kLiteralArrayOpen));
        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kComma));
        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kBracketClose));

        assert(tnz.nextToken() == Token(sp, kLiteralArrayOpen));
        assert(tnz.nextToken() == Token(sp, kBracketClose));

        assert(tnz.nextToken() == Token(sp, kLiteralVectorOpen));
        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kMapTo));
        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kParanClose));

        assert(tnz.nextToken() == Token(sp, kLiteralVectorOpen));
        assert(tnz.nextToken() == Token(sp, kParanClose));
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
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    String("n.n."));

      try {
        assert(tnz.nextToken() == Token(sp, String("Buffer")));
        assert(tnz.nextToken() == Token(sp, kGenericOpen));
        assert(tnz.nextToken() == Token(sp, String("Int")));
        assert(tnz.nextToken() == Token(sp, kGenericClose));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, kParanClose));
        assert(tnz.nextToken() == Token(sp, kBracketOpen));
        assert(tnz.nextToken() == Token(sp, String("i")));
        assert(tnz.nextToken() == Token(sp, kBracketClose));
        assert(tnz.nextToken() == Token(sp, kLess));
        assert(tnz.nextToken() == Token(sp, kInt, 10));
        assert(tnz.nextToken() == Token(sp, kLogicalAnd));
        assert(tnz.nextToken() == Token(sp, kBool, true));
        assert(tnz.nextToken() == Token(sp, kLogicalOr));
        assert(tnz.nextToken() == Token(sp, kBool, false));

        assert(tnz.nextToken() == Token(sp, String("T")));
        assert(tnz.nextToken() == Token(sp, kGenericOpen));
        assert(tnz.nextToken() == Token(sp, String("S")));
        assert(tnz.nextToken() == Token(sp, kGenericOpen));
        assert(tnz.nextToken() == Token(sp, String("Y")));
        assert(tnz.nextToken() == Token(sp, kGenericClose));
        assert(tnz.nextToken() == Token(sp, kGenericClose));

        assert(tnz.nextToken() == Token(sp, String("T")));
        assert(tnz.nextToken() == Token(sp, kGenericOpen));
        assert(tnz.nextToken() == Token(sp, String("S")));
        assert(tnz.nextToken() == Token(sp, kGenericOpen));
        assert(tnz.nextToken() == Token(sp, String("Y")));
        assert(tnz.nextToken() == Token(sp, kGenericClose));
        assert(tnz.nextToken() == Token(sp, kGenericClose));

        assert(tnz.nextToken() == Token(sp, String("a")));
        assert(tnz.nextToken() == Token(sp, kLess));
        assert(tnz.nextToken() == Token(sp, String("b")));

        assert(tnz.nextToken() == Token(sp, String("val")));
        assert(tnz.nextToken() == Token(sp, kShiftLeft));
        assert(tnz.nextToken() == Token(sp, kInt, 5));
        assert(tnz.nextToken() == Token(sp, String("val")));
        assert(tnz.nextToken() == Token(sp, kShiftRight));
        assert(tnz.nextToken() == Token(sp, kInt, 2));

        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kLess));
        assert(tnz.nextToken() == Token(sp, kInt, 1));

        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kLessEqual));
        assert(tnz.nextToken() == Token(sp, kInt, 1));

        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kGreater));
        assert(tnz.nextToken() == Token(sp, kInt, 1));

        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kGreaterEqual));
        assert(tnz.nextToken() == Token(sp, kInt, 1));

        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kCompare));
        assert(tnz.nextToken() == Token(sp, kInt, 1));

        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kUnequal));
        assert(tnz.nextToken() == Token(sp, kInt, 1));

        assert(tnz.nextToken() == Token(sp, kInt, 2));
        assert(tnz.nextToken() == Token(sp, kEqual));
        assert(tnz.nextToken() == Token(sp, kInt, 1));

        assert(tnz.nextToken() == Token(sp, String("a")));
        assert(tnz.nextToken() == Token(sp, kPlus));
        assert(tnz.nextToken() == Token(sp, String("b")));

        assert(tnz.nextToken() == Token(sp, kString, String("a")));
        assert(tnz.nextToken() == Token(sp, kAppend));
        assert(tnz.nextToken() == Token(sp, kString, String("b")));

        assert(tnz.nextToken() == Token(sp, String("a")));
        assert(tnz.nextToken() == Token(sp, kMinus));
        assert(tnz.nextToken() == Token(sp, String("b")));

        assert(tnz.nextToken() == Token(sp, String("a")));
        assert(tnz.nextToken() == Token(sp, kMultiply));
        assert(tnz.nextToken() == Token(sp, String("b")));

        assert(tnz.nextToken() == Token(sp, String("a")));
        assert(tnz.nextToken() == Token(sp, kDivide));
        assert(tnz.nextToken() == Token(sp, String("b")));

        assert(tnz.nextToken() == Token(sp, String("a")));
        assert(tnz.nextToken() == Token(sp, kExponent));
        assert(tnz.nextToken() == Token(sp, kInt, 2));

        assert(tnz.nextToken() == Token(sp, String("a")));
        assert(tnz.nextToken() == Token(sp, kMod));
        assert(tnz.nextToken() == Token(sp, kInt, 5));


        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kBitXor));
        assert(tnz.nextToken() == Token(sp, kInt, 2));

        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kBitOr));
        assert(tnz.nextToken() == Token(sp, kInt, 2));

        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kBitAnd));
        assert(tnz.nextToken() == Token(sp, kInt, 2));


        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kFold));
        assert(tnz.nextToken() == Token(sp, kInt, 2));

        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kMapTo));
        assert(tnz.nextToken() == Token(sp, kInt, 2));

        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kIn));
        assert(tnz.nextToken() == Token(sp, kInt, 2));

        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kIsa));
        assert(tnz.nextToken() == Token(sp, String("Number")));

        assert(tnz.nextToken() == Token(sp, kInt, 1));
        assert(tnz.nextToken() == Token(sp, kAs));
        assert(tnz.nextToken() == Token(sp, String("Octet")));

      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "io|File  self.io|val.display\n"
        "f('T)  12`mm\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    String("n.n."));

      try {
        assert(tnz.nextToken() == Token(sp, String("io")));
        assert(tnz.nextToken() == Token(sp, kPipe));
        assert(tnz.nextToken() == Token(sp, String("File")));

        assert(tnz.nextToken() == Token(sp, String("self")));
        assert(tnz.nextToken() == Token(sp, kDot));
        assert(tnz.nextToken() == Token(sp, String("io")));
        assert(tnz.nextToken() == Token(sp, kPipe));
        assert(tnz.nextToken() == Token(sp, String("val")));
        assert(tnz.nextToken() == Token(sp, kDot));
        assert(tnz.nextToken() == Token(sp, String("display")));

        assert(tnz.nextToken() == Token(sp, String("f")));
        assert(tnz.nextToken() == Token(sp, kParanOpen));
        assert(tnz.nextToken() == Token(sp, kQuote));
        assert(tnz.nextToken() == Token(sp, String("T")));
        assert(tnz.nextToken() == Token(sp, kParanClose));

        assert(tnz.nextToken() == Token(sp, kInt, 12));
        assert(tnz.nextToken() == Token(sp, kBackQuote));
        assert(tnz.nextToken() == Token(sp, String("mm")));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "##  ?val:name";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                    String("n.n."));

      try {
        assert(tnz.nextToken() == Token(sp, kSangHash));
        assert(tnz.nextToken() == Token(sp, kMacroParam, String("val:name")));
      }
      catch (const Exception& ne) {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }
  }
};

static TokenizerUnitTest tokenizerUnitTest;

#endif  // #if defined(UNITTESTS)

