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


using namespace heather;


Tokenizer::Tokenizer(Port<Char>* port)
  : fPort(port),
    fLineCount(1),
    fCC(EOF),
    fNextCharIsGenericOpen(false),
    fInGenericContext(0)
{
  assert(port != NULL);

  nextChar();
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
  catch (const EofException& )
  {
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
    while (!isDelimiter(fCC))
    {
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
  catch (const EofException& )
  {
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
      return Token(kSymbol, String() + currentChar);
    }
  }

  Token token = readIdentifier(String() + currentChar, kSymbol, acceptGenerics);
  if (token.fType == kSymbol) {
    if (token.fStrValue == String("**"))
      return Token(kExponent);
    else if (token.fStrValue == String("<=>"))
      return Token(kCompare);
    else if (token.fStrValue == String("=="))
      return Token(kEqual);
    else if (token.fStrValue == String("<>"))
      return Token(kUnequal);
    else if (token.fStrValue == String("<>"))
      return Token(kUnequal);
    else if (token.fStrValue == String("<="))
      return Token(kLessEqual);
    else if (token.fStrValue == String(">="))
      return Token(kGreaterEqual);
    else if (token.fStrValue == String("in"))
      return Token(kIn);
    else if (token.fStrValue == String("and"))
      return Token(kLogicalAnd);
    else if (token.fStrValue == String("or"))
      return Token(kLogicalOr);
    else if (token.fStrValue == String("mod"))
      return Token(kMod);
    else if (token.fStrValue == String("AND"))
      return Token(kBitAnd);
    else if (token.fStrValue == String("OR"))
      return Token(kBitOr);
    else if (token.fStrValue == String("XOR"))
      return Token(kBitXor);
    else if (token.fStrValue == String("<<"))
      return Token(kShiftLeft);
    else if (token.fStrValue == String(">>"))
      return Token(kShiftRight);
    else if (token.fStrValue == String("isa"))
      return Token(kIsa);
    else if (token.fStrValue == String("as"))
      return Token(kAs);
    else if (token.fStrValue == String("by"))
      return Token(kBy);
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
      token = Token(kRational, fval * sign, sval);
    }
    break;

  default:
    assert(0);
  }

  token.fIsImaginary = isImaginary;

  return token;
}


Token
Tokenizer::readNumericCharacter(bool needsTerminator)
{
  Token token = readNumber(1);
  if (token.fType == kInteger) {
    int readc = token.fIntValue;

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
  if (sym.fType != kSymbol)
    parseError(String("expected character symbol"));

  Token ct = (sym.fStrValue.length() == 1
              ? Token(kChar, sym.fStrValue[0])
              : translateChar(sym.fStrValue));
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
        if (ct.fType == kChar)
          result = result + Char(ct.fIntValue);
        else
          parseError(String("Char expected"));
      }
      else {
        result = result + Char(fCC);
        nextChar();
      }
    }
  }
  catch (const EofException& )
  {
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
    printf("%s ", t.c_str());
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


//----------------------------------------------------------------------------

Token& Token::operator=(const Token& other)
{
  fType = other.fType;
  fStrValue = other.fStrValue;
  fIntValue = other.fIntValue;
  fInt2Value = other.fInt2Value;
  fDoubleValue = other.fDoubleValue;
  fIsImaginary = other.fIsImaginary;

  return *this;
}


bool
Token::operator==(const Token& other) const
{
  if (fType == other.fType) {
    switch (fType) {
    case kString:
    case kSymbol:
    case kKeyarg:
    case kMacroParam:
    case kKeyword:
      return fStrValue == other.fStrValue;

    case kChar:
      return fIntValue == other.fIntValue;

    case kInteger:
      return fIntValue == other.fIntValue &&
        fIsImaginary == other.fIsImaginary;
    case kReal:
      return fDoubleValue == other.fDoubleValue &&
        fIsImaginary == other.fIsImaginary;
    case kRational:
      return fIntValue == other.fIntValue &&
        fInt2Value == other.fInt2Value &&
        fIsImaginary == other.fIsImaginary;

    default:
      return true;
    }
  }
  return false;
}


const char*
Token::c_str() const
{
  static char buffer[512];
  buffer[0] = '\0';

  switch (fType) {
  case kEOF:              strcpy(buffer, "EOF"); break;
  case kInvalid:          strcpy(buffer, "INVALID"); break;
  case kPlus:             strcpy(buffer, "+"); break;
  case kMinus:            strcpy(buffer, "-"); break;
  case kDivide:           strcpy(buffer, "/"); break;
  case kMultiply:         strcpy(buffer, "*"); break;
  case kExponent:         strcpy(buffer, "**"); break;
  case kFold:             strcpy(buffer, "%"); break;
  case kCompare:          strcpy(buffer, "<=>"); break;
  case kEqual:            strcpy(buffer, "=="); break;
  case kUnequal:          strcpy(buffer, "<>"); break;
  case kLess:             strcpy(buffer, "<"); break;
  case kLessEqual:        strcpy(buffer, "<="); break;
  case kGreater:          strcpy(buffer, ">"); break;
  case kGreaterEqual:     strcpy(buffer, ">="); break;
  case kAssign:           strcpy(buffer, "="); break;
  case kMapTo:            strcpy(buffer, "->"); break;
  case kIn:               strcpy(buffer, "in"); break;
  case kMod:              strcpy(buffer, "mod"); break;
  case kIsa:              strcpy(buffer, "isa"); break;
  case kAs:               strcpy(buffer, "as"); break;
  case kBy:               strcpy(buffer, "by"); break;
  case kLogicalAnd:       strcpy(buffer, "and"); break;
  case kLogicalOr:        strcpy(buffer, "or"); break;
  case kBitAnd:           strcpy(buffer, "AND"); break;
  case kBitOr:            strcpy(buffer, "OR"); break;
  case kBitXor:           strcpy(buffer, "XOR"); break;
  case kShiftLeft:        strcpy(buffer, "<<"); break;
  case kShiftRight:       strcpy(buffer, ">>"); break;

  case kString:
    buffer[0] = '"';
    fStrValue.toUtf8(buffer + 1, 510);
    strcat(buffer, "\"");
    break;
      
  case kChar:
    sprintf(buffer, "\\u0%x;", fIntValue);
    break;
  case kSymbol:
    fStrValue.toUtf8(buffer, 510);
    break;
  case kKeyarg:
    fStrValue.toUtf8(buffer, 510);
    strcat(buffer, ":");
    break;
  case kMacroParam:
    buffer[0] = '?';
    fStrValue.toUtf8(buffer + 1, 510);
    break;
  case kKeyword:
    buffer[0] = '#';
    fStrValue.toUtf8(buffer + 1, 510);
    break;

  case kInteger:  
    sprintf(buffer, "%d", fIntValue);
    if (fIsImaginary)
      strcat(buffer, "i");
    break;
  case kReal:
    sprintf(buffer, "%lf", fDoubleValue);
    if (fIsImaginary)
      strcat(buffer, "i");
    break;
  case kRational:
    sprintf(buffer, "%d/%d", fIntValue, fInt2Value);
    if (fIsImaginary)
      strcat(buffer, "i");
    break;

  case kParanOpen:          strcpy(buffer, "("); break;
  case kParanClose:         strcpy(buffer, ")"); break;
  case kBracketOpen:        strcpy(buffer, "["); break;
  case kBracketClose:       strcpy(buffer, "]"); break;
  case kBraceOpen:          strcpy(buffer, "{"); break;
  case kBraceClose:         strcpy(buffer, "}"); break;
  case kGenericOpen:        strcpy(buffer, "<"); break;
  case kGenericClose:       strcpy(buffer, ">"); break;
  case kComma:              strcpy(buffer, ","); break;
  case kSemicolon:          strcpy(buffer, ";"); break;
  case kColon:              strcpy(buffer, ":"); break;
  case kAt:                 strcpy(buffer, "@"); break;
  case kAmpersand:          strcpy(buffer, "&"); break;
  case kPipe:               strcpy(buffer, "|"); break;
  case kBackQuote:          strcpy(buffer, "`"); break;
  case kQuote:              strcpy(buffer, "'"); break;
  case kEllipsis:           strcpy(buffer, "..."); break;
  case kRange:              strcpy(buffer, ".."); break;
  case kDot:                strcpy(buffer, "."); break;

  case kLiteralVectorOpen:  strcpy(buffer, "#("); break;
  case kLiteralArrayOpen:   strcpy(buffer, "#["); break;
  case kSangHash:           strcpy(buffer, "##"); break;
  };
  
  return buffer;
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

      try
      {
        assert(tnz.nextToken() == Token(kSymbol, String("interface")));
        assert(tnz.nextToken() == Token(kSymbol, String("zero")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kString, String("eyestep/zero 1.0:portables")));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(kSymbol, String("export")));
        assert(tnz.nextToken() == Token(kSymbol, String("public")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kMultiply));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(kSymbol, String("def")));
        assert(tnz.nextToken() == Token(kSymbol, String("class")));
        assert(tnz.nextToken() == Token(kSymbol, String("Portable")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("T")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("x")));
        assert(tnz.nextToken() == Token(kAt));
        assert(tnz.nextToken() == Token(kSymbol, String("Int")));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("Copyable")));
        assert(tnz.nextToken() == Token(kComma));
        assert(tnz.nextToken() == Token(kSymbol, String("Comparable")));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(kBraceOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("slot")));
        assert(tnz.nextToken() == Token(kSymbol, String("first")));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(kSymbol, String("T")));
        assert(tnz.nextToken() == Token(kAssign));
        assert(tnz.nextToken() == Token(kSymbol, String("x")));

        assert(tnz.nextToken() == Token(kSemicolon));

        assert(tnz.nextToken() == Token(kSymbol, String("slot")));
        assert(tnz.nextToken() == Token(kSymbol, String("data")));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(kSymbol, String("Octet")));
        assert(tnz.nextToken() == Token(kBracketOpen));
        assert(tnz.nextToken() == Token(kBracketClose));
        assert(tnz.nextToken() == Token(kBraceClose));
      }
      catch (const NotationException& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "12345 0aaaah 0aBcDeFh 07123t 101101y 1y 2t 3h 4\n"
        "12.34 0.12345e+10 123.45e+7 12.3456e-5 -3.1415\n"
        "2/3 120/33 1/1024\n"
        "5i  3.1415i\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try
      {
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
        assert(tnz.nextToken() == Token(kReal, -3.1315));

        assert(tnz.nextToken() == Token(kRational, 2, 3));
        assert(tnz.nextToken() == Token(kRational, 120, 33));
        assert(tnz.nextToken() == Token(kRational, 1, 1024));
        assert(tnz.nextToken() == Token(kInteger, 5).setIsImaginary(true));
        assert(tnz.nextToken() == Token(kReal, 3.1415).setIsImaginary(true));
      }
      catch (const Exception& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "\\space  \\u60h  \\( \\newline \\cr"
        "\"hello,\\nl;world!\"  \"\\esc;\\u61h;\\(\\;;\"\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try
      {
        assert(tnz.nextToken() == Token(kChar, 0x20));
        assert(tnz.nextToken() == Token(kChar, 0x60));
        assert(tnz.nextToken() == Token(kChar, '('));
        assert(tnz.nextToken() == Token(kChar, 0x0a));
        assert(tnz.nextToken() == Token(kChar, 0x0d));

        assert(tnz.nextToken() == Token(kString, String("hello,\nworld!")));
        assert(tnz.nextToken() == Token(kString, String("\033a(;;")));
      }
      catch (const Exception& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "def f(args : &(String, Uri, Boolean)[] ...) ...\n"
        "def f(arg: _x = 0 .. 20 by 2)\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try
      {
        assert(tnz.nextToken() == Token(kSymbol, String("def")));
        assert(tnz.nextToken() == Token(kSymbol, String("f")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("args")));
        assert(tnz.nextToken() == Token(kColon));
        assert(tnz.nextToken() == Token(kAmpersand));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("String")));
        assert(tnz.nextToken() == Token(kComma));
        assert(tnz.nextToken() == Token(kSymbol, String("Uri")));
        assert(tnz.nextToken() == Token(kComma));
        assert(tnz.nextToken() == Token(kSymbol, String("Boolean")));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kBracketOpen));
        assert(tnz.nextToken() == Token(kBracketClose));
        assert(tnz.nextToken() == Token(kEllipsis));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kEllipsis));

        assert(tnz.nextToken() == Token(kSymbol, String("def")));
        assert(tnz.nextToken() == Token(kSymbol, String("f")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kKeyarg, String("arg")));
        assert(tnz.nextToken() == Token(kSymbol, String("_x")));
        assert(tnz.nextToken() == Token(kAssign));
        assert(tnz.nextToken() == Token(kInteger, 0));
        assert(tnz.nextToken() == Token(kRange));
        assert(tnz.nextToken() == Token(kInteger, 20));
        assert(tnz.nextToken() == Token(kBy));
        assert(tnz.nextToken() == Token(kInteger, 2));
        assert(tnz.nextToken() == Token(kParanClose));
      }
      catch (const Exception& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "#abc #delft\n"
        "#[1, 2] #[]\n"
        "#(1 -> 2) #()\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try
      {
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
      catch (const Exception& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "Buffer<Int>()[i] < 10 and true or false\n"
        "T<S<Y>>  T<S<Y> >  a < b\n"
        "val << 5 val >> 2\n"
        "2 < 1  2 <= 1  2 > 1  2 >= 1  2 <=> 1  2 <> 1  2 == 1\n"
        "a + b  a - b  a * b  a / b  a ** 2  a mod 5\n"
        "1 XOR 2  1 OR 2  1 AND 2\n"
        "1 % 2  1 -> 2  1 in 2  1 isa Number  1 as Octet\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try
      {
        assert(tnz.nextToken() == Token(kSymbol, String("Buffer")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("Int")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kParanClose));
        assert(tnz.nextToken() == Token(kBracketOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("i")));
        assert(tnz.nextToken() == Token(kBracketClose));
        assert(tnz.nextToken() == Token(kLess));
        assert(tnz.nextToken() == Token(kInteger, 10));
        assert(tnz.nextToken() == Token(kLogicalAnd));
        assert(tnz.nextToken() == Token(kSymbol, String("true")));
        assert(tnz.nextToken() == Token(kLogicalOr));
        assert(tnz.nextToken() == Token(kSymbol, String("false")));

        assert(tnz.nextToken() == Token(kSymbol, String("T")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("S")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("Y")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kGenericClose));

        assert(tnz.nextToken() == Token(kSymbol, String("T")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("S")));
        assert(tnz.nextToken() == Token(kGenericOpen));
        assert(tnz.nextToken() == Token(kSymbol, String("Y")));
        assert(tnz.nextToken() == Token(kGenericClose));
        assert(tnz.nextToken() == Token(kGenericClose));

        assert(tnz.nextToken() == Token(kSymbol, String("a")));
        assert(tnz.nextToken() == Token(kLess));
        assert(tnz.nextToken() == Token(kSymbol, String("b")));

        assert(tnz.nextToken() == Token(kSymbol, String("val")));
        assert(tnz.nextToken() == Token(kShiftLeft));
        assert(tnz.nextToken() == Token(kInteger, 5));
        assert(tnz.nextToken() == Token(kSymbol, String("val")));
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

        assert(tnz.nextToken() == Token(kSymbol, String("a")));
        assert(tnz.nextToken() == Token(kPlus));
        assert(tnz.nextToken() == Token(kSymbol, String("b")));

        assert(tnz.nextToken() == Token(kSymbol, String("a")));
        assert(tnz.nextToken() == Token(kMinus));
        assert(tnz.nextToken() == Token(kSymbol, String("b")));

        assert(tnz.nextToken() == Token(kSymbol, String("a")));
        assert(tnz.nextToken() == Token(kMultiply));
        assert(tnz.nextToken() == Token(kSymbol, String("b")));

        assert(tnz.nextToken() == Token(kSymbol, String("a")));
        assert(tnz.nextToken() == Token(kDivide));
        assert(tnz.nextToken() == Token(kSymbol, String("b")));

        assert(tnz.nextToken() == Token(kSymbol, String("a")));
        assert(tnz.nextToken() == Token(kExponent));
        assert(tnz.nextToken() == Token(kInteger, 2));

        assert(tnz.nextToken() == Token(kSymbol, String("a")));
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
        assert(tnz.nextToken() == Token(kSymbol, String("Number")));

        assert(tnz.nextToken() == Token(kInteger, 1));
        assert(tnz.nextToken() == Token(kAs));
        assert(tnz.nextToken() == Token(kSymbol, String("Octet")));

      }
      catch (const Exception& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "io|File  self.io|val.display\n"
        "f('T)  12`mm\n";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try
      {
        assert(tnz.nextToken() == Token(kSymbol, String("io")));
        assert(tnz.nextToken() == Token(kPipe));
        assert(tnz.nextToken() == Token(kSymbol, String("File")));

        assert(tnz.nextToken() == Token(kSymbol, String("self")));
        assert(tnz.nextToken() == Token(kDot));
        assert(tnz.nextToken() == Token(kSymbol, String("io")));
        assert(tnz.nextToken() == Token(kPipe));
        assert(tnz.nextToken() == Token(kSymbol, String("val")));
        assert(tnz.nextToken() == Token(kDot));
        assert(tnz.nextToken() == Token(kSymbol, String("display")));

        assert(tnz.nextToken() == Token(kSymbol, String("f")));
        assert(tnz.nextToken() == Token(kParanOpen));
        assert(tnz.nextToken() == Token(kQuote));
        assert(tnz.nextToken() == Token(kSymbol, String("T")));
        assert(tnz.nextToken() == Token(kParanClose));

        assert(tnz.nextToken() == Token(kInteger, 12));
        assert(tnz.nextToken() == Token(kBackQuote));
        assert(tnz.nextToken() == Token(kSymbol, String("mm")));
      }
      catch (const Exception& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }

    {
      static const char* test =
        "##  ?val:name";
      Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))));

      try
      {
        assert(tnz.nextToken() == Token(kSangHash));
        assert(tnz.nextToken() == Token(kMacroParam, String("val:name")));
      }
      catch (const Exception& ne)
      {
        fprintf(stderr, "ERROR: %s\n", (const char*)StrHelper(ne.message()));
      }
    }
  }
};

static TokenizerUnitTest tokenizerUnitTest;

#endif  // #if defined(UNITTESTS)

