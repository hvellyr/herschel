/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <string.h>

#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "registry.h"
#include "str.h"
#include "strbuf.h"
#include "tokenizer.h"

#include <UnitTest++.h>

using namespace heather;


Tokenizer::Tokenizer(Port<Char>* port, const String& srcName,
                     CharRegistry* charRegistry)
  : fPort(port),
    fSrcName(srcName),
    fLineCount(1),
    fCC(0xffff),
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
  return ( c == ' ' || c == '\n' || c == '\r' || c == '\t' ||
           c == '\f' || c == '\v' );
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
  return ( c == '-' || c == '_' || c == '*' || c == '+' ||
           c == '%' || c == '?' || c == '!' || c == '/' ||
           c == '|');
}


bool
Tokenizer::isSymbolChar(Char c) const
{
  return isAlpha(c) || isDigit(c) || isAlphaSpec(c);
}


bool
Tokenizer::isDelimiter(Char c) const
{
  return ( isWhitespace(c) ||
           c == '"' || c == '\'' ||
           c == '(' || c == ')' ||
           c == '[' || c == ']' ||
           c == '{' || c == '}' ||
           c == 0x300c || c == 0x300d ||
           c == '.' || c == ',' || c == ';' || c == '#' || c == '@' ||
           c == '^' );
}


bool
Tokenizer::isEOL(Char c) const
{
  return c == '\n' || c == '\r';
}


int
Tokenizer::nextChar()
{
  if (fCC == EOF)
    throw AnnotatedEofException(srcpos());

  try {
    int c = fPort->read();
    if (c == '\n' || c == '\r')
      fLineCount++;
    fCC = c;
  }
  catch (const EofException& ) {
    fCC = EOF;
  }
  return fCC;
}


void
Tokenizer::scanUntilDelimiter()
{
  while (fCC != EOF && !isDelimiter(fCC))
    nextChar();
}


SrcPos
Tokenizer::srcpos() const
{
  return SrcPos(fSrcName, fLineCount);
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


struct ReservedId
{
  String fName;
  TokenType   fType;
};

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
    if (token.idValue() == String("true"))
      return Token(startPos, kBool, true);
    else if (token.idValue() == String("false"))
      return Token(startPos, kBool, false);

    static const ReservedId reservedIds[] = {
      { String("**"),            kExponent     },
      { String("<=>"),           kCompare      },
      { String("=="),            kEqual        },
      { String("<>"),            kUnequal      },
      { String("<="),            kLessEqual    },
      { String(">="),            kGreaterEqual },
      { String("in"),            kIn           },
      { String("and"),           kLogicalAnd   },
      { String("or"),            kLogicalOr    },
      { String("mod"),           kMod          },
      { String("AND"),           kBitAnd       },
      { String("OR"),            kBitOr        },
      { String("XOR"),           kBitXor       },
      { String("<<"),            kShiftLeft    },
      { String(">>"),            kShiftRight   },
      { String("isa"),           kIsa          },
      { String("as"),            kAs           },
      { String("by"),            kBy           },
      { String("++"),            kAppend       },
      { String(MID_DefId),       kDefId        },
      { String(MID_ElseId),      kElseId       },
      { String(MID_EofId),       kEofId        },
      { String(MID_ExportId),    kExportId     },
      { String(MID_ExtendId),    kExtendId     },
      { String(MID_ExternId),    kExternId     },
      { String(MID_FUNCTIONId),  kFUNCTIONId   },
      { String(MID_ForId),       kForId        },
      { String(MID_FunctionId),  kFunctionId   },
      { String(MID_IfId),        kIfId         },
      { String(MID_ImportId),    kImportId     },
      { String(MID_LetId),       kLetId        },
      { String(MID_MatchId),     kMatchId      },
      { String(MID_ModuleId),    kModuleId     },
      { String(MID_NilId),       kNilId        },
      { String(MID_NotId),       kNotId        },
      { String(MID_OnId),        kOnId         },
      { String(MID_ReifyId),     kReifyId      },
      { String(MID_SelectId),    kSelectId     },
      { String(MID_ThenId),      kThenId       },
      { String(MID_WhenId),      kWhenId       },
      { String(MID_WhereId),     kWhereId      },
      { String(MID_WhileId),     kWhileId      },

      { String(),  kInvalid },      // sentinel
    };

    String tokenid = token.idValue();
    for (const ReservedId* p = &reservedIds[0]; p->fType != kInvalid; p++) {
      if (tokenid == p->fName)
        return Token(startPos, p->fType);
    }
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

  assert(!first.isEmpty());

  if (fCC == '.') {
    type = kReal;
    nextChar();
    second = readIntNumberPart(false);

    if (fCC == 'e' || fCC == 'E') {
      nextChar();
      if (fCC == '-' || fCC == '+') {
        expSign = fCC == '-' ? -1 : 1;
        nextChar();
        exponent = readIntNumberPart(false);
      }
      else {
        errorf(srcpos(), E_BadNumberNotation,
               "bad scientific notation: \\u0%x;", fCC);
        scanUntilDelimiter();
      }
    }
  }
  else if (fCC == '/') {
    type = kRational;
    nextChar();
    second = readIntNumberPart(true);
  }

  if (fCC == 'h' || fCC == 'H') {
    if (type != kInt)
      errorf(srcpos(), E_BadNumberNotation,
             "hexadecimal notation for unappropriate number type");
    else
      radix = 16;
    nextChar();
  }
  else if (fCC == 't' || fCC == 'T') {
    if (type != kInt)
      errorf(srcpos(), E_BadNumberNotation,
             "hexadecimal notation for unappropriate number type");
    else
      radix = 8;
    nextChar();
  }
  if (fCC == 'y' || fCC == 'Y') {
    if (type != kInt)
      errorf(srcpos(), E_BadNumberNotation,
             "hexadecimal notation for unappropriate number type");
    else
      radix = 2;
    nextChar();
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

    Token ct = Token(startPos, kChar, readc);
    if (needsTerminator) {
      if (fCC == ';') {
        nextChar();
        return ct;
      }
      else if (fCC == EOF) {
        errorf(startPos, E_UnterminatedChar, "file ended before char end");
        return ct;
      }
      else {
        errorf(startPos, E_UnterminatedChar, "unterminated char");
      }
    }
    else
      return Token(startPos, kChar, readc);
  }
  else {
    errorf(startPos, E_BadCharNotation, "expected integer notation for codepoint");
    return Token(startPos, kChar, 0xffff);
  }

  return Token();
}


Char
Tokenizer::mapCharNameToChar(const SrcPos& startPos, const String& charnm)
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

  error(startPos, E_UnknownCharName, String("Unknown char name: ") + charnm);
  return 0xffff;
}


Token
Tokenizer::translateChar(const SrcPos& startPos, const String& charnm)
{
  Char c = mapCharNameToChar(startPos, charnm);
  return Token(startPos, kChar, c);
}


Token
Tokenizer::readSymbolCharacter(const SrcPos& startPos, bool needsTerminator)
{
  Token sym = readIdentifier(startPos, String(), kSymbol,
                             false /* don't accept generics */);
  if (sym.type() != kId) {
    errorf(startPos, E_ExpectedCharName, "expected character symbol");
    return sym;
  }

  Token ct = (sym.idValue().length() == 1
              ? Token(startPos, kChar, sym.idValue()[0])
              : translateChar(startPos, sym.idValue()));
  if (needsTerminator) {
    if (fCC == ';') {
      nextChar();
      return ct;
    }
    else if (fCC == EOF) {
      errorf(startPos, E_UnterminatedChar, "file ended before char end");
      return ct;
    }
    else {
      errorf(startPos, E_UnterminatedChar, "unterminated char");
    }
  }

  return ct;
}


Token
Tokenizer::readNamedCharacter(const SrcPos& startPos, bool needsTerminator)
{
  if (isAlpha(fCC) || isDigit(fCC))
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
    if (isDigit(fCC))
      return readNumericCharacter(startPos, needsTerminator);
    else if (!isDelimiter(fCC)) {
      errorf(startPos, E_UnterminatedChar, "expected numerical char notation");
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
    errorf(startPos, E_UnterminatedChar, "incomplete char notation");
    return Token(startPos, kChar, 0xffff);
  }
}



Token
Tokenizer::readString(const SrcPos& startPos, int endChar, TokenType type)
{
  StringBuffer result;

  try {
    for ( ; ; ) {
      if (fCC == endChar) {
        nextChar();
        return Token(startPos, type, result.toString());
      }
      else if (fCC == '\\') {
        SrcPos charSp = srcpos();
        nextChar();
        Token ct = readCharacter(charSp, true /* needs terminator */);
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
    errorf(ae.srcpos(), E_UnterminatedString,
           "File ended before end of string.  Began on line %d",
           startPos.lineNumber());
    errorf(startPos, 0, "String started here");
    throw;
  }

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
      // whitespace
    case ' ': case '\n': case '\r': case '\t':
    case '\f': case '\v':
      nextChar();
      continue;

    case '(': return makeTokenAndNext(srcpos(), kParanOpen);
    case ')': return makeTokenAndNext(srcpos(), kParanClose);
    case '[': return makeTokenAndNext(srcpos(), kBracketOpen);
    case ']': return makeTokenAndNext(srcpos(), kBracketClose);
    case '{': return makeTokenAndNext(srcpos(), kBraceOpen);
    case '}': return makeTokenAndNext(srcpos(), kBraceClose);

      // utf8: e3 80 8c | 343 200 214
    case 0x300c: return makeTokenAndNext(srcpos(), kMacroOpen);
      // utf8: e3 80 8d | 343 200 215
    case 0x300d: return makeTokenAndNext(srcpos(), kMacroClose);

    case ',': return makeTokenAndNext(srcpos(), kComma);
    case ';': return makeTokenAndNext(srcpos(), kSemicolon);
    case ':': return makeTokenAndNext(srcpos(), kColon);

    case '@': return makeTokenAndNext(srcpos(), kAt);
    case '|':
      nextChar();
      if (isSymbolChar(fCC))
        return readIdentifier(beginSrcpos, String("|"), kSymbol, true);
      else
        return Token(beginSrcpos, kPipe);

    case '\'': return makeTokenAndNext(srcpos(), kQuote);
    case '^': return makeTokenAndNext(srcpos(), kReference);

    case '&':
      nextChar();
      if (fCC == '(')
        return makeTokenAndNext(beginSrcpos, kUnionOpen);
      return Token(beginSrcpos, kAmpersand);

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
        else
          return Token(beginSrcpos, kMinus);
      }

    case '?':
      nextChar();
      if (fCC == '"') {
        nextChar();
        Token param = readIdentifier(beginSrcpos, String(), kMacroParamAsStr, false);
        if (fCC != '"') {
          errorf(srcpos(), E_MissingApos, "Missing \" in ?\"-notation");
        }
        else
          nextChar();
        if (param.idValue().isEmpty()) {
          errorf(beginSrcpos, E_BadMacroPattern, "empty macro parameter");
          return Token();
        }

        return param;
      }
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
      return readString(beginSrcpos, '"', kString);

    case '~':
      nextChar();
      return readString(beginSrcpos, '~', kDocString);

    case '\\':
      {
        nextChar();
        Token ct = readCharacter(beginSrcpos, false); // need a terminator?
        if (ct.isSet())
          return ct;
        continue;
      }

    default:
      if (isAlpha(fCC) || isAlphaSpec(fCC))
        return readSymbolOrOperator(true);
      else if (isDigit(fCC))
        return readNumber(beginSrcpos, 1);
      else {
        errorf(beginSrcpos, E_UnexpectedChar,
               "unexpected char: \\u0%x", fCC);
        nextChar();
      }
    }
  }

  return Token();
}


#if defined(UNITTESTS)

#include <UnitTest++.h>


//----------------------------------------------------------------------------

SUITE(Tokenizer)
{
  TEST(Basic1)
  {
    SrcPos sp;

    static const char* test =
      "module zero (\"eyestep/zero 1.0:portables\")\n"
      "  export public(*)\n"
      "-- a simple portable class\n"
      "def class Portable<T>(x @ Int) : (Copyable, Comparable)\n"
      "{\n"
      "  slot first : T = x ;\n"
      "  slot data : Octet[]\n"
      "}\n";

    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, ::strlen(test))),
                  String("n.n."));

    CHECK_EQUAL(tnz.nextToken(), Token(sp, kModuleId));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("zero")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kString, String("eyestep/zero 1.0:portables")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

    CHECK_EQUAL(tnz.nextToken(), Token(sp, kExportId));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("public")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kMultiply));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

    CHECK_EQUAL(tnz.nextToken(), Token(sp, kDefId));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("class")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Portable")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericOpen));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("T")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericClose));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("x")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kAt));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Int")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kColon));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Copyable")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kComma));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Comparable")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

    CHECK_EQUAL(tnz.nextToken(), Token(sp, kBraceOpen));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("slot")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("first")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kColon));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("T")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kAssign));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("x")));

    CHECK_EQUAL(tnz.nextToken(), Token(sp, kSemicolon));

    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("slot")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("data")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kColon));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Octet")));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketOpen));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketClose));
    CHECK_EQUAL(tnz.nextToken(), Token(sp, kBraceClose));
  }


  TEST(Numbers)
  {
    SrcPos sp;

    static const char* test =
      "true false\n"
      "12345 0aaaah 0aBcDeFh 07123t 101101y 1y 2t 3h 4\n"
      "12.34 0.12345e+10 123.45e+7 12.3456e-5 -3.1415\n"
      "2/3 120/33 1/1024\n"
      "5i  3.1415i\n";
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."));

    try {
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBool, true));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBool, false));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 12345));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 0xaaaa));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 0xabcdef));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 07123));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 45));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 3));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 4));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReal, 12.34));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReal, 0.12345e+10));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReal, 0.12345e+10)); // normalized 123.45e+7
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReal, 0.000123456)); // normalized
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReal, -3.1415));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kRational, Rational(2, 3)));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kRational, Rational(120, 33)));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kRational, Rational(1, 1024)));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 5).setIsImaginary(true));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReal, 3.1415).setIsImaginary(true));
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }


  TEST(Chars)
  {
    SrcPos sp;

    static const char* test =
      "\\space  \\u60h  \\( \\newline \\cr\n"
      "\"hello,\\nl;world!\"  \"\\esc;\\u61h;\\(\\;;\"\n"
      "\\ga \\gong ";
    Ptr<CharRegistry> cr = new CharRegistry;
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."), cr);
    cr->registerValue(String("ga"), 0xac00);
    cr->registerValue(String("gong"), 0xacf5);

    try {
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kChar, 0x20));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kChar, 0x60));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kChar, '('));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kChar, 0x0a));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kChar, 0x0d));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kString, String("hello,\nworld!")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kString, String("\033a(;;")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kChar, 0xac00));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kChar, 0xacf5));
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }

  TEST(FunctionDefs)
  {
    SrcPos sp;

    static const char* test =
      "def f(args : &(String, Uri, Boolean)[] ...) ...\n"
      "  ~ Some function f, does not contain \\~ or similar Spuk.~\n"
      "def f(arg: _x = 0 .. 20 by 2)\n"
      "def g(a @ ^'T)\n"
      "def h(a : ^Repo) a^ = 5 &m = 4\n";
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."));

    try {
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDefId));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("f")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("args")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kColon));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kUnionOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("String")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kComma));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Uri")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kComma));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Boolean")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kEllipsis));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kEllipsis));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDocString,
                                      " Some function f, does not contain ~ or similar Spuk."));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDefId));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("f")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kKeyarg, String("arg")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("_x")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kAssign));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 0));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kRange));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 20));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBy));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDefId));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("g")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kAt));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReference));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kQuote));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("T")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDefId));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("h")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kColon));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReference));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Repo")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kReference));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kAssign));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 5));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kAmpersand));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("m")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kAssign));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 4));
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }


  TEST(KeywordStaticContainer)
  {
    SrcPos sp;

    static const char* test =
      "#abc #delft\n"
      "#[1, 2] #[]\n"
      "#(1 -> 2) #()\n"
      "&(1, 2)\n";
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."));

    try {
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kKeyword, String("abc")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kKeyword, String("delft")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLiteralArrayOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kComma));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLiteralArrayOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLiteralVectorOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kMapTo));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLiteralVectorOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kUnionOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kComma));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }


  TEST(Generics)
  {
    SrcPos sp;

    static const char* test =
      "Buffer<Int>()[i] < 10 and true or false\n"
      "T<S<Y>>  T<S<Y> >  a < b\n"
      "val << 5 val >> 2\n"
      "2 < 1  2 <= 1  2 > 1  2 >= 1  2 <=> 1  2 <> 1  2 == 1\n"
      "a + b  \"a\" ++ \"b\" a - b  a * b  a / b  a ** 2  a mod 5\n"
      "1 XOR 2  1 OR 2  1 AND 2\n"
      "1 % 2  1 -> 2  1 in 2  1 isa Number  1 as Octet\n"
      "|abc ->abc\n";
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."));

    try {
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Buffer")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Int")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("i")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBracketClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLess));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 10));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLogicalAnd));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBool, true));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLogicalOr));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBool, false));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("T")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("S")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Y")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("T")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("S")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Y")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericClose));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGenericClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLess));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("b")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("val")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kShiftLeft));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 5));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("val")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kShiftRight));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLess));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kLessEqual));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGreater));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kGreaterEqual));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kCompare));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kUnequal));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kEqual));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kPlus));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("b")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kString, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kAppend));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kString, String("b")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kMinus));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("b")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kMultiply));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("b")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDivide));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("b")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kExponent));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("a")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kMod));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 5));


      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBitXor));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBitOr));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kBitAnd));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));


      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kFold));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kMapTo));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kIn));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 2));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kIsa));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Number")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 1));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kAs));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("Octet")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("|abc")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("->abc")));
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }


  TEST(Namespaces)
  {
    SrcPos sp;

    static const char* test =
      "io|File  self.io|val.display\n"
      "f('T)  12'mm\n";
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."));

    try {
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("io|File")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("self")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDot));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("io|val")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kDot));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("display")));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("f")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanOpen));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kQuote));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("T")));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kParanClose));

      CHECK_EQUAL(tnz.nextToken(), Token(sp, kInt, 12));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kQuote));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, String("mm")));
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }


  TEST(MacroVars)
  {
    SrcPos sp;

    static const char* test =
      "##  ?val:name ?\"abc\" ?\"\" ";
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."));

    try {
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kSangHash));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kMacroParam, "val:name"));
      CHECK_EQUAL(tnz.nextToken(), Token(sp, kMacroParamAsStr, "abc"));

      {
        LogSurpressor beSilent;
        // ?"" is not allowed.
        CHECK_EQUAL(tnz.nextToken(), Token());
      }
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }

  TEST(SpecMacroBrackets)
  {
    SrcPos sp;

    static const char* test =
      "\343\200\214 xyz \343\200\215 ";
    Tokenizer tnz(new CharPort(new DataPort((Octet*)test, strlen(test))),
                  String("n.n."));

    try {
      assert(tnz.nextToken() == Token(sp, kMacroOpen));
      assert(tnz.nextToken() == Token(sp, kSymbol, "xyz"));
      assert(tnz.nextToken() == Token(sp, kMacroClose));
    }
    catch (const Exception& ne) {
      logf(kError, StrHelper(ne.message()));
    }
  }
}

#endif  // #if defined(UNITTESTS)

