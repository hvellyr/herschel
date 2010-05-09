/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_tokenizer_h
#define bootstrap_tokenizer_h

#include "port.h"
#include "numbers.h"
#include "registry.h"


namespace heather
{
  class String;

  //--------------------------------------------------------------------------

  class NotationException : public Exception
  {
  public:
    NotationException(int line, const String& msg)
      : Exception(msg),
        fLine(line)
    { }

    int line() const
    {
      return fLine;
    }

  private:
    int fLine;
  };


  //--------------------------------------------------------------------------

  typedef Registry<int> CharRegistry;


  //--------------------------------------------------------------------------
  
  enum TokenType
  {
    kEOF,
    kInvalid,

    kPlus,
    kMinus,
    kDivide,
    kMultiply,
    kExponent,
    kFold,
    kCompare,
    kEqual,
    kUnequal,
    kLess,
    kLessEqual,
    kGreater,
    kGreaterEqual,
    kAssign,
    kMapTo,
    kIn,
    kMod,
    kIsa,
    kAs,
    kBy,
    kLogicalAnd,
    kLogicalOr,
    kBitAnd,
    kBitOr,
    kBitXor,
    kShiftLeft,
    kShiftRight,

    kString,
    kChar,
    kBool,
    kInteger,
    kReal,
    kRational,

    kSymbol,
    kKeyarg,
    kMacroParam,
    kKeyword,

    kParanOpen,
    kParanClose,
    kBracketOpen,
    kBracketClose,
    kBraceOpen,
    kBraceClose,
    kGenericOpen,
    kGenericClose,
    kComma,
    kSemicolon,
    kColon,

    kAt,
    kAmpersand,
    kPipe,
    kBackQuote,
    kQuote,
    kEllipsis,
    kRange,
    kDot,

    kLiteralVectorOpen,
    kLiteralArrayOpen,
    kSangHash,
  };


  class Token
  {
  public:
    Token()
      : fType(kInvalid),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
    { }

    Token(const Token& other)
    {
      *this = other;
    }

    Token(TokenType type)
      : fType(type),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
    { }

    Token(TokenType type, const String& value)
      : fType(type),
        fStrValue(value),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
    { }

    Token(TokenType type, const char* value)
      : fType(type),
        fStrValue(String(value)),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
    { }

    Token(TokenType type, int value)
      : fType(type),
        fBoolValue(false),
        fIntValue(value),
        fDoubleValue(0.0),
        fIsImaginary(false)
    {
      assert(type == kInteger);
    }


    Token(TokenType type, bool value)
      : fType(type),
        fBoolValue(value),
        fIntValue(0),
        fDoubleValue(0.0),
        fIsImaginary(false)
    {
      assert(type == kBool);
    }

    Token(TokenType type, double value)
      : fType(type),
        fBoolValue(false),
        fIntValue(0),
        fDoubleValue(value),
        fIsImaginary(false)
    { }

    Token(TokenType type, const Rational& rat)
      : fType(type),
        fBoolValue(false),
        fIntValue(0),
        fRationalValue(rat),
        fDoubleValue(0.0),
        fIsImaginary(false)
    { }

    Token& operator=(const Token& other);

    bool operator==(const Token& other) const;
    bool operator!=(const Token& other) const
    {
      return !(operator==(other));
    }

    bool isSymbol() const
    {
      return fType == kSymbol;
    }

    bool isKeyArg() const
    {
      return (fType == kSymbol && fStrValue.length() > 1 &&
              fStrValue[fStrValue.length() - 1] == ':');
    }

    String toString() const;

    Token& setIsImaginary(bool value)
    {
      fIsImaginary = value;
      return *this;
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

  String operator+(const String& one, const Token& two);


  //--------------------------------------------------------------------------

  class Tokenizer : public RefCountable
  {
  public:
    Tokenizer(Port<Char>* port, CharRegistry* charRegistry = NULL);

    bool isEof() const;

    Token nextToken();
    Token nextTokenImpl();

  private:
    void readCommentLine();
    Token readIdentifier(const String& prefix, TokenType type,
                         bool acceptGenerics);

    bool isEOL(Char c) const;
    bool isInCharRange(Char c, Char from, Char to) const;
    bool isWhitespace(Char c) const;
    bool isDigit(Char c) const;
    bool isHexDigit(Char c) const;
    bool isAlpha(Char c) const;
    bool isAlphaSpec(Char c) const;
    bool isSymbolChar(Char c) const;
    bool isDelimiter(Char c) const;

    void parseError(const String& msg)
      throw (NotationException);

    int nextChar();
    Token makeTokenAndNext(TokenType type);

    Token readSymbolOrOperator(bool acceptGenerics);
    Token readNumber(int sign);
    Token readCharacter(bool needsTerminator);
    Token readString();
    Token readNamedCharacter(bool needsTerminator);
    Token readNumericCharacter(bool needsTerminator);
    Token readSymbolCharacter(bool needsTerminator);
    Token translateChar(const String& charnm);
    Char mapCharNameToChar(const String& charnm);
    String readIntNumberPart(bool acceptHex);



    //-------- data member
    Ptr<Port<Char> > fPort;
    int fLineCount;
    int fCC;
    bool fNextCharIsGenericOpen;
    int fInGenericContext;
    Ptr<CharRegistry> fCharRegistry;
  };
};

#endif // bootstrap_tokenizer_h
