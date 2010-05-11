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
#include "token.h"


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
