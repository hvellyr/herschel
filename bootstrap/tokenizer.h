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

  class AnnotatedEofException : public EofException
  {
  public:
    AnnotatedEofException(const SrcPos& srcpos)
      : EofException(),
        fSrcPos(srcpos)
    { }

    const SrcPos& srcpos() const
    {
      return fSrcPos;
    }

  private:
    SrcPos fSrcPos;
  };


  //--------------------------------------------------------------------------

  typedef Registry<int> CharRegistry;


  //--------------------------------------------------------------------------

  class Tokenizer : public RefCountable
  {
  public:
    Tokenizer(Port<Char>* port, const String& srcName,
              CharRegistry* charRegistry = NULL);

    bool isEof() const;

    Token nextToken();
    Token nextTokenImpl();

    SrcPos srcpos() const;

  private:
    void readCommentLine();
    Token readIdentifier(const SrcPos& startPos,
                         const String& prefix, TokenType type,
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

    int nextChar();
    void scanUntilDelimiter();
    Token makeTokenAndNext(const SrcPos& where, TokenType type);

    Token readSymbolOrOperator(bool acceptGenerics);
    Token readNumber(const SrcPos& startPos, int sign);
    Token readCharacter(const SrcPos& startPos, bool needsTerminator);
    Token readString(const SrcPos& startPos);
    Token readNamedCharacter(const SrcPos& startPos, bool needsTerminator);
    Token readNumericCharacter(const SrcPos& startPos, bool needsTerminator);
    Token readSymbolCharacter(const SrcPos& startPos, bool needsTerminator);
    Token translateChar(const SrcPos& startPos, const String& charnm);
    Char mapCharNameToChar(const SrcPos& startPos, const String& charnm);
    String readIntNumberPart(bool acceptHex);



    //-------- data member
    Ptr<Port<Char> > fPort;
    String fSrcName;
    int    fLineCount;
    int    fCC;
    bool   fNextCharIsGenericOpen;
    int    fInGenericContext;
    Ptr<CharRegistry> fCharRegistry;
  };
};

#endif // bootstrap_tokenizer_h
