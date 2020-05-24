/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "numbers.hpp"
#include "port.hpp"
#include "registry.hpp"
#include "token.hpp"

#include <memory>


namespace herschel {
class String;

//! An EofException which carries a detailed source position annotation.
//!
//! The source annotation should encode the position where EOF occured but
//! where a construct was started which was found to be unfinished at EOF.
class AnnotatedEofException : public EofException {
public:
  AnnotatedEofException(const SrcPos& srcpos)
      : EofException()
      , fSrcPos(srcpos)
  {
  }

  //! Returns the annotated source position.
  const SrcPos& srcpos() const { return fSrcPos; }

private:
  SrcPos fSrcPos;
};


//! The type for the character registry.
//!
//! It lists characters (unicode code points) by their character names.
using CharRegistry = Registry<int>;


class Tokenizer {
public:
  Tokenizer(std::shared_ptr<Port<Char>> port, const String& srcName,
            std::shared_ptr<CharRegistry> charRegistry = nullptr);

  bool isEof() const;

  Token nextToken();
  Token nextTokenImpl();

  SrcPos srcpos() const;

private:
  void readCommentLine();
  Token readIdentifier(const SrcPos& startPos, const String& prefix, TokenType type,
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
  Token readString(const SrcPos& startPos, int endChar, TokenType type);
  Token readNamedCharacter(const SrcPos& startPos, bool needsTerminator);
  Token readNumericCharacter(const SrcPos& startPos, bool needsTerminator);
  Token readSymbolCharacter(const SrcPos& startPos, bool needsTerminator);
  Token translateChar(const SrcPos& startPos, const String& charnm);
  Char mapCharNameToChar(const SrcPos& startPos, const String& charnm);
  String readIntNumberPart(bool acceptHex);

  Token toInt(const SrcPos& startPos, const String& token, int radix, int reqbitsize,
              bool isUnsigned, int sign);

  std::shared_ptr<Port<Char>> fPort;
  String fSrcName;
  int fLineCount;
  int fColumnCount;
  int fCC;
  bool fNextCharIsGenericOpen;
  int fInGenericContext;
  std::shared_ptr<CharRegistry> fCharRegistry;
};

}  // namespace herschel
