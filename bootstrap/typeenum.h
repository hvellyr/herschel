/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_typeenum_h
#define bootstrap_typeenum_h

#include "refcountable.h"

namespace heather
{
  class SrcPos;
  class Token;

  class TypeEnumMaker : public RefCountable
  {
  public:
    ~TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const = 0;
  };


  class IntTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~IntTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class BoolTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~BoolTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class CharTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~CharTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class KeywordTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~KeywordTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class OctetTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~OctetTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class RationalTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~RationalTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class RealTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~RealTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class ShortTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~ShortTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class UShortTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~UShortTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class WordTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~WordTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class UWordTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~UWordTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class LongTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~LongTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class ULongTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~ULongTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class StringTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~StringTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class EofTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~EofTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class NilTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~NilTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class UnspecifiedTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~UnspecifiedTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class DoubleTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~DoubleTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class FloatTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~FloatTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class LongDoubleTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~LongDoubleTypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };
};                              // namespace

#endif                          // bootstrap_typeenum_h
