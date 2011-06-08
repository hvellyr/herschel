/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_typeenum_h
#define bootstrap_typeenum_h

#include "refcountable.h"

namespace herschel
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


  class RationalTypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~RationalTypeEnumMaker() { }

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


  class Float32TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~Float32TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class Float64TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~Float64TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class Float128TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~Float128TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class Int8TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~Int8TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class UInt8TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~UInt8TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class Int16TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~Int16TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class UInt16TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~UInt16TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class Int32TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~Int32TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class UInt32TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~UInt32TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class Int64TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~Int64TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };


  class UInt64TypeEnumMaker : public TypeEnumMaker
  {
  public:
    ~UInt64TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const;
  };
};                              // namespace

#endif                          // bootstrap_typeenum_h
