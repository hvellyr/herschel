/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once


namespace herschel
{
  class SrcPos;
  class Token;

  class TypeEnumMaker
  {
  public:
    virtual ~TypeEnumMaker() { }

    virtual Token nextEnumItem(const SrcPos& srcpos,
                               const Token& enumItemSymbol,
                               const Token& lastInitToken) const = 0;
  };


  class BoolTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class CharTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class KeywordTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class RationalTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class StringTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class EofTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class NilTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class UnspecifiedTypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class Float32TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class Float64TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class Float128TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class Int8TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class UInt8TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class Int16TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class UInt16TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class Int32TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class UInt32TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class Int64TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };


  class UInt64TypeEnumMaker : public TypeEnumMaker
  {
  public:
    Token nextEnumItem(const SrcPos& srcpos,
                       const Token& enumItemSymbol,
                       const Token& lastInitToken) const override;
  };
};                              // namespace
