/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "common.h"

#include "type.h"
#include "typeenum.h"
#include "errcodes.h"
#include "log.h"
#include "srcpos.h"


using namespace heather;


#define NUMBER_TYPE_ENUM_MAKER(_TypeName, _kind, _method, _init, _step)  \
Token                                                                   \
_TypeName ## TypeEnumMaker::nextEnumItem(const SrcPos& srcpos,          \
                                         const Token& enumItemSymbol,   \
                                         const Token& lastInitToken) const \
{                                                                       \
  if (!lastInitToken.isSet()) {                                         \
    return Token(srcpos, k ## _kind, _init);                            \
  }                                                                     \
  else if (lastInitToken == k ## _kind) {                               \
    return Token(srcpos, k ## _kind,                                    \
                 lastInitToken. _method ## Value() + _step);            \
  }                                                                     \
                                                                        \
  errorf(srcpos, E_EnumInitTypeMismatch,                                \
         "Init value does not match enum type");                        \
  return Token();                                                       \
}


NUMBER_TYPE_ENUM_MAKER(Int, Int, int, 0, 1)
NUMBER_TYPE_ENUM_MAKER(Long, Int, int, 0, 1)
NUMBER_TYPE_ENUM_MAKER(ULong, Int, int, 0, 1)
NUMBER_TYPE_ENUM_MAKER(Short, Int, int, 0, 1)
NUMBER_TYPE_ENUM_MAKER(UShort, Int, int, 0, 1)
NUMBER_TYPE_ENUM_MAKER(Word, Int, int, 0, 1)
NUMBER_TYPE_ENUM_MAKER(UWord, Int, int, 0, 1)
NUMBER_TYPE_ENUM_MAKER(Real, Real, real, 0.0, 1.0)
NUMBER_TYPE_ENUM_MAKER(Float, Real, real, 0.0, 1.0)
NUMBER_TYPE_ENUM_MAKER(Double, Real, real, 0.0, 1.0)
NUMBER_TYPE_ENUM_MAKER(LongDouble, Real, real, 0.0, 1.0)
NUMBER_TYPE_ENUM_MAKER(Rational, Rational, rational, Rational(), Rational(1, 1))
NUMBER_TYPE_ENUM_MAKER(Char, Char, char, 0, 1)
NUMBER_TYPE_ENUM_MAKER(Octet, Int, int, 0, 1)

Token
BoolTypeEnumMaker::nextEnumItem(const SrcPos& srcpos,
                                const Token& enumItemSymbol,
                                const Token& lastInitToken) const
{
  if (!lastInitToken.isSet()) {
    return Token(srcpos, kBool, false);
  }
  else if (lastInitToken == kBool) {
    return Token(srcpos, kBool, true);
  }

  errorf(srcpos, E_EnumInitTypeMismatch,
         "Init value does not match enum type");
  return Token();
}


#define SIMPLE_TYPE_ENUM_MAKE(_TypeName, _value)                        \
Token                                                                   \
_TypeName ## TypeEnumMaker::nextEnumItem(const SrcPos& srcpos,          \
                                         const Token& enumItemSymbol,   \
                                         const Token& lastInitToken) const \
{                                                                       \
  return Token(srcpos, kSymbol, String(_value));                        \
}

SIMPLE_TYPE_ENUM_MAKE(Eof, "eof")
SIMPLE_TYPE_ENUM_MAKE(Nil, "nil")
SIMPLE_TYPE_ENUM_MAKE(Unspecified, "unspecified")


#define STRING_TYPE_ENUM_MAKE(_TypeName, _kind)                         \
Token                                                                   \
_TypeName ## TypeEnumMaker::nextEnumItem(const SrcPos& srcpos,          \
                                         const Token& enumItemSymbol,   \
                                         const Token& lastInitToken) const \
{                                                                       \
  if (!lastInitToken.isSet() || lastInitToken == k ## _kind) {          \
    return Token(srcpos, k ## _kind, enumItemSymbol.idValue());         \
  }                                                                     \
                                                                        \
  errorf(srcpos, E_EnumInitTypeMismatch,                                \
         "Init value does not match enum type");                        \
  return Token();                                                       \
}


STRING_TYPE_ENUM_MAKE(String, String);
STRING_TYPE_ENUM_MAKE(Keyword, Keyword);
