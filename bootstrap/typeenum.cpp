/* -*-c++-*-

   This file is part of the herschel package

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


using namespace herschel;


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


//                     _TypeName,  _kind,    _method, _init, _step
NUMBER_TYPE_ENUM_MAKER(Int,        Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(Real,       Real,     real,    0.0,   1.0)
NUMBER_TYPE_ENUM_MAKER(Float32,    Real,     real,    0.0,   1.0)
NUMBER_TYPE_ENUM_MAKER(Float64,    Real,     real,    0.0,   1.0)
NUMBER_TYPE_ENUM_MAKER(Float128,   Real,     real,    0.0,   1.0)
NUMBER_TYPE_ENUM_MAKER(Rational,   Rational, rational, Rational(), Rational(1, 1))
NUMBER_TYPE_ENUM_MAKER(Char,       Char,     char,    0,     1)

NUMBER_TYPE_ENUM_MAKER(Int8,       Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(UInt8,      Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(Int16,      Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(UInt16,     Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(Int32,      Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(UInt32,     Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(Int64,      Int,      int,     0,     1)
NUMBER_TYPE_ENUM_MAKER(UInt64,     Int,      int,     0,     1)

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

SIMPLE_TYPE_ENUM_MAKE(Eof, "lang|eof")
SIMPLE_TYPE_ENUM_MAKE(Nil, "lang|nil")
SIMPLE_TYPE_ENUM_MAKE(Unspecified, "lang|unspecified")


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
