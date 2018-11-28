/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


#include "typeenum.hpp"

#include "errcodes.hpp"
#include "log.hpp"
#include "srcpos.hpp"
#include "type.hpp"


namespace herschel {


#define NUMBER_TYPE_ENUM_MAKER(_TypeName, _kind, _method, _init, _step)            \
  Token _TypeName##TypeEnumMaker::nextEnumItem(const SrcPos& srcpos,               \
                                               const Token& enumItemSymbol,        \
                                               const Token& lastInitToken) const   \
  {                                                                                \
    if (!lastInitToken.isSet()) {                                                  \
      return Token(srcpos, k##_kind, _init);                                       \
    }                                                                              \
    else if (lastInitToken == k##_kind) {                                          \
      return Token(srcpos, k##_kind, lastInitToken._method##Value() + _step);      \
    }                                                                              \
                                                                                   \
    errorf(srcpos, E_EnumInitTypeMismatch, "Init value does not match enum type"); \
    return Token();                                                                \
  }


#define NUMBER_TYPE_ENUM_MAKER_INT(_TypeName, _ctor, _bitwidth)                    \
  Token _TypeName##TypeEnumMaker::nextEnumItem(const SrcPos& srcpos,               \
                                               const Token& enumItemSymbol,        \
                                               const Token& lastInitToken) const   \
  {                                                                                \
    if (!lastInitToken.isSet()) {                                                  \
      return Token::new##_ctor(srcpos, _bitwidth, 0);                              \
    }                                                                              \
    else if (lastInitToken == kInt) {                                              \
      return Token::new##_ctor(srcpos, _bitwidth, lastInitToken.intValue() + 1);   \
    }                                                                              \
                                                                                   \
    errorf(srcpos, E_EnumInitTypeMismatch, "Init value does not match enum type"); \
    return Token();                                                                \
  }


//                     _TypeName,  _kind,    _method, _init, _step
NUMBER_TYPE_ENUM_MAKER(Float32, Float, float, 0.0, 1.0)
NUMBER_TYPE_ENUM_MAKER(Float64, Float, float, 0.0, 1.0)
NUMBER_TYPE_ENUM_MAKER(Float128, Float, float, 0.0, 1.0)
NUMBER_TYPE_ENUM_MAKER(Rational, Rational, rational, Rational(), Rational(1, 1))
NUMBER_TYPE_ENUM_MAKER(Char, Char, char, 0, 1)

//                     _TypeName,  _bitwidth
NUMBER_TYPE_ENUM_MAKER_INT(Int8, Int, 8)
NUMBER_TYPE_ENUM_MAKER_INT(Int16, Int, 16)
NUMBER_TYPE_ENUM_MAKER_INT(Int32, Int, 32)
NUMBER_TYPE_ENUM_MAKER_INT(Int64, Int, 64)

NUMBER_TYPE_ENUM_MAKER_INT(UInt8, UInt, 8)
NUMBER_TYPE_ENUM_MAKER_INT(UInt16, UInt, 16)
NUMBER_TYPE_ENUM_MAKER_INT(UInt32, UInt, 32)
NUMBER_TYPE_ENUM_MAKER_INT(UInt64, UInt, 64)


Token BoolTypeEnumMaker::nextEnumItem(const SrcPos& srcpos, const Token& enumItemSymbol,
                                      const Token& lastInitToken) const
{
  if (!lastInitToken.isSet()) {
    return Token(srcpos, kBool, false);
  }
  else if (lastInitToken == kBool) {
    return Token(srcpos, kBool, true);
  }

  errorf(srcpos, E_EnumInitTypeMismatch, "Init value does not match enum type");
  return Token();
}


#define SIMPLE_TYPE_ENUM_MAKE(_TypeName, _value)                                 \
  Token _TypeName##TypeEnumMaker::nextEnumItem(const SrcPos& srcpos,             \
                                               const Token& enumItemSymbol,      \
                                               const Token& lastInitToken) const \
  {                                                                              \
    return Token(srcpos, kSymbol, String(_value));                               \
  }

SIMPLE_TYPE_ENUM_MAKE(Eof, "lang|eof")
SIMPLE_TYPE_ENUM_MAKE(Nil, "lang|nil")
SIMPLE_TYPE_ENUM_MAKE(Unspecified, "lang|unspecified")


#define STRING_TYPE_ENUM_MAKE(_TypeName, _kind)                                    \
  Token _TypeName##TypeEnumMaker::nextEnumItem(const SrcPos& srcpos,               \
                                               const Token& enumItemSymbol,        \
                                               const Token& lastInitToken) const   \
  {                                                                                \
    if (!lastInitToken.isSet() || lastInitToken == k##_kind) {                     \
      return Token(srcpos, k##_kind, enumItemSymbol.idValue());                    \
    }                                                                              \
                                                                                   \
    errorf(srcpos, E_EnumInitTypeMismatch, "Init value does not match enum type"); \
    return Token();                                                                \
  }


STRING_TYPE_ENUM_MAKE(String, String);
STRING_TYPE_ENUM_MAKE(Keyword, Keyword);

}  // namespace herschel
