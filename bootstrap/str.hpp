/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include <memory>
#include <string>
#include <vector>

#if defined(UNITTESTS)
#  include <iostream>
#endif


//----------------------------------------------------------------------------

namespace herschel {
class StringImpl;

//--------------------------------------------------------------------------

//! A unicode (wide character) enabled string implementation
//!
//! Instances can be created from plain C strings and std::string, which are
//! expected to be utf8 encoded 8bit strings.
class String {
public:
  //! Constructs an empty string
  String();

  //! Copy constructor
  String(const String& other);

  //! Create a new instance from the null terminated utf8 encoded plain
  //! string \p utf8.  After construction the caller is free to deallocate
  //! \p utf8.
  explicit String(zstring utf8);

  explicit String(const std::string& utf8);

  //! Create a new instance from the first \p items characters of the utf8
  //! encoded plain string \p utf8.  When a null byte is found in \p utf8
  //! before \p items have reached only the part of \p utf8 until the null
  //! byte is copied.  After construction the caller is free to deallocate
  //! \p utf8.
  explicit String(zstring utf8, int items);

  //! Create a new instance from the first \p items characters of the wide
  //! character arrays \p str.  When a null character is found in \p str
  //! before \p items have reached only the part of \p utf8 until the null
  //! character is copied.  After construction the caller is free to
  //! deallocate \p str.
  String(const Char* str, int items);

  //! Assign operator
  String& operator=(const String& other);

  //! Compare the first \p maxItems characters from \p other to \c this.
  //! The return value is like that of the standard C library \c strcmp
  //! function, i.e. it returns a negative value when \c this is less than
  //! \p other, a positive value when \c this is greater than \p other, and
  //! 0 if the compared parts of \c this and \p other are identical.  If \p
  //! maxItems is negative \c this and \p other are compared completely.
  int compare(const String& other, int maxItems = -1) const;

  bool operator<(const String& other) const;
  bool operator<=(const String& other) const;
  bool operator>(const String& other) const;
  bool operator>=(const String& other) const;
  bool operator==(const String& other) const;
  bool operator!=(const String& other) const;

  //! Returns the length of the string in characters, excl. a propably
  //! terminating null.
  int length() const;

  //! Indicates whether the string is empty, i.e. has a length of 0.
  bool isEmpty() const;


  //! Indicates whether the string starts with a substring \p needle
  bool startsWith(const String& needle) const;

  //! Indicates whether the string ends with a substring \p needle.
  bool endsWith(const String& needle) const;

  //! Returns the character index of the first occurance of the substring \p
  //! needle in the string, starting from \p offset.  If \p needle is not
  //! contained in the string at all it returns a negative value.
  int indexOf(const String& needle, int offset = 0) const;
  //! Returns the character index of the first occurance of the character \p
  //! c in the string, starting from \p offset.  If \p needle is not
  //! contained in the string at all it returns a negative value.
  int indexOf(Char c, int offset = 0) const;
  //! Returns the character index of the last occurance of the substring \p
  //! needle in the string, starting from \p offset.  If \p offset is -1 the
  //! search starts at the end of the string.  If \p needle is not
  //! contained in the string at all it returns a negative value.
  int lastIndexOf(const String& needle, int offset = -1) const;
  //! Returns the character index of the last occurance of the character \p
  //! needle in the string, starting from \p offset.  If \p offset is -1 the
  //! search starts at the end of the string.  If \p needle is not contained
  //! in the string at all it returns a negative value.
  int lastIndexOf(Char c, int offset = -1) const;

  //! Creates a new instance from the string with \p second appending to its
  //! end.
  String operator+(const String& second) const;

  //! Creates a new instance from the string with \p c appending to its end.
  String operator+(Char c) const;
  //! Creates a new instance from the string with \p c appending to its end.
  String operator+(char c) const;

  //! Returns the character at char index \p atIndex.
  Char operator[](int atIndex) const;

  //! Transforms the receiver to a null terminated utf8 encoded string into
  //! \p dst and returns length of generated octets without terminating 0.
  //! If \p dst is nullptr returns only the required length of octets
  //! (without terminating 0).  \p dst is maxItems octets large.  At maximum
  //! maxItems - 1 octets (without terminatin 0) are copied.
  int toUtf8(char* dst, int maxItems) const;

  //! Splits the receiver at the first occurance of \p c and puts the
  //! parts before and after into \p before and \p after.  Returns the
  //! index where \p c has been found or -1.
  int split(Char c, String& before, String& after) const;

  //! Splits the receiver at the first occurance of the substring \p needle
  //! and puts the parts before and after into \p before and \p after.
  //! Returns the index where \p c has been found or -1.  Note that \p
  //! needle is neither part of \p before nor \p after.
  int split(const String& needle, String& before, String& after) const;

  //! Returns a substring from [\p from, \p to).
  String part(int from, int to) const;

  //! Treats the receiver as a string representation of an integer number
  //! and converts it into the numerical form using the base \p radix.  No
  //! other characters than the number characters are accepted.
  //!
  //! If the string is found not to be a number representation it throws a
  //! \c NotANumberException exception.
  int toInt(int radix = 10) const;

  //! Treats the receiver as a string representation of a double number and
  //! converts it into the numerical form.  No other characters than the
  //! number characters are accepted.
  //!
  //! If the string is found not to be a number representation it throws a
  //! \c NotANumberException exception.
  double toDouble() const;

  //! Treats the receiver as a string representation of an integer number
  //! and converts it into the numerical form using the base \p radix.  No
  //! other characters than the number characters are accepted.
  //!
  //! If the string is found not to be a number representation it throws a
  //! \c NotANumberException exception.
  int64_t toInt64(int radix = 10) const;

  //! Treats the receiver as a string representation of an integer number
  //! and converts it into the numerical form using the base \p radix.  No
  //! other characters than the number characters are accepted.
  //!
  //! If the string is found not to be a number representation it throws a
  //! \c NotANumberException exception.
  uint64_t toUInt64(int radix = 10) const;

  //! Returns a \c std::string representation of the receiver.  The value is
  //! encoded as utf8.
  operator std::string() const;

private:
  friend class StringBuffer;

  const Char* data() const;

  //-------- data member
  std::shared_ptr<StringImpl> fImpl;
};

//! Concatenates \p one and \p two into a new instance.
String operator+(const String& one, zstring two);
//! Appends a string representation of \p value to \p one and returns it as
//! new instance.
String operator+(const String& one, int value);
//! Appends a string representation of \p value to \p one and returns it as
//! new instance.
String operator+(const String& one, int64_t value);
//! Appends a string representation of \p value to \p one and returns it as
//! new instance.
String operator+(const String& one, double value);

//! Returns a string representation of \p value.
String fromInt(int value);
//! Returns a string representation of \p value.
String fromFloat(float value);
//! Returns a string representation of \p value.
String fromDouble(double value);
//! Returns a string representation of \p value.  The return value is either
//! \c "true" or \c "false".
String fromBool(bool value);


int str_utf8_to_wcs(zstring src, int items, Char* dst, int maxItems);
int str_wcs_to_utf8(const Char* src, int items, Octet* dst, int maxItems);

//! Returns a new symbol, which is unique throughout the application's life
//! time.
String uniqueName(const char* prefix);


//! Utility helping to encode String instances to utf8 encoded plain C char
//! strings

class StrHelper {
public:
  //! Construct an instance from a given String instance
  StrHelper(const String& str)
  {
    int reqLen = str.toUtf8(nullptr, str.length() * 5);
    fBuffer.resize(reqLen + 1);
    fLength = str.toUtf8(&fBuffer[0], reqLen + 1);
  }

  //! Return the length of the encoded utf8 plain C string without
  //! terminating 0 byte in bytes.
  int length() const { return fLength; }

  //! Returns the encoded C string.
  operator const char*() const { return &fBuffer[0]; }

  //! Returns the encoded C string.
  zstring c_str() const { return &fBuffer[0]; }

private:
  std::vector<char> fBuffer;
  int fLength;
};


//! Encode a given String by escaping special XML characters like &, <, and '
String xmlEncode(const String& str);

//! Encode a given plain C string by escaping special XML characters like &,
//! <, and '
String xmlEncode(zstring str);

#if defined(UNITTESTS)
std::ostream& operator<<(std::ostream& os, const String& str);
std::ostream& operator<<(std::ostream& os, char c);
#endif

}  // namespace herschel
