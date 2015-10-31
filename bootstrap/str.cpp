/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "common.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "require.h"
#include "log.h"
#include "str.h"
#include "strbuf.h"
#include "refcountable.h"
#include "exception.h"

#include <string>


using namespace herschel;

static int str_cmp(const Char* s1, int len1, const Char* s2, int len2);
static int str_ncmp(const Char* s1, int len1, const Char* s2, int len2,
                    int maxItems);
static int str_chr(const Char* src, int len, Char c);
static int str_rchr(const Char* src, int len, Char c);
static int str_str(const Char* haystack, int hslen,
                   const Char* needle, int nlen);
static int str_rstr(const Char* haystack, int hslen, int ofs,
                    const Char* needle, int nlen);


namespace herschel
{
  class StringImpl : public RefCountable
  {
  public:
    StringImpl()
      : fData(nullptr),
        fLength(0)
    { }

    ~StringImpl()
    {
      if (fData)
      {
        ::free(fData);
        fData = nullptr;
      }
    }

    void reallocate(int size)
    {
      if (size > 0)
      {
        fData = (Char*)::realloc(fData, size * sizeof(Char));
        fLength = size;
      }
      else
      {
        ::free(fData);
        fData = nullptr;
        fLength = 0;
      }
    }


    Char* dataFromEnd(int offset)
    {
      hr_assert(offset >= 0 && offset <= fLength);
      return &fData[fLength - offset];
    }


    const Char* data(int offset) const
    {
      hr_assert(offset >= 0 && offset < fLength);
      return &fData[offset];
    }


    Char* data(int offset)
    {
      hr_assert(offset >= 0 && offset < fLength);
      return &fData[offset];
    }


    void createFromUtf8(zstring utf8, int items)
    {
      int reqlen = str_utf8_to_wcs(utf8, items, nullptr, 0);

      reallocate(reqlen);
      int reallen = str_utf8_to_wcs(utf8, items, fData, reqlen);
      hr_assert(reallen == reqlen);

      fLength = reallen;
    }


    void copyFromWcs(int offset, const Char* wcs, int items)
    {
      hr_assert(offset + items <= fLength);

      Char* dp = &fData[offset];
      const Char* sp = wcs;
      for (int i = 0; i < items; i++, dp++, sp++)
        *dp = *sp;
    }

    //-------- data members
    Char* fData;
    int fLength;
  };


  //--------------------------------------------------------------------------

  String operator+(const String& one, zstring two)
  {
    return one + String(two);
  }


  String operator+(const String& one, int value)
  {
    char tmp[32];
    sprintf(tmp, "%d", value);
    return one + tmp;
  }


  String operator+(const String& one, int64_t value)
  {
    char tmp[64];
    sprintf(tmp, "%lld", value);
    return one + tmp;
  }


  String operator+(const String& one, double value)
  {
    char tmp[32];
    sprintf(tmp, "%lf", value);
    return one + tmp;
  }


  String fromInt(int value)
  {
    char tmp[32];
    sprintf(tmp, "%d", value);
    return String(tmp);
  }


  String fromFloat(float value)
  {
    char tmp[32];
    sprintf(tmp, "%f", value);
    return String(tmp);
  }


  String fromDouble(double value)
  {
    char tmp[32];
    sprintf(tmp, "%lf", value);
    return String(tmp);
  }


  String fromBool(bool value)
  {
    return value ? String("true") : String("false");
  }
};


//----------------------------------------------------------------------------

String::String()
  : fImpl(new StringImpl)
{
  fImpl->incRef();
}


String::String(const String& other)
  : fImpl(nullptr)
{
  other.fImpl->incRef();
  fImpl = other.fImpl;
}


String::String(zstring utf8)
  : fImpl(new StringImpl)
{
  fImpl->incRef();
  fImpl->createFromUtf8(utf8, ::strlen(utf8));
}


String::String(const std::string& utf8)
  : fImpl(new StringImpl)
{
  fImpl->incRef();
  fImpl->createFromUtf8(utf8.c_str(), utf8.length());
}


String::String(zstring utf8, int items)
  : fImpl(new StringImpl)
{
  fImpl->incRef();
  fImpl->createFromUtf8(utf8, items);
}


String::String(const Char* str, int items)
  : fImpl(new StringImpl)
{
  fImpl->incRef();
  if (items > 0) {
    fImpl->reallocate(items);
    fImpl->copyFromWcs(0, str, items);
  }
}


String::~String()
{
  fImpl->decRef();
}


const Char*
String::data() const
{
  return fImpl->data(0);
}


String&
String::operator=(const String& other)
{
  other.fImpl->incRef();
  fImpl->decRef();
  fImpl = other.fImpl;
  return *this;
}


int
String::compare(const String& other, int maxItems) const
{
  if (maxItems >= 0)
    return str_ncmp(fImpl->fData, fImpl->fLength,
                    other.fImpl->fData, other.fImpl->fLength,
                    maxItems);
  else
    return str_cmp(fImpl->fData, fImpl->fLength,
                   other.fImpl->fData, other.fImpl->fLength);
}


bool
String::operator<(const String& other) const
{
  return compare(other) < 0;
}


bool
String::operator<=(const String& other) const
{
  return compare(other) <= 0;
}


bool
String::operator>(const String& other) const
{
  return compare(other) > 0;
}


bool
String::operator>=(const String& other) const
{
  return compare(other) >= 0;
}


bool
String::operator==(const String& other) const
{
  return compare(other) == 0;
}


bool
String::operator!=(const String& other) const
{
  return compare(other) != 0;
}


int
String::length() const
{
  return fImpl->fLength;
}


bool
String::isEmpty() const
{
  return fImpl->fLength == 0;
}


bool
String::startsWith(const String& needle) const
{
  return compare(needle, needle.length()) == 0;
}


bool
String::endsWith(const String& needle) const
{
  if (needle.length() <= length()) {
    return str_cmp(fImpl->dataFromEnd(needle.fImpl->fLength),
                   needle.fImpl->fLength,
                   needle.fImpl->fData, needle.fImpl->fLength) == 0;
  }
  else
    return false;
}


int
String::indexOf(const String& needle, int offset) const
{
  if (needle.fImpl->fLength == 0)
    return offset;

  int idx = str_str(fImpl->data(offset), fImpl->fLength - offset,
                    needle.fImpl->fData, needle.fImpl->fLength);
  return idx >= 0 ? idx + offset : -1;
}


int
String::indexOf(Char c, int offset) const
{
  int idx = str_chr(fImpl->data(offset), fImpl->fLength - offset, c);
  return idx >= 0 ? idx + offset : -1;
}


int
String::lastIndexOf(const String& needle, int offset) const
{
  int ofs = offset < 0 ? fImpl->fLength : offset;

  if (ofs >= needle.fImpl->fLength) {
    int rofs = ofs - needle.fImpl->fLength;
    int idx = str_rstr(fImpl->fData, fImpl->fLength, rofs,
                       needle.fImpl->fData, needle.fImpl->fLength);
    return idx >= 0 ? idx : -1;
  }
  return -1;
}


int
String::lastIndexOf(Char c, int offset) const
{
  int ofs = offset < 0 ? fImpl->fLength - 1 : offset;

  int idx = str_rchr(fImpl->fData, ofs, c);
  return idx >= 0 ? idx : -1;
}


String
String::operator+(const String &second) const
{
  String tmp;
  tmp.fImpl->reallocate(fImpl->fLength + second.fImpl->fLength);
  tmp.fImpl->copyFromWcs(0, fImpl->fData, fImpl->fLength);
  tmp.fImpl->copyFromWcs(fImpl->fLength,
                         second.fImpl->fData, second.fImpl->fLength);
  return tmp;
}


String
String::operator+(Char c) const
{
  String tmp;
  tmp.fImpl->reallocate(fImpl->fLength + 1);
  tmp.fImpl->copyFromWcs(0, fImpl->fData, fImpl->fLength);
  tmp.fImpl->fData[fImpl->fLength] = c;
  return tmp;
}


String
String::operator+(char c) const
{
  return operator+(Char(c));
}


Char
String::operator[] (int atIndex) const
{
  hr_assert(atIndex >= 0 && atIndex < fImpl->fLength);
  return fImpl->fData[atIndex];
}


int
String::toUtf8(char* dst, int maxItems) const
{
  if (!dst)
    return str_wcs_to_utf8(fImpl->fData, fImpl->fLength, nullptr, maxItems);

  int len = str_wcs_to_utf8(fImpl->fData, fImpl->fLength,
                            (Octet*)dst, maxItems);
  dst[len] = '\0';
  return len;
}


int
String::split(Char c, String& before, String& after) const
{
  int idx = indexOf(c);
  if (idx >= 0) {
    before = String(fImpl->fData, idx);
    after = String(fImpl->fData + idx + 1, fImpl->fLength - (idx + 1));

    return idx;
  }

  return -1;
}


int
String::split(const String& needle, String& before, String& after) const
{
  int idx = indexOf(needle);
  if (idx >= 0) {
    before = String(fImpl->fData, idx);
    after = String(fImpl->fData + idx + needle.fImpl->fLength,
                   fImpl->fLength - (idx + needle.fImpl->fLength));

    return idx;
  }

  return -1;
}


String
String::part(int from, int to) const
{
  if (from == 0 && to >= fImpl->fLength)
    return *this;

  if (from >= 0 && from < to && from < fImpl->fLength) {
    int items = (to < fImpl->fLength ? to : fImpl->fLength) - from;
    return String(fImpl->fData + from, items);
  }
  return String();
}


int
String::toInt(int radix) const
{
  char tmp[128];
  char *endptr = nullptr;

  toUtf8(tmp, 128);

  errno = 0;

  int val = strtol(tmp, &endptr, radix);
  if (endptr && strlen(endptr) > 0)
    throw NotANumberException(String("Is not a number: ") + tmp);

  if (errno == ERANGE) {
    logf(kWarn, "Number to large: %s", tmp);
  }

  return val;
}


int64_t
String::toInt64(int radix) const
{
  char tmp[128];
  char *endptr = nullptr;

  toUtf8(tmp, 128);

  errno = 0;

  int64_t val = strtoll(tmp, &endptr, radix);
  if (endptr && strlen(endptr) > 0)
    throw NotANumberException(String("Is not a number: ") + tmp);

  if (errno == ERANGE) {
    logf(kWarn, "Number to large: %s", tmp);
  }

  return val;
}


uint64_t
String::toUInt64(int radix) const
{
  char tmp[128];
  char *endptr = nullptr;

  toUtf8(tmp, 128);

  errno = 0;

  uint64_t val = strtoull(tmp, &endptr, radix);
  if (endptr && strlen(endptr) > 0)
    throw NotANumberException(String("Is not a number: ") + tmp);

  if (errno == ERANGE) {
    logf(kWarn, "Number to large: %s", tmp);
  }

  return val;
}


double
String::toDouble() const
{
  char tmp[128];
  char *endptr = nullptr;

  toUtf8(tmp, 128);

  double val = strtod(tmp, &endptr);
  if (endptr && strlen(endptr) > 0)
    throw NotANumberException(String("Is not a number: ") + tmp);
  return val;
}


String::operator std::string() const
{
  return std::string(StrHelper(*this));
}


//----------------------------------------------------------------------------

static int
str_cmp(const Char* s1, int len1, const Char* s2, int len2)
{
  if (len1 == len2) {
    for (int i = 0; i < len1; i++, s1++, s2++) {
      if (*s1 < *s2)
        return -1;
      else if (*s1 > *s2)
        return 1;
    }
    return 0;
  }
  else if (len1 < len2)
    return -1;
  return 1;
}


static int
str_ncmp(const Char* s1, int len1, const Char* s2, int len2, int maxItems)
{
  if (maxItems == 0)
    return 0;

  int i = 0;
  for ( ; i < len1 && i < len2 && i < maxItems; i++, s1++, s2++) {
    if (*s1 < *s2)
      return -1;
    else if (*s1 > *s2)
      return 1;
  }

  if (i == maxItems)
    // all characters until are equal
    return 0;
  return len1 - len2;
}


static int
str_chr(const Char* src, int len, Char c)
{
  const Char* sp = src;
  for (int i = 0; i < len; i++, sp++) {
    if (*sp == c)
      return i;
  }
  return -1;
}


static int
str_rchr(const Char* src, int len, Char c)
{
  const Char* sp = src + len;
  for (int i = len; i >= 0; i--, sp--) {
    if (*sp == c)
      return i;
  }
  return -1;
}


static int
str_str(const Char* haystack, int hslen, const Char* needle, int nlen)
{
  /* Check for the null needle case.  */
  if (nlen == 0)
    return 0;

  for (int hsofs = 0;
       (hsofs = str_chr(haystack + hsofs, hslen - hsofs, *needle)) >= 0;
       hsofs++)
  {
    if (str_ncmp(haystack + hsofs, hslen + hsofs, needle, nlen, nlen) == 0)
      return hsofs;
  }
  return -1;
}


static int
str_rstr(const Char* haystack, int hslen, int ofs, const Char* needle, int nlen)
{
  if (nlen == 0)
    return ofs;

  const Char* sp = haystack + ofs;
  for (int i = ofs; i >= 0; i--, sp--) {
    if (*sp == *needle) {
      if (str_str(sp, hslen - i, needle, nlen) == 0)
        return i;
    }
  }

  return -1;
}


int
herschel::str_utf8_to_wcs(zstring src, int items, Char* dst, int maxItems)
{
  Octet* sp = (Octet*)src;
  const Octet* end = (Octet*)src + items;

  if (dst) {
    Char* dp = dst;

    while (sp < end) {
      int c = *sp++;

      if (!(c & 0x80)) {
        *dp++ = (Char)c;
      }
      else {
        Char uc = '?';

        if ((c & 0xE0) == 0xC0) {
          if (sp < end) {
            uc = (Char)(((c & 0x1F) << 6) | (*sp++ & 0x3F));
          }
        }
        else if ((c & 0xF0) == 0xE0) {
          if (end - sp >= 2) {
            uc = (Char)(((c & 0x0F) << 12)
                        | ((((int) sp[0]) & 0x3F) << 6)
                        | (sp[1] & 0x3F));
          }
          sp += 2;
        }
        else if ((c & 0xF8) == 0xF0)
          sp += 3;
        else if ((c & 0xFC) == 0xF8)
          sp += 4;
        else if ((c & 0xFE) == 0xFC)
          sp += 5;

        *dp++ = uc;
      }
    }

    return dp - dst;
  }
  else {
    int len = 0;

    while (sp < end) {
      int c = *sp;

      if (!(c & 0x80)) {
        len++;
        sp++;
      }
      else {
        if ((c & 0xE0) == 0xC0) {
          len++;
          sp += 2;
        }
        else if ((c & 0xF0) == 0xE0) {
          len++;
          sp += 3;
        }
        else if ((c & 0xF8) == 0xF0) {
          len++;
          sp += 4;
        }
        else if ((c & 0xFC) == 0xF8) {
          len++;
          sp += 5;
        }
        else if ((c & 0xFE) == 0xFC) {
          len++;
          sp += 6;
        }
      }
    }

    return len;
  }
}


int
herschel::str_wcs_to_utf8(const Char* src, int items, Octet* dst, int maxItems)
{
  const Char* sp = src;

  if (dst) {
    int len = 0;
    Octet* dp = dst;
    for (int i = 0; i < items; i++, sp++) {
      if (*sp & 0xff80) {
        if (*sp & 0xf800) {
          len += 3;
          if (len >= maxItems)
            return len - 3;

          *dp++ = Char(0xe0 | (*sp >> 12));
          *dp++ = Char(0x80 | ((*sp >> 6) & 0x3f));
        }
        else {
          len += 2;
          if (len >= maxItems)
            return len - 2;

          *dp++ = (Octet)(0xc0 | ((*sp >> 6) & 0x3f));
        }
        *dp++ = (Octet)(0x80 | (*sp & 0x3f));
      }
      else {
        len++;
        if (len >= maxItems)
          return len - 1;

        *dp++ = (Octet)(*sp);
      }
    }

    return len;
  }
  else {
    int len = 0;
    for (int i = 0; i < items; i++, sp++) {
      if (*sp & 0xff80) {
        if (*sp & 0xf800) {
          len += 3;
          if (len >= maxItems)
            return len - 3;
        }
        else {
          len += 2;
          if (len >= maxItems)
            return len - 2;
        }
      }
      else {
        len++;
        if (len >= maxItems)
          return len - 1;
      }
    }

    return len;
  }
}


String
herschel::xmlEncode(const String& str)
{
  std::vector<Char> buffer;
  buffer.reserve(str.length());

  for (int i = 0; i < str.length(); i++) {
    Char c = str[i];
    if (c == '<') {
      buffer.push_back('&');
      buffer.push_back('l');
      buffer.push_back('t');
      buffer.push_back(';');
    }
    else if (c == '&') {
      buffer.push_back('&');
      buffer.push_back('a');
      buffer.push_back('m');
      buffer.push_back('p');
      buffer.push_back(';');
    }
    else if (c == '\'') {
      buffer.push_back('&');
      buffer.push_back('a');
      buffer.push_back('p');
      buffer.push_back('o');
      buffer.push_back('s');
      buffer.push_back(';');
    }
    else
      buffer.push_back(c);
  }

  return String(&buffer[0], buffer.size());
}


String
herschel::xmlEncode(zstring str)
{
  return xmlEncode(String(str));
}


String
herschel::uniqueName(zstring prefix)
{
  static int counter = 0;
  StringBuffer buffer;
  buffer << "__" << prefix << "_" << fromInt(counter++);
  return buffer.toString();
}


#if defined(UNITTESTS)
std::ostream& herschel::operator<<(std::ostream& os, const String& str)
{
  os << "\"" << (zstring)StrHelper(str) << "\"";
  return os;
}


std::ostream& herschel::operator<<(std::ostream& os, char c)
{
  os << "'" << c << "'";
  return os;
}
#endif
