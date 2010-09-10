/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "str.h"
#include "refcountable.h"
#include "exception.h"
#include "unittests.h"

#if defined(UNITTESTS)
#  include <iostream>
#endif


using namespace heather;

static int str_cmp(const Char* s1, int len1, const Char* s2, int len2);
static int str_ncmp(const Char* s1, int len1, const Char* s2, int len2,
                    int maxItems);
static int str_chr(const Char* src, int len, Char c);
static int str_rchr(const Char* src, int len, Char c);
static int str_str(const Char* haystack, int hslen,
                   const Char* needle, int nlen);
static int str_rstr(const Char* haystack, int hslen, int ofs,
                    const Char* needle, int nlen);


namespace heather
{
  class StringImpl : public RefCountable
  {
  public:
    StringImpl()
      : fData(NULL),
        fLength(0)
    { }

    ~StringImpl()
    {
      if (fData != NULL)
      {
        ::free(fData);
        fData = NULL;
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
        fData = NULL;
        fLength = 0;
      }
    }


    Char* dataFromEnd(int offset)
    {
      assert(offset >= 0 && offset <= fLength);
      return &fData[fLength - offset];
    }


    const Char* data(int offset) const
    {
      assert(offset >= 0 && offset < fLength);
      return &fData[offset];
    }


    Char* data(int offset)
    {
      assert(offset >= 0 && offset < fLength);
      return &fData[offset];
    }


    void createFromUtf8(const char* utf8, int items)
    {
      int reqlen = str_utf8_to_wcs(utf8, items, NULL, 0);

      reallocate(reqlen);
      int reallen = str_utf8_to_wcs(utf8, items, fData, reqlen);
      assert(reallen == reqlen);

      fLength = reallen;
    }


    void copyFromWcs(int offset, const Char* wcs, int items)
    {
      assert(offset + items <= fLength);

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

  String operator+(const String& one, const char* two)
  {
    return one + String(two);
  }


  String operator+(const String& one, int value)
  {
    char tmp[32];
    sprintf(tmp, "%d", value);
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
  : fImpl(NULL)
{
  other.fImpl->incRef();
  fImpl = other.fImpl;
}


String::String(const char* utf8)
  : fImpl(new StringImpl)
{
  fImpl->incRef();
  fImpl->createFromUtf8(utf8, ::strlen(utf8));
}


String::String(const char* utf8, int items)
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
String::startsWith(const String& needle, int offset) const
{
  return compare(needle, needle.length()) == 0;
}


bool
String::endsWith(const String& needle, int offset) const
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
  assert(atIndex >= 0 && atIndex < fImpl->fLength);
  return fImpl->fData[atIndex];
}


int
String::toUtf8(char* dst, int maxItems) const
{
  if (dst == NULL)
    return str_wcs_to_utf8(fImpl->fData, fImpl->fLength, NULL, maxItems);

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
  char *endptr = NULL;

  toUtf8(tmp, 128);

  int val = strtol(tmp, &endptr, radix);
  if (endptr != NULL && strlen(endptr) > 0)
    throw NotANumberException(String("Is not a number: ") + tmp);
  return val;
}


double
String::toDouble() const
{
  char tmp[128];
  char *endptr = NULL;

  toUtf8(tmp, 128);

  double val = strtod(tmp, &endptr);
  if (endptr != NULL && strlen(endptr) > 0)
    throw NotANumberException(String("Is not a number: ") + tmp);
  return val;
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
heather::str_utf8_to_wcs(const char* src, int items, Char* dst, int maxItems)
{
  Octet* sp = (Octet*)src;
  const Octet* end = (Octet*)src + items;

  if (dst != NULL) {
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
      int c = *sp++;

      if (!(c & 0x80)) {
        len++;
      }
      else {
        if ((c & 0xE0) == 0xC0) {
          len++;
          if (sp < end)
            sp++;
        }
        else if ((c & 0xF0) == 0xE0) {
          len += 2;
          sp += 2;
        }
        else if ((c & 0xF8) == 0xF0) {
          len += 3;
          sp += 3;
        }
        else if ((c & 0xFC) == 0xF8) {
          len += 4;
          sp += 4;
        }
        else if ((c & 0xFE) == 0xFC) {
          len += 5;
          sp += 5;
        }
      }
    }

    return len;
  }
}


int
heather::str_wcs_to_utf8(const Char* src, int items, Octet* dst, int maxItems)
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
heather::xmlEncode(const String& str)
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
heather::xmlEncode(const char* str)
{
  return xmlEncode(String(str));
}


#if defined(UNITTESTS)

#include <UnitTest++.h>

std::ostream& heather::operator<<(std::ostream& os, const String& str)
{
  os << "\"" << (const char*)StrHelper(str) << "\"";
  return os;
}


std::ostream& heather::operator<<(std::ostream& os, char c)
{
  os << "'" << c << "'";
  return os;
}


std::ostream& heather::operator<<(std::ostream& os, const char* str0)
{
  os << String(str0);
  return os;
}



//----------------------------------------------------------------------------

SUITE(String)
{
  TEST(Simple)
  {
    CHECK_EQUAL(String().length(), 0);
    CHECK_EQUAL(String("").length(), 0);
    CHECK_EQUAL(String("hello, world!").length(), 13);
  }

  TEST(Compare)
  {
    CHECK_EQUAL(String("abc"), String("abc"));
    CHECK_EQUAL(String(""), String(""));
    CHECK(String("") != String("abc"));
    CHECK(String("abc") < String("xyz"));
    CHECK(String("abc") <= String("abc"));
    CHECK(String("abc") <= String("xyz"));
    CHECK(String("abc") != String("xyz"));
    CHECK(String("xyz") > String("abc"));
    CHECK(String("xyz") >= String("abc"));
    CHECK(String("xyz") >= String("xyz"));
    CHECK(String("abc") < String("xyz!"));
    CHECK(String("xyz!") > String("abc"));
  }

  TEST(FindParts)
  {
    String t("hello, world!");

    CHECK(t.startsWith(String("hello")));
    CHECK(t.startsWith(String()));

    CHECK(t.endsWith(String("world!")));
    CHECK(t.endsWith(String()));

    CHECK(String("a").endsWith(String("a")));
    CHECK(!String().endsWith(String("a")));
    CHECK(String("a").endsWith(String()));

    CHECK_EQUAL(t[6], ' ');
    CHECK_EQUAL(t[0], 'h');
    CHECK_EQUAL(t[12], '!');
    CHECK_EQUAL(t[t.length() - 1], '!');

    CHECK_EQUAL(t.indexOf('h'), 0);
    CHECK_EQUAL(t.indexOf('!'), 12);
    CHECK_EQUAL(t.indexOf('!', 12), 12);
    CHECK_EQUAL(t.indexOf('x'), -1);

    CHECK_EQUAL(t.indexOf('o'), 4);
    CHECK_EQUAL(t.indexOf('o', 4), 4);
    CHECK_EQUAL(t.indexOf('o', 5), 8);
    CHECK_EQUAL(t.indexOf('o', 9), -1);

    CHECK_EQUAL(t.indexOf(String("hello")), 0);
    CHECK_EQUAL(t.indexOf(String("hello"), 1), -1);

    CHECK_EQUAL(t.indexOf(String("world")), 7);

    CHECK_EQUAL(t.lastIndexOf('!'), 12);
    CHECK_EQUAL(t.lastIndexOf('h'), 0);
    CHECK_EQUAL(t.lastIndexOf('x'), -1);

    CHECK_EQUAL(t.lastIndexOf('!', 13), 12);
    CHECK_EQUAL(t.lastIndexOf('!', 12), 12);
    CHECK_EQUAL(t.lastIndexOf('h', 7), 0);
    CHECK_EQUAL(t.lastIndexOf('x', 11), -1);

    CHECK_EQUAL(t.lastIndexOf(String("world")), 7);
    CHECK_EQUAL(t.lastIndexOf(String("world"), 13), 7);
    CHECK_EQUAL(t.lastIndexOf(String("world"), 12), 7);
    CHECK_EQUAL(t.lastIndexOf(String("world"), 11), -1);
    CHECK_EQUAL(t.lastIndexOf(String("world"), 6), -1);
    CHECK_EQUAL(t.lastIndexOf(String("hello"), 6), 0);
    CHECK_EQUAL(t.lastIndexOf(String("hello"), 0), -1);

    CHECK_EQUAL(t.lastIndexOf(String()), 13);
    CHECK_EQUAL(t.lastIndexOf(String(), 10), 10);
  }

  TEST(AppendStrAndNumber)
  {
    CHECK_EQUAL(String("hello") +
                String(", ") + String("world!"), String("hello, world!"));
    CHECK_EQUAL(String() + "hello" + ", " + "world!", String("hello, world!"));
    CHECK_EQUAL(String("midi: ") + 128, String("midi: 128"));
    CHECK_EQUAL(String("midi: ") + fromInt(128), String("midi: 128"));
    CHECK_EQUAL(String("midi?: ") + fromBool(true), String("midi?: true"));
  }


  TEST(AppendChar)
  {
    String t = String("hello") + Char(',') + ' ' + String("world") + Char('!');
    char tmp[256];
    t.toUtf8(tmp, 256);
    CHECK_EQUAL(t, String("hello, world!"));
  }

  TEST(ConvertToUtf8)
  {
    String t("hello, world!");
    char tmp[128];
    CHECK_EQUAL(t.toUtf8(NULL, 128), 13);
    CHECK_EQUAL(t.toUtf8(tmp, 128), 13);
    CHECK_EQUAL(tmp[13], '\0');
    CHECK_EQUAL(strcmp(tmp, "hello, world!"), 0);
  }

  TEST(ConvertToUtf8Limit6)
  {
    String t("hello, world!");
    char tmp[128];
    CHECK_EQUAL(t.toUtf8(NULL, 6), 5);
    CHECK_EQUAL(t.toUtf8(tmp, 6), 5);
    CHECK_EQUAL(tmp[5], '\0');
    CHECK_EQUAL(strcmp(tmp, "hello"), 0);
  }

  TEST(ConvertEmptyStrToUtf8)
  {
    String t;
    char tmp[128];
    CHECK_EQUAL(t.toUtf8(NULL, 128), 0);
    CHECK_EQUAL(t.toUtf8(tmp, 128), 0);
    CHECK_EQUAL(tmp[0], '\0');
    CHECK_EQUAL(strcmp(tmp, ""), 0);
  }

  TEST(Split)
  {
    String t("hello, world!");
    String before, after;

    CHECK_EQUAL(t.split(',', before, after), 5);
    CHECK_EQUAL(before, String("hello"));
    CHECK_EQUAL(after, String(" world!"));

    CHECK_EQUAL(t.split('h', before, after), 0);
    CHECK_EQUAL(before, String());
    CHECK_EQUAL(after, String("ello, world!"));

    CHECK_EQUAL(t.split('!', before, after), 12);
    CHECK_EQUAL(before, String("hello, world"));
    CHECK_EQUAL(after, String());

    CHECK_EQUAL(t.split('x', before, after), -1);

    CHECK_EQUAL(t.split(String(", "), before, after), 5);
    CHECK_EQUAL(before, String("hello"));
    CHECK_EQUAL(after, String("world!"));

    CHECK_EQUAL(t.split(String("hello"), before, after), 0);
    CHECK_EQUAL(before, String());
    CHECK_EQUAL(after, String(", world!"));

    CHECK_EQUAL(t.split(String("world"), before, after), 7);
    CHECK_EQUAL(before, String("hello, "));
    CHECK_EQUAL(after, String("!"));

    CHECK_EQUAL(t.split(String("world!"), before, after), 7);
    CHECK_EQUAL(before, String("hello, "));
    CHECK_EQUAL(after, String());

    CHECK_EQUAL(t.split(String("hello, world!"), before, after), 0);
    CHECK_EQUAL(before, String());
    CHECK_EQUAL(after, String());

    CHECK_EQUAL(t.split(String("o"), before, after), 4);
    CHECK_EQUAL(before, String("hell"));
    CHECK_EQUAL(after, String(", world!"));

    CHECK_EQUAL(String().split(String(), before, after), 0);
    CHECK_EQUAL(before, String());
    CHECK_EQUAL(after, String());

    CHECK_EQUAL(t.part(0, t.length()), t);
    CHECK_EQUAL(t.part(0, 1), String("h"));
    CHECK_EQUAL(t.part(0, 5), String("hello"));
    CHECK_EQUAL(t.part(7, 12), String("world"));
    CHECK_EQUAL(t.part(13, 20), String());
  }

  TEST(ConvertToInt)
  {
    CHECK_EQUAL(String("123456").toInt(), 123456);
    CHECK_EQUAL(String("123456").toInt(16), 0x123456);
    CHECK_EQUAL(String("123456").toInt(8), 0123456);
    CHECK_EQUAL(String("0").toInt(), 0);
  }

  TEST(ConvertToDouble)
  {
    CHECK_EQUAL(String("3.1415").toDouble(), 3.1415);
  }
};


#endif  // #if defined(UNITTESTS)
