/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_str_h
#define bootstrap_str_h

#include "common.h"

#include <vector>
#if defined(UNITTESTS)
#  include <iostream>
#endif


//----------------------------------------------------------------------------

namespace heather
{
  class StringImpl;

  //--------------------------------------------------------------------------

  class String
  {
  public:
    String();
    String(const String& other);
    explicit String(const char* utf8);
    explicit String(const char* utf8, int items);
    String(const Char* str, int items);
    ~String();

    String& operator=(const String& other);

    int compare(const String& other, int maxItems = -1) const;
    bool operator<(const String& other) const;
    bool operator<=(const String& other) const;
    bool operator>(const String& other) const;
    bool operator>=(const String& other) const;
    bool operator==(const String& other) const;
    bool operator!=(const String& other) const;

    int length() const;
    bool isEmpty() const;


    bool startsWith(const String& needle, int offset = 0) const;
    bool endsWith(const String& needle, int offset = 0) const;

    int indexOf(const String& needle, int offset = 0) const;
    int indexOf(Char c, int offset = 0) const;
    int lastIndexOf(const String& needle, int offset = -1) const;
    int lastIndexOf(Char c, int offset = -1) const;

    String operator+(const String &second) const;
    String operator+(Char c) const;
    String operator+(char c) const;
    Char operator[] (int atIndex) const;

    //! Transforms receiver to 0-terminated utf8 into dst and returns
    //! length of generated octets without terminating 0.  If \p dst is
    //! NULL returns only the required length of octets (without
    //! terminating 0).  \p dst is maxItems octets large.  At maximum
    //! maxItems - 1 octets (without terminatin 0) are copied.
    int toUtf8(char* dst, int maxItems) const;

    //! splits the receiver at the first occurance of \p c and puts the
    //! parts before and after into \p before and \p after.  Returns the
    //! index where c has been found or -1.
    int split(Char c, String& before, String& after) const;
    int split(const String& needle, String& before, String& after) const;

    //! returns the substring from [from, to)
    String part(int from, int to) const;

    int toInt(int radix = 10) const;
    double toDouble() const;

  private:
    friend class StringBuffer;

    const Char* data() const;

    //-------- data member
    StringImpl* fImpl;
  };

  String operator+(const String& one, const char* two);
  String operator+(const String& one, int value);
  String operator+(const String& one, double value);

  String fromInt(int value);
  String fromFloat(float value);
  String fromDouble(double value);
  String fromBool(bool value);


  int str_utf8_to_wcs(const char* src, int items, Char* dst, int maxItems);
  int str_wcs_to_utf8(const Char* src, int items, Octet* dst, int maxItems);

  String uniqueName(const char* prefix);


  class StrHelper
  {
  public:
    StrHelper(const String& str)
    {
      int reqLen = str.toUtf8(NULL, str.length() * 5);
      fBuffer.resize(reqLen + 1);
      fLength = str.toUtf8(&fBuffer[0], reqLen + 1);
    }

    int length() const
    {
      return fLength;
    }

    operator const char*() const
    {
      return &fBuffer[0];
    }

  private:
    std::vector<char> fBuffer;
    int fLength;
  };


  String xmlEncode(const String& str);
  String xmlEncode(const char* str);

#if defined(UNITTESTS)
  std::ostream& operator<<(std::ostream& os, const String& str);
  std::ostream& operator<<(std::ostream& os, char c);
#endif
};

#endif  // bootstrap_str_h
