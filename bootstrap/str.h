/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_str_h
#define bootstrap_str_h

#include "common.h"

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

  private:
    StringImpl* fImpl;
  };

  String operator+(const String& one, const char* two);
  String operator+(const String& one, int value);
  String operator+(const String& one, float value);

  String fromInt(int value);
  String fromFloat(float value);
  String fromBool(bool value);
};

#endif  // bootstrap_str_h
