/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#if defined(UNITTESTS)
#  include <iostream>
#endif

namespace herschel
{
  //--------------------------------------------------------------------------

  template<typename T>
  T exponent(T value, int exp)
  {
    T result = 1;
    for (int i = 0; i < exp; i++)
      result *= value;
    return result;
  }


  //--------------------------------------------------------------------------

  class Rational
  {
  public:
    Rational()
      : fNumerator(0),
        fDenominator(1)
      { }

    Rational(int num, int den)
      : fNumerator(num),
        fDenominator(den)
      { }

    Rational(const Rational& other)
      : fNumerator(other.fNumerator),
        fDenominator(other.fDenominator)
      { }

    Rational& operator=(const Rational& other);

    bool operator==(const Rational& other) const;
    bool operator!=(const Rational& other) const;
    bool operator>(const Rational& other) const;
    bool operator>=(const Rational& other) const;
    bool operator<(const Rational& other) const;
    bool operator<=(const Rational& other) const;

    Rational operator+(const Rational& other) const;
    Rational operator-(const Rational& other) const;
    Rational operator*(const Rational& other) const;
    Rational operator/(const Rational& other) const;
    Rational exponent(int exp) const;


    int numerator() const
    {
      return fNumerator;
    }

    int denominator() const
    {
      return fDenominator;
    }


    double toFloat() const
    {
      return double(fNumerator) / double(fDenominator);
    }

  private:
    int fNumerator;
    int fDenominator;
  };

#if defined(UNITTESTS)
  std::ostream& operator<<(std::ostream& os, const Rational& rat);
#endif

} // namespace
