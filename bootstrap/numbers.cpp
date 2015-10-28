/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "numbers.h"

using namespace herschel;


//----------------------------------------------------------------------------

Rational&
Rational::operator=(const Rational& other)
{
  fNumerator = other.fNumerator;
  fDenominator = other.fDenominator;
  return *this;
}


bool
Rational::operator==(const Rational& other) const
{
  return fNumerator == other.fNumerator &&
    fDenominator == other.fDenominator;
}


bool
Rational::operator!=(const Rational& other) const
{
  return fNumerator != other.fNumerator ||
    fDenominator != other.fDenominator;
}


bool
Rational::operator>(const Rational& other) const
{
  return (fNumerator * other.fDenominator -
          other.fNumerator * fDenominator > 0);
}


bool
Rational::operator>=(const Rational& other) const
{
  return (fNumerator * other.fDenominator -
          other.fNumerator * fDenominator >= 0);
}


bool
Rational::operator<(const Rational& other) const
{
  return (fNumerator * other.fDenominator -
          other.fNumerator * fDenominator < 0);
}


bool
Rational::operator<=(const Rational& other) const
{
  return (fNumerator * other.fDenominator -
          other.fNumerator * fDenominator <= 0);
}


Rational
Rational::operator+(const Rational& other) const
{
  if (fDenominator == other.denominator())
    return Rational(fNumerator + other.numerator(), fDenominator);

  int otherDen = other.denominator();
  int n = fNumerator * otherDen;
  int na = other.numerator() * fDenominator;
  return Rational(n + na, fDenominator * otherDen);
}


Rational
Rational::operator-(const Rational& other) const
{
  if (fDenominator == other.denominator())
    return Rational(fNumerator - other.numerator(), fDenominator);

  int otherDen = other.denominator();
  int n = fNumerator * otherDen;
  int na = other.numerator() * fDenominator;
  return Rational(n - na, fDenominator * otherDen);
}


Rational
Rational::operator*(const Rational& other) const
{
  return Rational(fNumerator * other.numerator(),
                  fDenominator * other.denominator());
}


Rational
Rational::operator/(const Rational& other) const
{
  return Rational(fNumerator * other.denominator(),
                  fDenominator * other.numerator());
}


Rational
Rational::exponent(int exp) const
{
  if (exp > 0)
    return Rational(herschel::exponent(fNumerator, exp),
                    herschel::exponent(fDenominator, exp));
  else if (exp < 0)
    return Rational(herschel::exponent(fDenominator, -exp),
                    herschel::exponent(fNumerator, -exp));
  else
    return Rational(1, 1);
}


#if defined(UNITTESTS)
std::ostream& herschel::operator<<(std::ostream& os, const Rational& rat)
{
  os << rat.numerator() << "/" << rat.denominator();
  return os;
}
#endif
