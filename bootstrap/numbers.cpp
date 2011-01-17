/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
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


//------------------------------------------------------------------------------

#if defined(UNITTESTS)

#include <UnitTest++.h>
#include <iostream>

std::ostream& herschel::operator<<(std::ostream& os, const Rational& rat)
{
  os << rat.numerator() << "/" << rat.denominator();
  return os;
}

SUITE(RationalNumber)
{
  TEST(ctor)
  {
    CHECK_EQUAL(Rational().numerator(), 0);
    CHECK_EQUAL(Rational().denominator(), 1);

    CHECK_EQUAL(Rational(0, 1), Rational());

    Rational r(42, 11);
    CHECK_EQUAL(r.numerator(), 42);
    CHECK_EQUAL(r.denominator(), 11);

    CHECK_EQUAL(Rational(r), r);
  }

  TEST(assign)
  {
    Rational r(42, 11);
    r = Rational(123, 17);
    CHECK_EQUAL(r.denominator(), 17);
    CHECK_EQUAL(r.numerator(), 123);
  }


  TEST(compare)
  {
    CHECK(Rational(5, 4) > Rational(4, 4));
    CHECK(Rational(5, 4) >= Rational(4, 4));
    CHECK(Rational(4, 4) >= Rational(4, 4));

    CHECK(Rational(4, 4) < Rational(5, 4));
    CHECK(Rational(4, 4) <= Rational(5, 4));
    CHECK(Rational(4, 4) <= Rational(4, 4));

    CHECK(Rational(4, 4) == Rational(4, 4));
    CHECK(Rational(4, 4) != Rational(5, 4));
  }


  TEST(toreal)
  {
    CHECK_EQUAL(Rational(22, 11).toReal(), 2.0);
  }


  TEST(add)
  {
    CHECK_EQUAL(Rational(5, 4) + Rational(2, 4), Rational(7, 4));
    CHECK_EQUAL(Rational(-5, 4) + Rational(2, 4), Rational(-3, 4));
    CHECK_EQUAL(Rational(2, 4) + Rational(-5, 4), Rational(-3, 4));
    CHECK_EQUAL(Rational(7, 9) + Rational(11, 4), Rational(127, 36));

    CHECK_EQUAL(Rational() + Rational(11, 4), Rational(11, 4));
  }


  TEST(minus)
  {
    CHECK_EQUAL(Rational(5, 4) - Rational(2, 4), Rational(3, 4));
    CHECK_EQUAL(Rational(-5, 4) - Rational(2, 4), Rational(-7, 4));
    CHECK_EQUAL(Rational(2, 4) - Rational(-5, 4), Rational(7, 4));
    CHECK_EQUAL(Rational(7, 9) - Rational(11, 4), Rational(-71, 36));
  }


  TEST(multiply)
  {
    CHECK_EQUAL(Rational(5, 4) * Rational(2, 4), Rational(10, 16));
    CHECK_EQUAL(Rational(-5, 4) * Rational(2, 4), Rational(-10, 16));
    CHECK_EQUAL(Rational(2, 4) * Rational(-5, 4), Rational(-10, 16));

    CHECK_EQUAL(Rational(7, 9) * Rational(11, 4), Rational(77, 36));
  }


  TEST(devide)
  {
    CHECK_EQUAL(Rational(5, 4) / Rational(2, 4), Rational(20, 8));
    CHECK_EQUAL(Rational(-5, 4) / Rational(2, 4), Rational(-20, 8));
    CHECK_EQUAL(Rational(2, 4) / Rational(-5, 4), Rational(8, -20));

    CHECK_EQUAL(Rational(7, 9) / Rational(11, 4), Rational(28, 99));
  }


  TEST(exponent)
  {
    CHECK_EQUAL(Rational(42, 11).exponent(0), Rational(1, 1));
    CHECK_EQUAL(Rational(5, 4).exponent(3), Rational(125, 64));

    CHECK_EQUAL(Rational(5, 4).exponent(-3), Rational(64, 125));
  }
}

#endif
