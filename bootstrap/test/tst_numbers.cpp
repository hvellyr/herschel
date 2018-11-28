/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../numbers.hpp"

#include <iostream>


using namespace herschel;

TEST_CASE("Rational constructor", "[numbers]")
{
  REQUIRE(Rational().numerator() == 0);
  REQUIRE(Rational().denominator() == 1);

  REQUIRE(Rational(0, 1) == Rational());

  Rational r(42, 11);
  REQUIRE(r.numerator() == 42);
  REQUIRE(r.denominator() == 11);

  REQUIRE(Rational(r) == r);
}


TEST_CASE("Rational assignment", "[numbers]")
{
  Rational r(42, 11);
  r = Rational(123, 17);
  REQUIRE(r.denominator() == 17);
  REQUIRE(r.numerator() == 123);
}


TEST_CASE("Rational compare", "[numbers]")
{
  REQUIRE(Rational(5, 4) > Rational(4, 4));
  REQUIRE(Rational(5, 4) >= Rational(4, 4));
  REQUIRE(Rational(4, 4) >= Rational(4, 4));

  REQUIRE(Rational(4, 4) < Rational(5, 4));
  REQUIRE(Rational(4, 4) <= Rational(5, 4));
  REQUIRE(Rational(4, 4) <= Rational(4, 4));

  REQUIRE(Rational(4, 4) == Rational(4, 4));
  REQUIRE(Rational(4, 4) != Rational(5, 4));
}


TEST_CASE("Rational to real", "[numbers]")
{
  REQUIRE(Rational(22, 11).toFloat() == 2.0);
}


TEST_CASE("Rational add", "[numbers]")
{
  REQUIRE(Rational(5, 4) + Rational(2, 4) == Rational(7, 4));
  REQUIRE(Rational(-5, 4) + Rational(2, 4) == Rational(-3, 4));
  REQUIRE(Rational(2, 4) + Rational(-5, 4) == Rational(-3, 4));
  REQUIRE(Rational(7, 9) + Rational(11, 4) == Rational(127, 36));

  REQUIRE(Rational() + Rational(11, 4) == Rational(11, 4));
}


TEST_CASE("Rational minus", "[numbers]")
{
  REQUIRE(Rational(5, 4) - Rational(2, 4) == Rational(3, 4));
  REQUIRE(Rational(-5, 4) - Rational(2, 4) == Rational(-7, 4));
  REQUIRE(Rational(2, 4) - Rational(-5, 4) == Rational(7, 4));
  REQUIRE(Rational(7, 9) - Rational(11, 4) == Rational(-71, 36));
}


TEST_CASE("Rational multiply", "[numbers]")
{
  REQUIRE(Rational(5, 4) * Rational(2, 4) == Rational(10, 16));
  REQUIRE(Rational(-5, 4) * Rational(2, 4) == Rational(-10, 16));
  REQUIRE(Rational(2, 4) * Rational(-5, 4) == Rational(-10, 16));

  REQUIRE(Rational(7, 9) * Rational(11, 4) == Rational(77, 36));
}


TEST_CASE("Rational devide", "[numbers]")
{
  REQUIRE(Rational(5, 4) / Rational(2, 4) == Rational(20, 8));
  REQUIRE(Rational(-5, 4) / Rational(2, 4) == Rational(-20, 8));
  REQUIRE(Rational(2, 4) / Rational(-5, 4) == Rational(8, -20));

  REQUIRE(Rational(7, 9) / Rational(11, 4) == Rational(28, 99));
}


TEST_CASE("Rational exponent", "[numbers]")
{
  REQUIRE(Rational(42, 11).exponent(0) == Rational(1, 1));
  REQUIRE(Rational(5, 4).exponent(3) == Rational(125, 64));

  REQUIRE(Rational(5, 4).exponent(-3) == Rational(64, 125));
}
