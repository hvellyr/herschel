/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../str.h"
#include "../utils.h"


using namespace herschel;

TEST_CASE("Vector of", "[utils]")
{
  SECTION("basic")
  {
    std::vector<int> a = vector_of(1)(2)(3)(4)(5);
    REQUIRE(a.size() == 5);
    REQUIRE(a[0] == 1);
    REQUIRE(a[1] == 2);
    REQUIRE(a[2] == 3);
    REQUIRE(a[3] == 4);
    REQUIRE(a[4] == 5);
  }

  SECTION("vector_of with 1 element")
  {
    std::vector<int> a = vector_of(42);
    REQUIRE(a.size() == 1);
    REQUIRE(a[0] == 42);
  }

  SECTION("vector_of of complex type")
  {
    std::vector<String> a = vector_of(String("hello"))
                                     (String(" "))
                                     (String("world"));
    REQUIRE(a.size() == 3);
    REQUIRE(a[0] == String("hello"));
    REQUIRE(a[1] == String(" "));
    REQUIRE(a[2] == String("world"));
  }
}
