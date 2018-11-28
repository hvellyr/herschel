/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../str.hpp"
#include "../utils.hpp"


using namespace herschel;

TEST_CASE("MakeVector", "[utils]")
{
  SECTION("basic")
  {
    std::vector<int> a = makeVector(1, 2, 3);
    REQUIRE(a.size() == 3);
    REQUIRE(a[0] == 1);
    REQUIRE(a[1] == 2);
    REQUIRE(a[2] == 3);
  }

  SECTION("MakeVector with 1 element")
  {
    std::vector<int> a = makeVector(42);
    REQUIRE(a.size() == 1);
    REQUIRE(a[0] == 42);
  }

  SECTION("MakeVector of complex type")
  {
    std::vector<String> a = makeVector(String("hello"),
                                       String(" "),
                                       String("world"));
    REQUIRE(a.size() == 3);
    REQUIRE(a[0] == String("hello"));
    REQUIRE(a[1] == String(" "));
    REQUIRE(a[2] == String("world"));
  }
}
