/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../valuesaver.h"


using namespace herschel;

TEST_CASE("ValueSaver", "[utils]")
{
  SECTION("default value")
  {
    int x = 11;
    {
      ValueSaver<int> vs(x);
      x = 5;
    }
    REQUIRE(x == 11);
  }


  SECTION("custom initial value")
  {
    int x = 11;
    {
      ValueSaver<int> vs(x, 22);
      REQUIRE(x == 22);
      x = 5;
    }
    REQUIRE(x == 11);
  }
}
