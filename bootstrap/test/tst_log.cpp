/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../log.hpp"


using namespace herschel;

TEST_CASE("LogSurpressor", "[logging]")
{
  REQUIRE(!isSilent());

  {
    LogSurpressor beSilent;
    REQUIRE(isSilent());

    {
      LogSurpressor beSilent2;
      REQUIRE(isSilent());
    }
    REQUIRE(isSilent());
  }

  REQUIRE(!isSilent());
}
