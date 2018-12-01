/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../predefined.hpp"
#include "../str.hpp"
#include "../symbol.hpp"


namespace herschel {

TEST_CASE("Symbol mangling", "[symbol]")
{
  REQUIRE(String("__QN13hello/2dworld") == mangleToC(String("hello-world")));
  REQUIRE(String("__QN03app4main") == mangleToC(Names::kAppMain));
  REQUIRE(String("__QN4core2io12/2astdout/2a") == mangleToC(String("core.io.*stdout*")));
  REQUIRE(String("__QN9call/2fcc") == mangleToC(String("call/cc")));
}

}  // namespace herschel
