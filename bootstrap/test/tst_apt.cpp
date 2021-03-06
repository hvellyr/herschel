/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../apt.h"
#include "../str.h"

#include <memory>
#include <string>


using namespace herschel;

TEST_CASE("APT ApplyNode", "[apt][apply-node]")
{
  auto an = makeApplyNode(SrcPos(), makeSymbolNode(SrcPos(), String("xyz")));
  REQUIRE(an->isSimpleCall());
  REQUIRE(an->simpleCallName() == String("xyz"));

  auto an2 = makeApplyNode(SrcPos(),
                           makeApplyNode(SrcPos(),
                                         makeSymbolNode(SrcPos(),
                                                        String("get-func"))));
  REQUIRE(!an2->isSimpleCall());
  REQUIRE(an2->simpleCallName() == String());
}
