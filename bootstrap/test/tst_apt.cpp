/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../apt.h"
#include "../str.h"

#include <string>


using namespace herschel;

TEST_CASE("APT ApplyNode", "[apt][apply-node]")
{
  Ptr<ApplyNode> an = new ApplyNode(SrcPos(), new SymbolNode(SrcPos(), String("xyz")));
  REQUIRE(an->isSimpleCall());
  REQUIRE(an->simpleCallName() == String("xyz"));

  Ptr<ApplyNode> an2 = new ApplyNode(SrcPos(),
                                     new ApplyNode(SrcPos(),
                                                   new SymbolNode(SrcPos(),
                                                                  String("get-func"))));
  REQUIRE(!an2->isSimpleCall());
  REQUIRE(an2->simpleCallName() == String());
}
