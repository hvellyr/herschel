/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../ast.hpp"
#include "../str.hpp"

#include <memory>
#include <string>


namespace herschel {

TEST_CASE("AST ApplyNode", "[ast][apply-node]")
{
  auto an = makeApplyNode(SrcPos(), makeSymbolNode(SrcPos(), String("xyz")));
  REQUIRE(an->isSimpleCall());
  REQUIRE(an->simpleCallName() == String("xyz"));

  auto an2 = makeApplyNode(
      SrcPos(), makeApplyNode(SrcPos(), makeSymbolNode(SrcPos(), String("get-func"))));
  REQUIRE(!an2->isSimpleCall());
  REQUIRE(an2->simpleCallName() == String());
}

}  // namespace herschel
