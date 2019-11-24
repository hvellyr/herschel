/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../scope.hpp"
#include "../srcpos.hpp"
#include "../type.hpp"

#include <iostream>

namespace herschel {


TEST_CASE("Scope lookup type", "[scope]")
{
  SrcPos sp;
  Type t0 = Type::makeTypeRef(
      String("Foo"), std::vector<Type>{Type::makeTypeRef(String("Char"), K(isValue))},
      TypeConstVector(), K(isValue));

  auto s0 = makeScope(kScopeL_CompileUnit);
  Type t1 = s0->lookupType_unused(t0);
  // printf("%s\n", (zstring)StrHelper(t1.toString()));
  REQUIRE(t1.isDef());
}

}  // namespace herschel
