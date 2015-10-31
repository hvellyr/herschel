/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../ptr.h"
#include "../scope.h"
#include "../srcpos.h"
#include "../type.h"

#include <iostream>

using namespace herschel;


TEST_CASE("Scope lookup type", "[scope]")
{
  SrcPos sp;
  Type t0 = Type::newTypeRef(String("Foo"),
                             std::vector<Type>{Type::newTypeRef(String("Char"),
                                                                K(isValue))},
                             TypeConstVector(),
                             K(isValue));

  auto s0 = makeScope(kScopeL_CompileUnit);
  Type t1 = s0->lookupType_unused(t0);
  // printf("%s\n", (zstring)StrHelper(t1.toString()));
  REQUIRE(t1.isDef());
}
