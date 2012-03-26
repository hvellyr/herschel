/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "utils.h"
#include "str.h"

#include <vector>


//----------------------------------------------------------------------------

using namespace herschel;


//----------------------------------------------------------------------------

#if defined(UNITTESTS)

#include <UnitTest++.h>

SUITE(VectorOf)
{
  TEST(vector_of)
  {
    std::vector<int> a = vector_of(1)(2)(3)(4)(5);
    CHECK_EQUAL(a.size(), 5);
    CHECK_EQUAL(a[0], 1);
    CHECK_EQUAL(a[1], 2);
    CHECK_EQUAL(a[2], 3);
    CHECK_EQUAL(a[3], 4);
    CHECK_EQUAL(a[4], 5);
  }

  TEST(vector_of_1)
  {
    std::vector<int> a = vector_of(42);
    CHECK_EQUAL(a.size(), 1);
    CHECK_EQUAL(a[0], 42);
  }

  TEST(vector_of_complex_type)
  {
    std::vector<String> a = vector_of(String("hello"))
                                     (String(" "))
                                     (String("world"));
    CHECK_EQUAL(a.size(), 3);
    CHECK_EQUAL(a[0], String("hello"));
    CHECK_EQUAL(a[1], String(" "));
    CHECK_EQUAL(a[2], String("world"));
  }
}

#endif  // #if defined(UNITTESTS)
