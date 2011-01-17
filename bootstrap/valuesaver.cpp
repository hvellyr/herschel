/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "valuesaver.h"


using namespace herschel;

#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>

SUITE(ValueSaver)
{
  TEST(DefaultInit)
  {
    int x = 11;
    {
      ValueSaver<int> vs(x);
      x = 5;
    }
    CHECK_EQUAL(x, 11);
  }


  TEST(CustomInit)
  {
    int x = 11;
    {
      ValueSaver<int> vs(x, 22);
      CHECK_EQUAL(x, 22);
      x = 5;
    }
    CHECK_EQUAL(x, 11);
  }
}

#endif  // #if defined(UNITTESTS)
