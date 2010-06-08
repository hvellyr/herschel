/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "valuesaver.h"
#include "unittests.h"


using namespace heather;

#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class ValueSaverUnitTest : public UnitTest
{
public:
  ValueSaverUnitTest() : UnitTest("ValueSaver") {}

  virtual void run()
  {
    int x = 11;
    {
      ValueSaver<int> vs(x);
      x = 5;
    }
    assert(x == 11);

    {
      ValueSaver<int> vs(x, 22);
      assert(x == 22);
    }
    assert(x == 11);
  }
};

static ValueSaverUnitTest valueSaverUnitTest;

#endif  // #if defined(UNITTESTS)
