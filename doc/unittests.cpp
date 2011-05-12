#include "unittests.h"

#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class XyzUnitTest : public UnitTest
{
public:
  XyzUnitTest() : UnitTest("Xyz") {}

  virtual void run()
  {
  }
};

static XyzUnitTest xyzUnitTest;

#endif  // #if defined(UNITTESTS)

