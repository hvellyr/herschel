/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <map>

#include "unittests.h"
#include "exception.h"

using namespace heather;

typedef std::map<String, UnitTest*> UnitTestMap;

static UnitTestMap&
getUnitTestMap()
{
  static UnitTestMap sUnitTests;
  return sUnitTests;
}


static void
registerUnitTest(const String& name, UnitTest* ut)
{
  assert(getUnitTestMap().find(name) == getUnitTestMap().end());
  getUnitTestMap().insert(std::make_pair(name, ut));
}


UnitTest::UnitTest(const String& name)
  : fName(name)
{
  registerUnitTest(fName, this);
}


UnitTest::UnitTest(const char* name)
  : fName(String(name))
{
  registerUnitTest(fName, this);
}


UnitTest::~UnitTest()
{
  UnitTestMap::iterator it = getUnitTestMap().find(fName);
  assert(it != getUnitTestMap().end());
  getUnitTestMap().erase(it);
}


void
UnitTest::runSuite()
{
  fprintf(stderr, "Run unittests '%s' ...", (const char*)StrHelper(fName));
  fflush(stderr);

  try {
    setup();
  }
  catch (const Exception& e) {
    fprintf(stderr, " failed in setup (%s)\n", (const char*)StrHelper(e.message()));
    return;
  }
  catch (...) {
    fprintf(stderr, " failed in setup (other exception)\n");
    return;
  }

  try {
    run();
  }
  catch (const Exception& e) {
    fprintf(stderr, " failed (%s)\n", (const char*)StrHelper(e.message()));
    return;
  }
  catch (...) {
    fprintf(stderr, " failed (other exception)\n");
    return;
  }

  try {
    teardown();
  }
  catch (const Exception& e) {
    fprintf(stderr, " failed in teardown (%s)\n", (const char*)StrHelper(e.message()));
    return;
  }
  catch (...) {
    fprintf(stderr, " failed in teardown (other exception)\n");
    return;
  }

  fprintf(stderr, " ok\n");
}


void
UnitTest::setup()
{
  // NOP by default
}


void
UnitTest::teardown()
{
  // NOP by default
}


//----------------------------------------------------------------------------

void UnitTest::runUnitTests()
{
  UnitTestMap& map = getUnitTestMap();
  for (UnitTestMap::iterator it = map.begin(); it != map.end(); it++) {
    it->second->runSuite();
  }
}
