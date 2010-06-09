/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_unittests_h
#define bootstrap_unittests_h

#include "str.h"

namespace heather
{
  class UnitTest
  {
  public:
    static void runUnitTests();

    UnitTest(const String& name);
    UnitTest(const char* name);
    virtual ~UnitTest();

    void runSuite();

    virtual void setup();
    virtual void run() = 0;
    virtual void teardown();

  protected:
    String fName;
  };
};                              // namespace

#endif // bootstrap_unittests_h
