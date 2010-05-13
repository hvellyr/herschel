/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "srcpos.h"
#include "str.h"
#include "strbuf.h"
#include "unittests.h"


using namespace heather;

String
SrcPos::toString() const
{
  return (StringBuffer(fFile) << ':' << fromInt(fLineNo)).toString();
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class SrcPosUnitTest : public UnitTest
{
public:
  SrcPosUnitTest() : UnitTest("SrcPos") {}

  virtual void run()
  {
    SrcPos pos;
    assert(pos.lineNumber() == 0 && pos.file() == String());
    assert(pos.toString() == String(":0"));

    pos = SrcPos("abc.hea", 1211);
    assert(pos.lineNumber() == 1211 && pos.file() == String("abc.hea"));
    assert(pos.toString() == String("abc.hea:1211"));
  }
};

static SrcPosUnitTest srcPosUnitTest;

#endif  // #if defined(UNITTESTS)

