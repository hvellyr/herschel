/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "common.h"
#include "srcpos.h"
#include "str.h"
#include "strbuf.h"


using namespace herschel;

String
SrcPos::toString() const
{
  return (StringBuffer(fFile) << ':' << fromInt(fLineNo)).toString();
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>
#include <iostream>

namespace herschel
{
  std::ostream& operator<<(std::ostream& os, const SrcPos& pos)
  {
    os << StrHelper(pos.toString());
    return os;
  }
};


SUITE(SrcPos)
{
  TEST(Default)
  {
    SrcPos pos;
    CHECK_EQUAL(pos.lineNumber(), 0);
    CHECK_EQUAL(pos.file(), String());
    CHECK_EQUAL(pos.toString(), String(":0"));
  }

  TEST(AssignOp)
  {
    SrcPos pos;
    pos = SrcPos("abc.hr", 1211);
    CHECK_EQUAL(pos.lineNumber(), 1211);
    CHECK_EQUAL(pos.file(), String("abc.hr"));
    CHECK_EQUAL(pos.toString(), String("abc.hr:1211"));
  }

  TEST(CopyCtor)
  {
    SrcPos p("abc.hr", 1211);
    SrcPos q(p);
    CHECK_EQUAL(q.lineNumber(), 1211);
    CHECK_EQUAL(q.file(), String("abc.hr"));
    CHECK_EQUAL(q.toString(), String("abc.hr:1211"));
  }


  TEST(EqualOp)
  {
    CHECK_EQUAL(SrcPos("abc.hr", 1211), SrcPos("abc.hr", 1211));
    CHECK_EQUAL(SrcPos(), SrcPos());
    CHECK(SrcPos("abc.hr", 1211) != SrcPos("abc.hr", 1300));
    CHECK(SrcPos("abc.hr", 1211) != SrcPos("abx.hr", 1211));
    CHECK(SrcPos("abc.hr", 1211) != SrcPos("abx.hr", 1300));
  }
}

#endif  // #if defined(UNITTESTS)

