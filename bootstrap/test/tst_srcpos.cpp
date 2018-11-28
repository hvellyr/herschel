/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../srcpos.hpp"

#include <iostream>


namespace herschel {
std::ostream& operator<<(std::ostream& os, const SrcPos& pos)
{
  os << StrHelper(pos.toString());
  return os;
}


TEST_CASE("SrcPos default constructor", "[srcpos]")
{
  SrcPos pos;
  REQUIRE(pos.lineNumber() == 0);
  REQUIRE(pos.file() == String());
  REQUIRE(pos.toString() == String(":0"));
}


TEST_CASE("SrcPos assignment", "[srcpos]")
{
  SrcPos pos;
  pos = SrcPos("abc.hr", 1211);
  REQUIRE(pos.lineNumber() == 1211);
  REQUIRE(pos.file() == String("abc.hr"));
  REQUIRE(pos.toString() == String("abc.hr:1211"));
}


TEST_CASE("SrcPos copy constructor", "[srcpos]")
{
  SrcPos p("abc.hr", 1211);
  SrcPos q(p);
  REQUIRE(q.lineNumber() == 1211);
  REQUIRE(q.file() == String("abc.hr"));
  REQUIRE(q.toString() == String("abc.hr:1211"));
}


TEST_CASE("SrcPos equal operator", "[srcpos]")
{
  REQUIRE(SrcPos("abc.hr", 1211) == SrcPos("abc.hr", 1211));
  REQUIRE(SrcPos() == SrcPos());
  REQUIRE(SrcPos("abc.hr", 1211) != SrcPos("abc.hr", 1300));
  REQUIRE(SrcPos("abc.hr", 1211) != SrcPos("abx.hr", 1211));
  REQUIRE(SrcPos("abc.hr", 1211) != SrcPos("abx.hr", 1300));
}

}  // namespace herschel
