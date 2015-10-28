/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../str.h"
#include "../file.h"

#include <string>


using namespace herschel;

TEST_CASE("File relative paths", "[path]")
{
  String t = String("tests/war/../raw/01.bin");

  String ct = file::canonicalPathName(t);

  REQUIRE(ct == file::workingDir() + "tests/raw/01.bin");
  REQUIRE(file::isFile(ct));
  REQUIRE(!file::isDir(ct));
}


TEST_CASE("File relative paths starting with a .", "[path]")
{
  String t = String("./tests/war/../raw/01.bin");

  String ct = file::canonicalPathName(t);

  REQUIRE(ct == file::workingDir() + "tests/raw/01.bin");
  REQUIRE(file::isFile(ct));
  REQUIRE(!file::isDir(ct));
}


TEST_CASE("File relative path to abs", "[path]")
{
  String t = String("tests/war/../raw/01.bin");

  String ct = file::canonicalPathName(t, String("/usr/lib/"));

  REQUIRE(ct == String("/usr/lib/tests/raw/01.bin"));
  REQUIRE(file::isFilePath(ct));
}


TEST_CASE("Append file to path", "[path]")
{
  String t = file::appendFile(String("/usr/share/herschel/"), String("a.xml"));
  REQUIRE(t == String("/usr/share/herschel/a.xml"));
  REQUIRE(file::isFilePath(t));

  REQUIRE(!file::isFilePath(String("/opt/")));
}


TEST_CASE("Append dir to path", "[path]")
{
  String t = file::appendDir(String("/usr/share/herschel/"), String("abc"));
  REQUIRE(t == String("/usr/share/herschel/abc/"));
  REQUIRE(!file::isFilePath(t));

  String z = file::appendDir(String("/usr/share/herschel/"), String("abc/"));
  REQUIRE(z == String("/usr/share/herschel/abc/"));
  REQUIRE(!file::isFilePath(z));
}


TEST_CASE("Append extensions to paths", "[path]")
{
  REQUIRE(file::appendExt(String("/usr/share/herschel/a"), String("xml")) ==
          String("/usr/share/herschel/a.xml"));
  REQUIRE(file::appendExt(String("/usr/share/herschel/a.xml"), String("txt")) ==
          String("/usr/share/herschel/a.xml.txt"));
}


TEST_CASE("Is absolute path", "[path]")
{
  REQUIRE(file::isAbsolutePath(String("/usr/lib/")));
  REQUIRE(!file::isAbsolutePath(String("tests/raw/01.bin")));
  REQUIRE(!file::isAbsolutePath(String("./tests/raw/01.bin")));
  REQUIRE(!file::isAbsolutePath(String("../tests/raw/01.bin")));
  REQUIRE(file::isAbsolutePath(String("~/tests/raw/01.bin")));
}


TEST_CASE("has path extension", "[path]")
{
  REQUIRE(!file::hasExtension(String("/usr/lib/")));
  REQUIRE(file::hasExtension(String("tests/raw/01.bin")));
  REQUIRE(!file::hasExtension(String("tests.all/raw/test")));
}
