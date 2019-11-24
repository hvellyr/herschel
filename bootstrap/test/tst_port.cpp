/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../port.hpp"

namespace herschel {


#define PAGESIZE 243  // take a odd number to test chunking
#define PAGENUM 8
#define BUFSIZE PAGENUM* PAGESIZE

TEST_CASE("DataPort string write", "[port][data-port]")
{
  DataPort dp;
  dp.write((Octet*)"hello, world!", 14);  // incl. terminating 0
  REQUIRE(dp.length() == (size_t)14);
  REQUIRE(strcmp((char*)dp.data(), "hello, world!") == 0);
}


TEST_CASE("DataPort block init constructor", "[port][data-port]")
{
  Octet tmp[BUFSIZE];
  for (int i = 0; i < BUFSIZE; i++)
    tmp[i] = i % 256;

  DataPort dp{tmp, BUFSIZE};
  for (int i = 0; i < BUFSIZE; i++) {
    REQUIRE(dp.read() == (i % 256));
  }
}


TEST_CASE("DataPort write", "[port][data-port]")
{
  DataPort dp;

  for (int i = 0; i < BUFSIZE; i++) {
    int x = dp.write(i % 256);
    REQUIRE(x == 1);
  }
  REQUIRE(dp.cursor() == BUFSIZE);

  dp.setCursor(0);
  for (int i = 0; i < BUFSIZE; i++) {
    REQUIRE(dp.read() == (i % 256));
  }
}


TEST_CASE("DataPort block write", "[port][data-port]")
{
  DataPort dp;

  for (int p = 0; p < PAGENUM; p++) {
    Octet tmp[PAGESIZE];
    for (int i = 0; i < PAGESIZE; i++)
      tmp[i] = (p * PAGESIZE + i) % 256;

    int x = dp.write(tmp, PAGESIZE);
    REQUIRE(x == PAGESIZE);
  }
  REQUIRE(dp.cursor() == BUFSIZE);

  dp.setCursor(0);
  for (int i = 0; i < BUFSIZE; i++) {
    REQUIRE(dp.read() == (i % 256));
  }
}


TEST_CASE("DataPort failing read", "[port][data-port]")
{
  Octet tmp[] = {'a', '\0'};
  DataPort dp{tmp, strlen((char*)tmp)};

  REQUIRE(dp.read() == 'a');

  try {
    dp.read();
    REQUIRE(false);  // must not come here
  }
  catch (const EofException&) {
  }
}

#undef BUFSIZE
#undef PAGESIZE


TEST_CASE("CharPort basic read and write", "[port][char-port]")
{
  Char src[] = {'a',      // 61
                0x424,    // d0 a4
                0xc548,   // ec 95 88
                0xfb00,   // ef ac 80
                0xac00,   // ea b0 80
                0x4e57};  // e4 b9 97
  auto dp = std::make_shared<DataPort>();
  auto cp = std::make_shared<CharPort>(dp);

  for (int i = 0; i < 6; i++)
    cp->write(src[i]);

  Octet expected[] = {'a',  0xd0, 0xa4, 0xec, 0x95, 0x88, 0xef, 0xac,
                      0x80, 0xea, 0xb0, 0x80, 0xe4, 0xb9, 0x97, 0x00};

  REQUIRE(dp->length() == (size_t)15);
  REQUIRE(::memcmp(dp->data(), expected, 15) == 0);

  dp->setCursor(0);
  for (int i = 0; i < 100; i++)
    cp->write(src, 6);

  REQUIRE(dp->length() == (size_t)(15 * 100));
  for (int i = 0; i < 100; i++) {
    REQUIRE(::memcmp(dp->data() + i * 15, expected, 15) == 0);
  }
}


TEST_CASE("CharPort block read", "[port][char-port]")
{
  Octet tmp[] = {'h', 'e', 'l', 'l', 'o', 0xd0, 0xa4, '-', '-', 0xec, 0x95, 0x88, 0x00};
  Char cs[] = {'h', 'e', 'l', 'l', 'o', 0x424, '-', '-', 0xc548};
  auto cp =
      std::make_shared<CharPort>(std::make_shared<DataPort>(tmp, strlen((char*)tmp)));
  for (int i = 0; i < 9; i++) {
    int c = cp->read();
    REQUIRE(c == cs[i]);
  }

  cp->setCursor(0);
  Char result[20];
  int clen = cp->read(result, 9);
  REQUIRE(clen == 9);

  REQUIRE(::memcmp(result, cs, sizeof(Char) * 9) == 0);
}


TEST_CASE("CharPort illegal read", "[port][char-port]")
{
  Octet tmp[] = {'a', '\0'};
  auto cp =
      std::make_shared<CharPort>(std::make_shared<DataPort>(tmp, strlen((char*)tmp)));

  REQUIRE(cp->read() == 'a');

  try {
    cp->read();
    REQUIRE(false);  // must not come here
  }
  catch (const EofException&) {
  }
}


TEST_CASE("FilePort test1", "[port][file-port]")
{
  static const Octet tmp[] =
      "Hamlet:\n"
      "Alas, poor Yorick! I knew him, Horatio, a fellow of infinite\n"
      "jest, of most excellent fancy. He hath bore me on his back a\n"
      "thousand times, and now how abhorr'd in my imagination it is!\n"
      "My gorge rises at it.\n";

  std::shared_ptr<FilePort> port;

  try {
    port = std::make_shared<FilePort>(String("tests/raw/01.bin"), "rb");
  }
  catch (IOException e) {
    REQUIRE(false);
    return;
  }

  const Octet* p = (Octet*)"\0";
  try {
    for (p = tmp; *p; p++) {
      Octet c = port->read();
      REQUIRE(c == *p);
    }
    // try to read beyond the end
    port->read();
    REQUIRE(false);
  }
  catch (const EofException&) {
    REQUIRE(!*p);
  }
  int explen = strlen((zstring)tmp);
  REQUIRE(port->cursor() == explen);

  port->setCursor(0);

  Octet buffer[512];
  int readlen = port->read(buffer, explen);
  REQUIRE(readlen == explen);
  REQUIRE(memcmp(tmp, buffer, explen) == 0);

  port->close();
  REQUIRE(!port->isOpen());

  try {
    (void)port->isEof();
    REQUIRE(false);
  }
  catch (const PortNotOpenException&) {
  }
}


TEST_CASE("FilePort Test2", "[port][file-port]")
{
  try {
    auto port = std::make_shared<FilePort>(String("tests/raw/does-not-exist.bin"), "rb");
    REQUIRE(false);
  }
  catch (const IOException& e) {
    REQUIRE(e.errCode() == ENOENT);
  }
}

}  // namespace herschel
