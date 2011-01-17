/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "str.h"
#include "refcountable.h"
#include "port.h"

using namespace herschel;

// ENOTSUP seem not to be defined on windows (xp)?
#if !defined(ENOTSUP)
#define ENOTSUP 0x0ffffffe
#endif


//----------------------------------------------------------------------------

PortNotOpenException::PortNotOpenException()
  : IOException(String("port is not open"), EBADF)
{ }


//----------------------------------------------------------------------------

FilePort::FilePort(const String& fileName, const char* mode)
  : fOwnsStream(true),
    fStream(NULL)
{
  fStream = ::fopen((const char*)StrHelper(fileName), mode);
  if (fStream == NULL)
    throw IOException(String("Could not open file '") + fileName + "'", errno);
}


FilePort::FilePort(FILE* stream)
  : fOwnsStream(false),
    fStream(stream)
{ }


FilePort::~FilePort()
{
  close();
}


void
FilePort::close()
{
  if (fStream != NULL && fOwnsStream) {
    ::fclose(fStream);
    fStream = NULL;
  }
}


bool
FilePort::isOpen() const
{
  return fStream != NULL;
}


bool
FilePort::isEof() const
{
  if (fStream == NULL)
    throw PortNotOpenException();

  return !hasUnreadData() && ::feof(fStream) != 0;
}


size_t
FilePort::write(const Octet* data, size_t items)
{
  if (fStream == NULL)
    throw PortNotOpenException();

  resetUnreadBuffer();

  size_t retval = ::fwrite(data, 1, items, fStream);
  if (retval != items)
    throw IOException(String("write failed"), errno);
  return items;
}


int
FilePort::write(Octet byte)
{
  if (fStream == NULL)
    throw PortNotOpenException();

  resetUnreadBuffer();

  int retval = ::fputc(byte, fStream);
  if (retval != 1)
    throw IOException(String("write failed"), errno);
  return 1;
}


size_t
FilePort::read(Octet* buffer, size_t items)
{
  if (fStream == NULL)
    throw PortNotOpenException();

  size_t retval = readFromUnreadBuffer(buffer, items);

  size_t step = items - retval;
  if (step > 0) {
    size_t bytesread = ::fread(buffer + retval, 1, step, fStream);
    if (bytesread == 0 && ::ferror(fStream) != 0)
      throw IOException(String("read failed"), errno);
    retval += bytesread;
  }

  return retval;
}


Octet
FilePort::read()
{
  if (fStream == NULL)
    throw PortNotOpenException();

  Octet value;
  if (readFromUnreadBuffer(&value, 1) == 1)
    return value;

  int retval = ::fgetc(fStream);
  if (retval == EOF) {
    if (::ferror(fStream) != 0)
      throw IOException(String("read failed"), errno);
    else
      throw EofException();
  }
  return retval;
}


void
FilePort::flush()
{
  if (fStream == NULL)
    throw PortNotOpenException();
  resetUnreadBuffer();
  ::fflush(fStream);
}


bool
FilePort::canSetCursor() const
{
  return true;
}


void
FilePort::setCursor(size_t cursor)
{
  if (fStream == NULL)
    throw PortNotOpenException();

  resetUnreadBuffer();

  if (fseek(fStream, cursor, SEEK_SET) != 0)
    throw IOException(String("seek failed"), errno);
}


long
FilePort::cursor()
{
  if (fStream == NULL)
    throw PortNotOpenException();

  long pos = ftell(fStream);
  if (pos < 0)
    throw IOException(String("tell failed"), errno);
  return pos;
}


//----------------------------------------------------------------------------

DataPort::DataPort()
  : fPos(0),
    fOwnsData(true),
    fData(NULL),
    fLength(0),
    fAllocated(0)
{ }


DataPort::DataPort(const Octet* buffer, size_t items)
  : fPos(0),
    fOwnsData(false),
    fData((Octet*)buffer),
    fLength(items),
    fAllocated(items)
{
}


DataPort::~DataPort()
{
  if (fOwnsData && fData != NULL) {
    ::free(fData);
    fData = NULL;
  }
}


bool
DataPort::isOpen() const
{
  return true;
}


bool
DataPort::isEof() const
{
  return !hasUnreadData() && fPos == fLength;
}


size_t
DataPort::write(const Octet* data, size_t items)
{
  if (!fOwnsData)
    throw IOException(String("Can't write this port"), EPERM);

  resetUnreadBuffer();

  if (fPos + items >= fAllocated) {
    if (fAllocated == 0)
      fAllocated = 64;
    while (fPos + items >= fAllocated)
      fAllocated *= 2;
    fData = (Octet*)::realloc(fData, fAllocated * sizeof(Octet));
  }

  if (fPos > fLength)
    ::memset(fData + fLength, fPos - fLength, 0);

  ::memcpy(fData + fPos, data, items);

  fPos += items;
  if (fPos > fLength)
    fLength = fPos;

  return items;
}


int
DataPort::write(Octet byte)
{
  if (!fOwnsData)
    throw IOException(String("Can't write this port"), EPERM);

  resetUnreadBuffer();

  return this->write(&byte, 1);
}


size_t
DataPort::read(Octet* buffer, size_t items)
{
  size_t bytesRead = 0;
  size_t noctets = items;

  if (noctets == 0)
    return bytesRead;

  size_t retval = readFromUnreadBuffer(buffer, noctets);

  noctets -= retval;
  if (noctets > 0) {
    size_t avail2 = fLength - fPos;
    if (avail2 > 0)
    {
      size_t step = avail2 > noctets ? noctets : avail2;

      ::memcpy(buffer + retval, fData + fPos, step);
      fPos += step;

      return step + retval;
    }
  }

  return 0;
}


Octet
DataPort::read()
{
  Octet value;
  if (readFromUnreadBuffer(&value, 1) == 1)
    return value;

  if (fPos < fLength)
    return (int)fData[fPos++];
  throw EofException();
}


void
DataPort::flush()
{
  resetUnreadBuffer();
}


bool
DataPort::canSetCursor() const
{
  return true;
}


void
DataPort::setCursor(size_t cursor)
{
  if (cursor >= 0) {
    resetUnreadBuffer();
    fPos = cursor;
  }
  else
    throw IOException(String("Negative cursor"), ENOTSUP);
}


long
DataPort::cursor()
{
  return fPos;
}


const Octet*
DataPort::data() const
{
  return fData;
}


size_t
DataPort::length() const
{
  return fLength;
}


//----------------------------------------------------------------------------

CharPort::CharPort(Port<Octet>* slave)
  : fSlave(slave)
{
  assert(fSlave != NULL);
}


bool
CharPort::isOpen() const
{
  return fSlave->isOpen();
}


bool
CharPort::isEof() const
{
  return !hasUnreadData() && fSlave->isEof();
}


size_t
CharPort::write(const Char* data, size_t items)
{
  resetUnreadBuffer();

  int clen = str_wcs_to_utf8(data, items, NULL, items * 6 + 1);
  fEncBuffer.reserve(clen + 1);
  clen = str_wcs_to_utf8(data, items, &fEncBuffer[0], clen + 1);
  fSlave->write(&fEncBuffer[0], clen);
  return items;
}


int
CharPort::write(Char c)
{
  resetUnreadBuffer();

  Octet tmp[10];
  int clen = str_wcs_to_utf8(&c, 1, tmp, 10);

  fSlave->write(tmp, clen);
  return 1;
}


Char
CharPort::read()
{
  Char value;
  if (readFromUnreadBuffer(&value, 1) == 1)
    return value;

  int c0 = fSlave->read();
  if (c0 != EOF) {
    if (!(c0 & 0x80))
      return c0;
    else if ((c0 & 0xe0) == 0xc0) {
      int c1 = fSlave->read();
      return ((c0 & 0x1f) << 6) | (c1 & 0x3f);
    }
    else if ((c0 & 0xf0) == 0xe0) {
      Octet tmp[2];
      fSlave->read(tmp, 2);
      return ((c0 & 0x0f) << 12) | ((tmp[0] & 0x3f) << 6) | (tmp[1] & 0x3f);
    }
    else if ((c0 & 0xf8) == 0xf0) {
      Octet tmp[3];
      fSlave->read(tmp, 3);
      return ((c0 & 0x07) << 18) | ((tmp[0] & 0x3f) << 12)
        | ((tmp[1] & 0x3f) << 6) | (tmp[2] & 0x3f);
    }
    else if ((c0 & 0xfc) == 0xf8) {
      Octet tmp[4];
      fSlave->read(tmp, 4);
      return ((c0 & 0x03) << 24) | ((tmp[0] & 0x3f) << 18)
        | ((tmp[1] & 0x3f) << 12) | ((tmp[2] & 0x3f) << 6) | (tmp[3] & 0x3f);
    }
    else if ((c0 & 0xfe) == 0xfc) {
      Octet tmp[5];
      fSlave->read(tmp, 5);
      return ((c0 & 0x01) << 30) | ((tmp[0] & 0x3f) << 24)
        | ((tmp[1] & 0x3f) << 18) | ((tmp[2] & 0x3f) << 12)
        | ((tmp[3] & 0x3f) << 6) | (tmp[4] & 0x3f);
    }

    return 0xffff;
  }

  throw EofException();
}


void
CharPort::flush()
{
  resetUnreadBuffer();
  fSlave->flush();
}


bool
CharPort::canSetCursor() const
{
  return fSlave->canSetCursor();
}


void
CharPort::setCursor(size_t cursor)
{
  resetUnreadBuffer();
  fSlave->setCursor(cursor);
}


long
CharPort::cursor()
{
  return fSlave->cursor();
}


//----------------------------------------------------------------------------

void
herschel::display(Port<Octet>* port, const char* value)
{
  if (value != NULL)
    port->write((const Octet*)value, strlen(value));
  else
    port->write((const Octet*)"(null)", 6);
}


void
herschel::displayln(Port<Octet>* port, const char* value)
{
  display(port, value);
  display(port, "\n");
}


void
herschel::display(Port<Octet>* port, const String& value)
{
  display(port, StrHelper(value));
}


void
herschel::displayln(Port<Octet>* port, const String& value)
{
  display(port, value);
  display(port, "\n");
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>

SUITE(DataPort)
{
#define PAGESIZE 243            // take a odd number to test chunking
#define PAGENUM    8
#define BUFSIZE  PAGENUM * PAGESIZE

  TEST(StringWrite)
  {
    Ptr<DataPort> dp = new DataPort;
    dp->write((Octet*)"hello, world!", 14); // incl. terminating 0
    CHECK_EQUAL(dp->length(), (size_t)14);
    CHECK_EQUAL(strcmp((char*)dp->data(), "hello, world!"), 0);
  }


  TEST(BlockInitCtor)
  {
    Octet tmp[BUFSIZE];
    for (int i = 0; i < BUFSIZE; i++)
      tmp[i] = i % 256;

    Ptr<DataPort> dp = new DataPort(tmp, BUFSIZE);
    for (int i = 0; i < BUFSIZE; i++) {
      CHECK_EQUAL(dp->read(), i % 256);
    }
  }


  TEST(Write)
  {
    Ptr<DataPort> dp = new DataPort;

    for (int i = 0; i < BUFSIZE; i++) {
      int x = dp->write(i % 256);
      CHECK_EQUAL(x, 1);
    }
    CHECK_EQUAL(dp->cursor(), BUFSIZE);

    dp->setCursor(0);
    for (int i = 0; i < BUFSIZE; i++) {
      CHECK_EQUAL(dp->read(), i % 256);
    }
  }


  TEST(BlockWrite)
  {
    Ptr<DataPort> dp = new DataPort;

    for (int p = 0; p < PAGENUM; p++) {
      Octet tmp[PAGESIZE];
      for (int i = 0; i < PAGESIZE; i++)
        tmp[i] = (p * PAGESIZE + i) % 256;

      int x = dp->write(tmp, PAGESIZE);
      CHECK_EQUAL(x, PAGESIZE);
    }
    CHECK_EQUAL(dp->cursor(), BUFSIZE);

    dp->setCursor(0);
    for (int i = 0; i < BUFSIZE; i++) {
      CHECK_EQUAL(dp->read(), i % 256);
    }
  }


  TEST(FailingRead)
  {
    Octet tmp[] = { 'a', '\0' };
    Ptr<DataPort> dp = new DataPort(tmp, strlen((char*)tmp));

    CHECK_EQUAL(dp->read(), 'a');

    try {
      dp->read();
      CHECK(false);             // must not come here
    }
    catch (const EofException& ) {
    }
  }

#undef BUFSIZE
#undef PAGESIZE
}


SUITE(CharPort)
{
  TEST(BasicReadAndWrite)
  {
    Char src[] = { 'a',       // 61
                   0x424,     // d0 a4
                   0xc548,    // ec 95 88
                   0xfb00,    // ef ac 80
                   0xac00,    // ea b0 80
                   0x4e57 };  // e4 b9 97
    Ptr<DataPort> dp = new DataPort;
    Ptr<CharPort> cp = new CharPort(dp);

    for (int i = 0; i < 6; i++)
      cp->write(src[i]);

    Octet expected[] = {
      'a',
      0xd0, 0xa4,
      0xec, 0x95, 0x88,
      0xef, 0xac, 0x80,
      0xea, 0xb0, 0x80,
      0xe4, 0xb9, 0x97, 0x00
    };

    CHECK_EQUAL(dp->length(), (size_t)15);
    CHECK_EQUAL(::memcmp(dp->data(), expected, 15), 0);

    dp->setCursor(0);
    for (int i = 0; i < 100; i++)
      cp->write(src, 6);

    CHECK_EQUAL(dp->length(), (size_t)(15 * 100));
    for (int i = 0; i < 100; i++) {
      CHECK_EQUAL(::memcmp(dp->data() + i * 15, expected, 15), 0);
    }
  }


  TEST(BlockRead)
  {
    Octet tmp[] = { 'h', 'e', 'l', 'l', 'o', 0xd0, 0xa4, '-', '-',
                    0xec, 0x95, 0x88, 0x00 };
    Char cs[] = { 'h', 'e', 'l', 'l', 'o', 0x424, '-', '-', 0xc548 };
    Ptr<CharPort> cp = new CharPort(new DataPort(tmp, strlen((char*)tmp)));
    for (int i = 0; i < 9; i++) {
      int c = cp->read();
      CHECK_EQUAL(c, cs[i]);
    }

    cp->setCursor(0);
    Char result[20];
    int clen = cp->read(result, 9);
    CHECK_EQUAL(clen, 9);

    CHECK_EQUAL(::memcmp(result, cs, sizeof(Char) * 9), 0);
  }


  TEST(IllegalRead)
  {
    Octet tmp[] = { 'a', '\0' };
    Ptr<CharPort> cp = new CharPort(new DataPort(tmp, strlen((char*)tmp)));

    CHECK_EQUAL(cp->read(), 'a');

    try {
      cp->read();
      CHECK(false);             // must not come here
    }
    catch (const EofException& ) {
    }
  }
}


SUITE(FilePort)
{
  TEST(Test1)
  {
    static const Octet tmp[] = "Hamlet:\n"
      "Alas, poor Yorick! I knew him, Horatio, a fellow of infinite\n"
      "jest, of most excellent fancy. He hath bore me on his back a\n"
      "thousand times, and now how abhorr'd in my imagination it is!\n"
      "My gorge rises at it.\n";

    Ptr<FilePort> port;

    try {
      port = new FilePort(String("tests/raw/01.bin"), "rb");
    }
    catch (IOException e) {
      CHECK(false);
      return;
    }

    const Octet* p = (Octet*)"\0";
    try {
      for (p = tmp; *p; p++) {
        Octet c = port->read();
        CHECK_EQUAL(c, *p);
      }
      // try to read beyond the end
      port->read();
      CHECK(false);
    }
    catch (const EofException& ) {
      CHECK(!*p);
    }
    int explen = strlen((const char*)tmp);
    CHECK_EQUAL(port->cursor(), explen);

    port->setCursor(0);

    Octet buffer[512];
    int readlen = port->read(buffer, explen);
    CHECK_EQUAL(readlen, explen);
    CHECK_EQUAL(memcmp(tmp, buffer, explen), 0);

    port->close();
    CHECK(!port->isOpen());

    try {
      (void)port->isEof();
      CHECK(false);
    }
    catch (const PortNotOpenException& ) {
    }
  }


  TEST(Test2)
  {
    try {
      Ptr<FilePort> port = new FilePort(String("tests/raw/does-not-exist.bin"), "rb");
      CHECK(false);
    }
    catch (const IOException& e) {
      CHECK_EQUAL(e.errCode(), ENOENT);
    }
  }
}

#endif  // #if defined(UNITTESTS)
