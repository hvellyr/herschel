/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
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
#include "unittests.h"

using namespace heather;


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

  return ::feof(fStream) != 0;
}


size_t
FilePort::write(const Octet* data, size_t items)
{
  if (fStream == NULL)
    throw PortNotOpenException();

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

  size_t retval = ::fread(buffer, 1, items, fStream);
  if (retval == 0 && ::ferror(fStream) != 0)
    throw IOException(String("read failed"), errno);
  return retval;
}


Octet
FilePort::read()
{
  if (fStream == NULL)
    throw PortNotOpenException();

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
  return fPos == fLength;
}


size_t
DataPort::write(const Octet* data, size_t items)
{
  if (!fOwnsData)
    throw IOException(String("Can't write this port"), EPERM);

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
  return this->write(&byte, 1);
}


size_t
DataPort::read(Octet* buffer, size_t items)
{
  size_t bytesRead = 0;
  Octet* dst = (Octet*)buffer;
  size_t noctets = items;

  if (noctets == 0)
    return bytesRead;

  size_t avail2 = fLength - fPos;
  if (avail2 > 0)
  {
    size_t step = avail2 > noctets ? noctets : avail2;

    ::memcpy(dst, fData + fPos, step);
    fPos += step;

    return step;
  }

  return 0;
}


Octet
DataPort::read()
{
  if (fPos < fLength)
    return (int)fData[fPos++];
  throw EofException();
}


void
DataPort::flush()
{
  // NOP
}


bool
DataPort::canSetCursor() const
{
  return true;
}


void
DataPort::setCursor(size_t cursor)
{
  if (cursor >= 0)
    fPos = cursor;
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
  return fSlave->isEof();
}


size_t
CharPort::write(const Char* data, size_t items)
{
  int clen = str_wcs_to_utf8(data, items, NULL, items * 6 + 1);
  fEncBuffer.reserve(clen + 1);
  clen = str_wcs_to_utf8(data, items, &fEncBuffer[0], clen + 1);
  fSlave->write(&fEncBuffer[0], clen);
  return items;
}


int
CharPort::write(Char c)
{
  Octet tmp[10];
  int clen = str_wcs_to_utf8(&c, 1, tmp, 10);

  fSlave->write(tmp, clen);
  return 1;
}


Char
CharPort::read()
{
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
  fSlave->setCursor(cursor);
}


long
CharPort::cursor()
{
  return fSlave->cursor();
}


//----------------------------------------------------------------------------

void
heather::display(Port<Octet>* port, const char* value)
{
  if (value != NULL)
    port->write((const Octet*)value, strlen(value));
  else
    port->write((const Octet*)"(null)", 6);
}


void
heather::displayln(Port<Octet>* port, const char* value)
{
  display(port, value);
  display(port, "\n");
}


void
heather::display(Port<Octet>* port, const String& value)
{
  display(port, StrHelper(value));
}


void
heather::displayln(Port<Octet>* port, const String& value)
{
  display(port, value);
  display(port, "\n");
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class DataPortUnitTest : public UnitTest
{
public:
  DataPortUnitTest() : UnitTest("DataPort") {}

  virtual void run()
  {
#define PAGESIZE 243            // take a odd number to test chunking
#define PAGENUM    8
#define BUFSIZE  PAGENUM * PAGESIZE

    {
      Ptr<DataPort> dp = new DataPort;
      dp->write((Octet*)"hello, world!", 14); // incl. terminating 0
      assert(dp->length() == 14);
      assert(strcmp((char*)dp->data(), "hello, world!") == 0);
    }

    {
      Octet tmp[BUFSIZE];
      for (int i = 0; i < BUFSIZE; i++)
        tmp[i] = i % 256;

      Ptr<DataPort> dp = new DataPort(tmp, BUFSIZE);
      for (int i = 0; i < BUFSIZE; i++) {
        assert(dp->read() == i % 256);
      }
    }

    {
      Ptr<DataPort> dp = new DataPort;

      for (int i = 0; i < BUFSIZE; i++) {
        int x = dp->write(i % 256);
        assert(x == 1);
      }
      assert(dp->cursor() == BUFSIZE);

      dp->setCursor(0);
      for (int i = 0; i < BUFSIZE; i++) {
        assert(dp->read() == i % 256);
      }
    }

    {
      Ptr<DataPort> dp = new DataPort;

      for (int p = 0; p < PAGENUM; p++) {
        Octet tmp[PAGESIZE];
        for (int i = 0; i < PAGESIZE; i++)
          tmp[i] = (p * PAGESIZE + i) % 256;

        int x = dp->write(tmp, PAGESIZE);
        assert(x == PAGESIZE);
      }
      assert(dp->cursor() == BUFSIZE);

      dp->setCursor(0);
      for (int i = 0; i < BUFSIZE; i++) {
        assert(dp->read() == i % 256);
      }
    }

    {
      Octet tmp[] = { 'a', '\0' };
      Ptr<DataPort> dp = new DataPort(tmp, strlen((char*)tmp));

      assert(dp->read() == 'a');

      try {
        dp->read();
        assert(0);              // must not come here
      }
      catch (const EofException& ) {
      }
    }

#undef BUFSIZE
#undef PAGESIZE
  }
};
static DataPortUnitTest dataPortUnitTest;


class CharPortUnitTest : public UnitTest
{
public:
  CharPortUnitTest() : UnitTest("CharPort") {}

  virtual void run()
  {
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

      assert(dp->length() == 15);
      assert(::memcmp(dp->data(), expected, 15) == 0);

      dp->setCursor(0);
      for (int i = 0; i < 100; i++)
        cp->write(src, 6);

      assert(dp->length() == 15 * 100);
      for (int i = 0; i < 100; i++) {
        assert(::memcmp(dp->data() + i * 15, expected, 15) == 0);
      }
    }

    {
      Octet tmp[] = { 'h', 'e', 'l', 'l', 'o', 0xd0, 0xa4, '-', '-',
                      0xec, 0x95, 0x88, 0x00 };
      Char cs[] = { 'h', 'e', 'l', 'l', 'o', 0x424, '-', '-', 0xc548 };
      Ptr<CharPort> cp = new CharPort(new DataPort(tmp, strlen((char*)tmp)));
      for (int i = 0; i < 9; i++) {
        int c = cp->read();
        assert(c == cs[i]);
      }

      cp->setCursor(0);
      Char result[20];
      int clen = cp->read(result, 9);
      assert(clen == 9);

      assert(::memcmp(result, cs, sizeof(Char) * 9) == 0);
    }

    {
      Octet tmp[] = { 'a', '\0' };
      Ptr<CharPort> cp = new CharPort(new DataPort(tmp, strlen((char*)tmp)));

      assert(cp->read() == 'a');

      try {
        cp->read();
        assert(0);              // must not come here
      }
      catch (const EofException& ) {
      }
    }
  }
};
static CharPortUnitTest charPortUnitTest;


class FilePortUnitTest : public UnitTest
{
public:
  FilePortUnitTest() : UnitTest("FilePort") {}

  virtual void run()
  {
    {
      static const Octet tmp[] = "Hamlet:\n"
        "Alas, poor Yorick! I knew him, Horatio, a fellow of infinite\n"
        "jest, of most excellent fancy. He hath bore me on his back a\n"
        "thousand times, and now how abhorr'd in my imagination it is!\n"
        "My gorge rises at it.\n";

      Ptr<FilePort> port = new FilePort(String("tests/raw/01.bin"), "rb");

      const Octet* p = (Octet*)"\0";
      try {
        for (p = tmp; *p; p++) {
          Octet c = port->read();
          assert(c == *p);
        }
        // try to read beyond the end
        port->read();
        assert(0);
      }
      catch (const EofException& ) {
        assert(!*p);
      }
      int explen = strlen((const char*)tmp);
      assert(port->cursor() == explen);

      port->setCursor(0);

      Octet buffer[512];
      int readlen = port->read(buffer, explen);
      assert(readlen == explen);
      assert(memcmp(tmp, buffer, explen) == 0);

      port->close();
      assert(!port->isOpen());

      try {
        (void)port->isEof();
        assert(0);
      }
      catch (const PortNotOpenException& ) {
      }
    }

    {
      try {
        Ptr<FilePort> port = new FilePort(String("tests/raw/does-not-exist.bin"), "rb");
        assert(0);
      }
      catch (const IOException& e) {
        assert(e.errCode() == ENOENT);
      }
    }
  }
};
static FilePortUnitTest filePortUnitTest;

#endif  // #if defined(UNITTESTS)
