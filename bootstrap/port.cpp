/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "common.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "require.h"
#include "str.h"
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

FilePort::FilePort(const String& fileName, zstring mode)
  : fOwnsStream(true),
    fStream(nullptr)
{
  fStream = ::fopen((zstring)StrHelper(fileName), mode);
  if (!fStream)
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
  if (fStream && fOwnsStream) {
    ::fclose(fStream);
    fStream = nullptr;
  }
}


bool
FilePort::isOpen() const
{
  return fStream;
}


bool
FilePort::isEof() const
{
  if (!fStream)
    throw PortNotOpenException();

  return !hasUnreadData() && ::feof(fStream) != 0;
}


size_t
FilePort::write(const Octet* data, size_t items)
{
  if (!fStream)
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
  if (!fStream)
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
  if (!fStream)
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
  if (!fStream)
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
  if (!fStream)
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
  if (!fStream)
    throw PortNotOpenException();

  resetUnreadBuffer();

  if (fseek(fStream, cursor, SEEK_SET) != 0)
    throw IOException(String("seek failed"), errno);
}


long
FilePort::cursor()
{
  if (!fStream)
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
    fData(nullptr),
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
  if (fOwnsData && fData) {
    ::free(fData);
    fData = nullptr;
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
  resetUnreadBuffer();
  fPos = cursor;
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

CharPort::CharPort(std::shared_ptr<Port<Octet>> slave)
  : fSlave(slave)
{
  hr_assert(fSlave);
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

  int clen = str_wcs_to_utf8(data, items, nullptr, items * 6 + 1);
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
herschel::display(Port<Octet>& port, zstring value)
{
  if (value)
    port.write((const Octet*)value, strlen(value));
  else
    port.write((const Octet*)"(null)", 6);
}


void
herschel::displayln(Port<Octet>& port, zstring value)
{
  display(port, value);
  display(port, "\n");
}


void
herschel::display(Port<Octet>& port, const String& value)
{
  display(port, StrHelper(value));
}


void
herschel::displayln(Port<Octet>& port, const String& value)
{
  display(port, value);
  display(port, "\n");
}
