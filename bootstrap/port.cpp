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

using namespace heather;


FilePort::FilePort(const String& fileName, const char* mode)
  : fStream(NULL)
{
}


bool
FilePort::isOpen() const
{
  return fStream != NULL;
}


bool
FilePort::isEof() const
{
  if (fStream != NULL)
    return feof(fStream) != 0;
  throw IOException(String("port is not open"), EBADF);
}


size_t
FilePort::write(void* data, size_t items)
{
  if (fStream != NULL) {
    size_t retval = fwrite(data, 1, items, fStream);
    if (retval != items)
      throw IOException(String("write failed"), errno);
  }
  throw IOException(String("port is not open"), EBADF);
}


int
FilePort::write(Octet byte)
{
  if (fStream != NULL) {
    int retval = fputc(byte, fStream);
    if (retval != 1)
      throw IOException(String("write failed"), errno);
  }
  throw IOException(String("port is not open"), EBADF);
}


size_t
FilePort::read(void* buffer, size_t items)
{
  throw IOException(String("port is not open"), EBADF);
}


int
FilePort::read()
{
  if (fStream != NULL) {
  }
  throw IOException(String("port is not open"), EBADF);
}


void
FilePort::flush()
{
}


void
FilePort::setCursor(size_t cursor)
{
  if (fStream != NULL) {
    if (fseek(fStream, cursor, SEEK_SET) != 0)
      throw IOException(String("seek failed"), errno);
  }
  throw IOException(String("port is not open"), EBADF);
}


long
FilePort::cursor()
{
  if (fStream != NULL) {
    long pos = ftell(fStream);
    if (pos < 0)
      throw IOException(String("tell failed"), errno);
    return pos;
  }
  throw IOException(String("port is not open"), EBADF);
}


//----------------------------------------------------------------------------

class FilePortUnitTest
{
public:
  FilePortUnitTest()
  {
    fprintf(stderr, "Run file port unit tests ...\n");
  }
};


static FilePortUnitTest unitTest;
