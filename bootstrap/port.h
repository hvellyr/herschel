/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_port_h
#define bootstrap_port_h

#include "refcountable.h"
#include "exception.h"


//----------------------------------------------------------------------------

namespace heather
{

  //--------------------------------------------------------------------------

  class IOException : public Exception
  {
  public:
    IOException(const String& msg, int errCode)
      : Exception(msg),
        fErrCode(errCode)
    { }

    int errCode()
    {
      return fErrCode;
    }

  protected:
    int fErrCode;
  };


  //--------------------------------------------------------------------------

  class Port : public RefCountable
  {
  public:
    virtual bool isOpen() const = 0;
    virtual bool isEof() const = 0;

    virtual size_t write(void* data, size_t items) = 0;
    virtual int write(Octet byte) = 0;

    virtual size_t read(void* buffer, size_t items) = 0;
    virtual int read() = 0;

    virtual void flush() = 0;

    virtual void setCursor(size_t cursor) = 0;
    virtual long cursor() = 0;
  };


  //--------------------------------------------------------------------------

  class FilePort : public Port
  {
  public:
    FilePort(const String& fileName, const char* mode);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual size_t write(void* data, size_t items);
    virtual int write(Octet byte);

    virtual size_t read(void* buffer, size_t items);
    virtual int read();

    virtual void flush();

    virtual void setCursor(size_t cursor);
    virtual long cursor();

  private:
    FILE* fStream;
  };

};                              // namespace

#endif                          // bootstrap_port_h
