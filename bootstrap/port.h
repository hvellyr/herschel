/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_port_h
#define bootstrap_port_h

#include <vector>

#include "refcountable.h"
#include "exception.h"
#include "ptr.h"


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

    int errCode() const
    {
      return fErrCode;
    }

  protected:
    int fErrCode;
  };


  //--------------------------------------------------------------------------

  class PortNotOpenException : public IOException
  {
  public:
    PortNotOpenException();
  };


  //--------------------------------------------------------------------------

  class EofException : public Exception
  {
  public:
    EofException()
      : Exception(String("End of file reached"))
    { }
  };


  //--------------------------------------------------------------------------

  template <typename T>
  class Port : public RefCountable
  {
  public:
    virtual bool isOpen() const = 0;
    virtual bool isEof() const = 0;

    virtual size_t write(T* data, size_t items)
    {
      for (size_t i = 0; i < items; i++) {
        int rv = this->write(data[i]);
        if (rv != 1)
          return rv;
      }
      return items;
    }
    virtual int write(T item) = 0;

    virtual size_t read(T* buffer, size_t items)
    {
      size_t i = 0;
      try {
        for ( ; i < items; i++)
          buffer[i] = this->read();
      }
      catch (const EofException& )
      {
      }
      return i;
    }
    virtual T read() = 0;

    virtual void flush() = 0;

    virtual bool canSetCursor() const
    {
      return false;
    }

    virtual void setCursor(size_t /* cursor */)
    {
    }

    virtual long cursor()
    {
      return 0;
    }
  };


  //--------------------------------------------------------------------------

  class FilePort : public Port<Octet>
  {
  public:
    FilePort(const String& fileName, const char* mode);
    ~FilePort();

    void close();

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual size_t write(Octet* data, size_t items);
    virtual int write(Octet byte);

    virtual size_t read(Octet* buffer, size_t items);
    virtual Octet read();

    virtual void flush();

    virtual bool canSetCursor() const;
    virtual void setCursor(size_t cursor);
    virtual long cursor();

  private:
    FILE* fStream;
  };


  //--------------------------------------------------------------------------

  class DataPort : public Port<Octet>
  {
  public:
    DataPort();
    DataPort(const Octet* buffer, size_t items);
    ~DataPort();

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual size_t write(Octet* data, size_t items);
    virtual int write(Octet byte);

    virtual size_t read(Octet* buffer, size_t items);
    virtual Octet read();

    virtual void flush();

    virtual bool canSetCursor() const;
    virtual void setCursor(size_t cursor);
    virtual long cursor();

    const Octet* data() const;
    size_t length() const;

  private:
    size_t fPos;
    bool   fOwnsData;
    Octet* fData;
    size_t fLength;
    size_t fAllocated;
  };


  //--------------------------------------------------------------------------

  class CharPort : public Port<Char>
  {
  public:
    CharPort(Port<Octet>* slave);

    virtual bool isOpen() const;
    virtual bool isEof() const;

    virtual size_t write(Char* data, size_t items);
    virtual int write(Char c);

    using Port<Char>::read;
    virtual Char read();

    virtual void flush();

    virtual bool canSetCursor() const;
    virtual void setCursor(size_t cursor);
    virtual long cursor();

  private:
    Ptr<Port<Octet> > fSlave;
    std::vector<Octet> fEncBuffer;
  };
};                              // namespace

#endif                          // bootstrap_port_h
