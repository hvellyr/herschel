/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_exception_h
#define bootstrap_exception_h

#include "str.h"

//----------------------------------------------------------------------------

namespace herschel
{
  //--------------------------------------------------------------------------

  class Exception
  {
  public:
    Exception(const String& msg)
      : fMsg(msg)
    { }

    const String& message() const
    {
      return fMsg;
    }

  protected:
    String fMsg;
  };


  //--------------------------------------------------------------------------

  class NotANumberException : public Exception
  {
  public:
    NotANumberException(const String& msg)
      : Exception(msg)
    { }
  };


  //--------------------------------------------------------------------------

  class NotSupportedException : public Exception
  {
  public:
    NotSupportedException(const char* functionName)
      : Exception(String("Method ") + functionName + " is not supported")
    { }
  };
};                              // namespace

#endif                          // bootstrap_exception_h
