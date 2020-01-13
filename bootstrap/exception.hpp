/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "str.hpp"


namespace herschel {

//! Base class for message based exceptions
class Exception {
public:
  Exception(const Exception& ) = default;

  Exception(const String& msg)
      : fMsg(msg)
  {
  }

  const String& message() const { return fMsg; }

protected:
  String fMsg;
};


//--------------------------------------------------------------------------

//! Exception denoting a bad number notation.
//!
//! This exception will be thrown by string to number conversions.
class NotANumberException : public Exception {
public:
  NotANumberException(const String& msg)
      : Exception(msg)
  {
  }
};


//--------------------------------------------------------------------------

//! Exception denoting a function which is not (yet) implemented
class NotSupportedException : public Exception {
public:
  NotSupportedException(zstring functionName)
      : Exception(String("Method ") + functionName + " is not supported")
  {
  }
};


//! Exception denoting an aborted compilation
class AbortException : public Exception {
public:
  AbortException(const AbortException& ) = default;
  AbortException(const String& msg)
    : Exception(msg)
  {
  }
};

}  // namespace herschel
