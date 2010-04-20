/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_exception_h
#define bootstrap_exception_h

#include "str.h"

//----------------------------------------------------------------------------

namespace heather
{
  //--------------------------------------------------------------------------

  class Exception
  {
  public:
    Exception(const String& msg)
      : fMsg(msg)
    { }

  protected:
    String fMsg;
  };

};                              // namespace

#endif                          // bootstrap_exception_h
