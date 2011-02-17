/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef runtime_rt_h
#define runtime_rt_h

#include <stdint.h>


struct ATOM
{
  int typeid;
  union {
    int v_int;
    float v_float;
  } u;
};

#endif                          // runtime_rt_h
