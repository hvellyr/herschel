/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef runtime_rt_h
#define runtime_rt_h

struct ATOM
{
  int typeid;
  union {
    int v_int;
  } u;
};

#endif                          // runtime_rt_h
