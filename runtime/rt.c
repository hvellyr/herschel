/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include <stdio.h>

#include "runtime/rt.h"


//------------------------------------------------------------------------------

int
atom_2_int(struct ATOM a)
{
  return a.u.v_int;
}


int
atom_2_bool(struct ATOM a)
{
  /* assert(a.typeid == 1); */
  return a.u.v_int;
}


int
class_register(const char* typeid, int instantiable, int isize)
{
  /* stub */
  printf("Register class '%s' (%s, %d oct)\n", typeid,
         (instantiable ? "yes" : "no"), isize);
  return 0;
}
