/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include <stdio.h>

#include "runtime/rt.h"
#include "runtime/trace.h"


int8_t
atom_2_int8(struct ATOM a)
{
  // TODO: assert that a.typeid refers to int8
  return (int8_t)a.u.v_int;
}


int16_t
atom_2_int16(struct ATOM a)
{
  // TODO: assert that a.typeid refers to int16
  return (int16_t)a.u.v_int;
}


int32_t
atom_2_int32(struct ATOM a)
{
  // TODO: assert that a.typeid refers to int32
  return (int32_t)a.u.v_int;
}


uint8_t
atom_2_uint8(struct ATOM a)
{
  // TODO: assert that a.typeid refers to uint8
  return (uint8_t)a.u.v_int;
}


uint16_t
atom_2_uint16(struct ATOM a)
{
  // TODO: assert that a.typeid refers to uint16
  return (uint16_t)a.u.v_int;
}


uint32_t
atom_2_uint32(struct ATOM a)
{
  // TODO: assert that a.typeid refers to uint32
  return (uint32_t)a.u.v_int;
}


uint32_t
atom_2_char(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to char */
  return (uint32_t)a.u.v_int;
}


int
atom_2_bool(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to bool */
  return a.u.v_int;
}


float
atom_2_float32(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to float32 */
  return a.u.v_float;
}


int64_t
atom_2_int64(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to int64 */

  /* TODO: int64 are not allocated inline, but on heap */
  return (int64_t)0;
}


uint64_t
atom_2_uint64(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to uint64 */

  /* TODO: uint64 are not allocated inline, but on heap */
  return (uint64_t)0;
}


double
atom_2_float64(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to float64 */

  /* TODO: doubles are not allocated inline, but on heap */
  return 0.0;
}


/* ------------------------------------------------------------------------
   class register
   ------------------------------------------------------------------------ */
extern void type_init();
extern void methods_init();

void
runtime_init()
{
  static int is_initialized = 0;

#if defined(UNITTESTS)
  ATOM a;
  hr_trace("platform", "Sizeof ATOM struct: %ld", sizeof(ATOM));
  hr_trace("platform", "Offset typeid:      %ld", (char*)&a.typeid - (char*)&a);
  hr_trace("platform", "Offset u.v_obj:     %ld", (char*)&a.u.v_obj - (char*)&a);
#endif

  if (!is_initialized) {
    is_initialized = 1;

    type_init();
    methods_init();
  }
}
