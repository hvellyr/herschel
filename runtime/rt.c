/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <stdio.h>

#include "runtime/rt.h"
#include "runtime/trace.h"


static const char*
atom_type_name(long typeid)
{
  static char buffer[256];

  switch (typeid) {
  case 'i': return "lang|Int32";
  case 'b': return "lang|Bool";
  case 'c': return "lang|Char";
  case 'A': return "lang|Any";
  case 'k': return "lang|Keyword";
  }

  sprintf(buffer, "unknown(%ld)", typeid);

  return buffer;
}


static void
unexpected_atom_type(long expected_id, long found_id)
{
  fprintf(stderr, "Unexpected type. Expected '%s', actually found '%s'\n",
          atom_type_name(expected_id), atom_type_name(found_id));
  exit(1);
}

int8_t
atom_2_int8(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to int8 */
  return (int8_t)a.u.v_int;
}


int16_t
atom_2_int16(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to int16 */
  return (int16_t)a.u.v_int;
}


int32_t
atom_2_int32(struct ATOM a)
{
  if (a.typeid != 'i')
    unexpected_atom_type('i', a.typeid);

  return (int32_t)a.u.v_int;
}


uint8_t
atom_2_uint8(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to uint8 */
  return (uint8_t)a.u.v_int;
}


uint16_t
atom_2_uint16(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to uint16 */
  return (uint16_t)a.u.v_int;
}


uint32_t
atom_2_uint32(struct ATOM a)
{
  /* TODO: assert that a.typeid refers to uint32 */
  return (uint32_t)a.u.v_int;
}


uint32_t
atom_2_char(struct ATOM a)
{
  if (a.typeid != 'c')
    unexpected_atom_type('c', a.typeid);

  return (uint32_t)a.u.v_int;
}


int
atom_2_bool(struct ATOM a)
{
  if (a.typeid != 'b')
    unexpected_atom_type('b', a.typeid);

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


void*
atom_2_keyword(struct ATOM a)
{
  if (a.typeid != 'k')
    unexpected_atom_type('k', a.typeid);

  return (void*)a.u.v_obj;
}


/* ------------------------------------------------------------------------
   class register
   ------------------------------------------------------------------------ */
extern void type_init();
extern void methods_init();
extern void keywords_init();

void
runtime_init()
{
  static int is_initialized = 0;

  if (!is_initialized) {
    is_initialized = 1;

#if defined(UNITTESTS)
  ATOM a;
  hr_trace("platform", "Sizeof ATOM struct: %ld", sizeof(ATOM));
  hr_trace("platform", "Offset typeid:      %ld", (char*)&a.typeid - (char*)&a);
  hr_trace("platform", "Offset u.v_obj:     %ld", (char*)&a.u.v_obj - (char*)&a);
#endif

    type_init();
    methods_init();
    keywords_init();
  }
}
