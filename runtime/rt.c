/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <stdio.h>

#include "runtime/herschel.h"
#include "runtime/trace.h"
#include "runtime/typeid.h"
#include "runtime/method.h"
#include "runtime/keyword.h"


static const char*
h7_atom_type_name(long typeid)
{
  static char buffer[256];

  switch (typeid) {
  case TYPE_TAG_INT8:   return "lang|Int8";
  case TYPE_TAG_INT16:  return "lang|Int16";
  case TYPE_TAG_INT32:  return "lang|Int32";
  case TYPE_TAG_INT64:  return "lang|Int64";
  case TYPE_TAG_UINT8:  return "lang|UInt8";
  case TYPE_TAG_UINT16: return "lang|UInt16";
  case TYPE_TAG_UINT32: return "lang|UInt32";
  case TYPE_TAG_UINT64: return "lang|UInt64";

  case TYPE_TAG_BOOL:   return "lang|Bool";
  case TYPE_TAG_CHAR:   return "lang|Char";
  case TYPE_TAG_ANY:    return "lang|Any";
  case TYPE_TAG_KEYW: return "lang|Keyword";
  }

  sprintf(buffer, "unknown(%ld)", typeid);

  return buffer;
}


static void
h7_unexpected_atom_type(long expected_id, long found_id)
{
  fprintf(stderr, "Unexpected type. Expected '%s', actually found '%s'\n",
          h7_atom_type_name(expected_id), h7_atom_type_name(found_id));
  exit(1);
}

int8_t
h7_atom_2_int8(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to int8 */
  return (int8_t)a.u.v_int;
}


int16_t
h7_atom_2_int16(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to int16 */
  return (int16_t)a.u.v_int;
}


int32_t
h7_atom_2_int32(H7_ATOM a)
{
  if (a.typeid != TYPE_TAG_INT32)
    h7_unexpected_atom_type(TYPE_TAG_INT32, a.typeid);

  return (int32_t)a.u.v_int;
}


uint8_t
h7_atom_2_uint8(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to uint8 */
  return (uint8_t)a.u.v_int;
}


uint16_t
h7_atom_2_uint16(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to uint16 */
  return (uint16_t)a.u.v_int;
}


uint32_t
h7_atom_2_uint32(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to uint32 */
  return (uint32_t)a.u.v_int;
}


uint32_t
h7_atom_2_char(H7_ATOM a)
{
  if (a.typeid != TYPE_TAG_CHAR)
    h7_unexpected_atom_type(TYPE_TAG_CHAR, a.typeid);

  return (uint32_t)a.u.v_int;
}


int
h7_atom_2_bool(H7_ATOM a)
{
  if (a.typeid != TYPE_TAG_BOOL)
    h7_unexpected_atom_type(TYPE_TAG_BOOL, a.typeid);

  return a.u.v_int;
}


float
h7_atom_2_float32(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to float32 */
  return a.u.v_float;
}


int64_t
h7_atom_2_int64(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to int64 */

  /* TODO: on 32bit machines the int64 are not allocated inline, but on
   * heap */
  return a.u.v_int64;
}


uint64_t
h7_atom_2_uint64(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to uint64 */

  /* TODO: on 32bit machines the uint64 are not allocated inline, but on
   * heap */
  return (uint64_t)a.u.v_int64;
}


double
h7_atom_2_float64(H7_ATOM a)
{
  /* TODO: assert that a.typeid refers to float64 */

  /* TODO: on 32bit machines the doubles are not allocated inline, but on
   * heap */
  return 0.0;
}


void*
h7_atom_2_keyword(H7_ATOM a)
{
  if (a.typeid != TYPE_TAG_KEYW)
    h7_unexpected_atom_type(TYPE_TAG_KEYW, a.typeid);

  return (void*)a.u.v_obj;
}


/* ------------------------------------------------------------------------
   class register
   ------------------------------------------------------------------------ */

void
h7_runtime_init()
{
  static int is_initialized = 0;

  if (!is_initialized) {
    is_initialized = 1;

#if defined(UNITTESTS)
  H7_ATOM a;
  h7_trace("platform", "Sizeof ATOM struct: %ld", sizeof(H7_ATOM));
  h7_trace("platform", "Offset typeid:      %ld", (char*)&a.typeid - (char*)&a);
  h7_trace("platform", "Offset u.v_obj:     %ld", (char*)&a.u.v_obj - (char*)&a);
#endif

    h7_type_init();
    h7_methods_init();
    h7_keywords_init();
  }
}
