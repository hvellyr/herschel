/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include <stdarg.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "runtime/rt.h"
#include "runtime/trace.h"
#include "runtime/typeid.h"


/* ------------------------------------------------------------------------
   strings
   ------------------------------------------------------------------------ */

typedef unsigned short Ucs2Char;


void allocate_string(ATOM* instance, const char* str)
{
  size_t items = 0;

#if defined(UNITTESTS)
  hr_trace("allocate", "Create instance of type 'lang|String': '%s'", str);
#endif

  items = strlen(str);

  instance->typeid = TYPE_TAG_STRING;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(Ucs2Char));

  *((size_t*)instance->u.v_obj) = items;
  {
    Ucs2Char* p = (Ucs2Char*)(instance->u.v_obj + sizeof(size_t));
    memset(p, 0, items * sizeof(Ucs2Char));

    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = str[i];
  }
}


int lang_string_compare(struct ATOM atom0, struct ATOM atom1)
{
  if (atom0.typeid == atom1.typeid && atom1.typeid == TYPE_TAG_STRING) {
    size_t items0 = *((size_t*)atom0.u.v_obj);
    size_t items1 = *((size_t*)atom1.u.v_obj);
    Ucs2Char* p0 = (Ucs2Char*)(atom0.u.v_obj + sizeof(size_t));
    Ucs2Char* p1 = (Ucs2Char*)(atom1.u.v_obj + sizeof(size_t));
    size_t i = 0;
    size_t n = items0 > items1 ? items1 : items0;

    while (i++ < n) {
      Ucs2Char uc0 = *p0++;
      Ucs2Char uc1 = *p1++;

      if (uc0 != uc1)
        return uc0 - uc1;
    }

    /* if we are here the two strings share a common prefix */
    if (items0 < items1)
      return -1;
    else if (items0 > items1)
      return 1;

    return 0;
  }

  return -1;
}


unsigned int lang_string_length(struct ATOM atom0)
{
  if (atom0.typeid == TYPE_TAG_STRING) {
    return *((size_t*)atom0.u.v_obj);
  }

  return 0;
}

unsigned int lang_string_char_at_index(struct ATOM atom0,
                                       unsigned int idx)
{
  if (atom0.typeid == TYPE_TAG_STRING) {
    size_t len = *((size_t*)atom0.u.v_obj);
    if (idx < len) {
      Ucs2Char* p0 = (Ucs2Char*)(atom0.u.v_obj + sizeof(size_t));
      return p0[idx];
    }
  }

  return 0;
}
