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

void allocate_string(ATOM* instance, const char* str)
{
  size_t items = 0;

#if defined(UNITTESTS)
  hr_trace("allocate", "Create instance of type 'lang|String': '%s'", str);
#endif

  items = strlen(str);

  instance->typeid = TYPE_TAG_STRING;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(char) * 3);

  *((size_t*)instance->u.v_obj) = items;
  {
    char* p = (char*)(instance->u.v_obj + sizeof(size_t));
    memset(p, 0, items * sizeof(char) * 3);

    size_t i;
    for (i = 0; i < items; i++, p += 3)
      *p = str[i];
  }
}


int lang_string_equal(struct ATOM atom0, struct ATOM atom1)
{
  if (atom0.typeid == atom1.typeid && atom1.typeid == TYPE_TAG_STRING) {
    size_t items0 = *((size_t*)atom0.u.v_obj);
    size_t items1 = *((size_t*)atom1.u.v_obj);
    char* p0 = (char*)(atom0.u.v_obj + sizeof(size_t));
    char* p1 = (char*)(atom1.u.v_obj + sizeof(size_t));
    size_t i = 0;

    if (items0 != items1)
      return 1;

    for ( ; i < items0 * 3; ++i, p0++, p1++) {
      if (*p0 != *p1)
        return 1;
    }

    return 0;
  }

  return 1;
}
