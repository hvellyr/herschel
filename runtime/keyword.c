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

#include "runtime/rt.h"
#include "runtime/hash.h"
#include "runtime/trace.h"

static HashTable* keyword_table = NULL; /* hash<char*, Keyword*> */


void
keywords_init()
{
  keyword_table = hashtable_alloc(27,
                                  hashtable_cstr_func,
                                  hashtable_cstr_cmp_func);
}


/* ------------------------------------------------------------------------
   keywords
   ------------------------------------------------------------------------ */

void* keyword_register(const char* keyw_str)
{
  Keyword* key = (Keyword*)hashtable_get(keyword_table, (void*)keyw_str);
  if (key == NULL) {
    key = malloc(sizeof(Keyword));
    key->name = keyw_str;

    hashtable_add(keyword_table, (void*)keyw_str, (void*)key);

#if defined(UNITTESTS)
  hr_trace("register", "Register keyword: %s [id %p]", keyw_str, key);
#endif
  }

  return key;
}


