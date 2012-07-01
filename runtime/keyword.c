/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include <stdarg.h>
#include <assert.h>
#include <stdio.h>

#include "runtime/herschel.h"
#include "runtime/hash.h"
#include "runtime/trace.h"
#include "runtime/keyword.h"

static H7_HashTable* keyword_table = NULL; /* hash<char*, Keyword*> */


void
h7_keywords_init()
{
  keyword_table = h7_hashtable_alloc(27,
                                     h7_hashtable_cstr_func,
                                     h7_hashtable_cstr_cmp_func);
}


/* ------------------------------------------------------------------------
   keywords
   ------------------------------------------------------------------------ */

void*
h7_keyword_register(const char* keyw_str)
{
  H7_Keyword* key = (H7_Keyword*)h7_hashtable_get(keyword_table,
                                                  (void*)keyw_str);
  if (key == NULL) {
    key = malloc(sizeof(H7_Keyword));
    key->name = keyw_str;

    h7_hashtable_add(keyword_table, (void*)keyw_str, (void*)key);

#if defined(UNITTESTS)
  h7_trace("register", "Register keyword: %s [id %p]", keyw_str, key);
#endif
  }

  return key;
}


