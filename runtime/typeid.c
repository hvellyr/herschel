/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include <stdarg.h>

#include "runtime/rt.h"
#include "runtime/hash.h"



static HashTable* types_name_to_type_map = NULL; /* hash<char*, Type*> */
static HashTable* types_tag_to_type_map = NULL;  /* hash<int, Type*> */
static int type_tag_id_counter = 0;

void
type_init()
{
  types_name_to_type_map = hashtable_alloc(27);
  types_tag_to_type_map = hashtable_alloc(27);
}


void
register_type(Type* type)
{
  type_tag_id_counter++;
  type->tag_id = type_tag_id_counter;

  hashtable_add(types_name_to_type_map, (void*)type->name, (void*)type);
  hashtable_add(types_tag_to_type_map, (void*)type->tag_id, (void*)type);
}


Type*
type_alloc(const char* nm, size_t isa_size, ...)
{
  Type* ty = malloc(sizeof(Type));

  ty->name = nm;
  ty->isa_size = isa_size;
  ty->tag_id = 0;
  ty->isa_set = NULL;

  if (isa_size > 0) {
    va_list vp;
    size_t i = 0;

    ty->isa = malloc(isa_size * sizeof(Type*));
    ty->isa_set = hashtable_alloc(11);

    va_start(vp, isa_size);
    for (i = 0; i < isa_size; i++) {
      Type* isa = va_arg(vp, Type*);
      ty->isa[i] = isa;

      hashtable_add(ty->isa_set, (void*)isa, (void*)isa);
      if (isa->isa_set != NULL)
        hashtable_add_all(ty->isa_set, isa->isa_set);
    }
    va_end(vp);
  }
  else
    ty->isa = NULL;

  return ty;
}


Type*
type_lookup_by_name(const char* nm)
{
  return hashtable_get(types_name_to_type_map, (void*)nm);
}


Type*
type_lookup_by_tag(int tag_id)
{
  return hashtable_get(types_name_to_type_map, (void*)tag_id);
}


int
type_isa(Type* one, Type* two)
{
  if (one == two)
    return 1;

  if (one->isa_set != NULL)
    return hashtable_get(one->isa_set, two) != NULL;

  return 0;
}


