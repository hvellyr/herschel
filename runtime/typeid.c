/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#include "runtime/rt.h"
#include "runtime/hash.h"
#include "runtime/trace.h"



static HashTable* types_name_to_type_map = NULL; /* hash<char*, Type*> */
/*static HashTable* types_tag_to_type_map = NULL;*/  /* hash<int, Type*> */
static int type_tag_id_counter = 127;

static Type** types_tag_vector = NULL;
static size_t types_tag_size = 0;


void
type_init()
{
  types_name_to_type_map = hashtable_alloc(27);
  /* types_tag_to_type_map = hashtable_alloc(27); */

  types_tag_size = 256;
  types_tag_vector = malloc(types_tag_size * sizeof(Type*));
  memset(types_tag_vector, 0, types_tag_size * sizeof(Type*));
}


int
tag_id_for_type(const Type* type)
{
  if (strcmp(type->name, "__QN4lang5Int32") == 0)
    return (int)'i';
  else if (strcmp(type->name, "__QN4lang4Bool") == 0)
    return (int)'b';
  else if (strcmp(type->name, "__QN4lang4Char") == 0)
    return (int)'c';
  else if (strcmp(type->name, "__QN4lang3Any") == 0)
    return (int)'A';

  type_tag_id_counter++;
  return type_tag_id_counter;
}


void
register_type(Type* type)
{
  type->tag_id = tag_id_for_type(type);

  if (hashtable_get(types_name_to_type_map, (void*)type->name) != NULL)
  {
    fprintf(stderr, "error: Multiple occurance of type '%s' clash.\n",
            type->name);
    return;
  }

  hashtable_add(types_name_to_type_map, (void*)type->name, (void*)type);
  /* hashtable_add(types_tag_to_type_map, (void*)type->tag_id, (void*)type); */

  if (type->tag_id >= types_tag_size) {
    types_tag_size *= 2;
    types_tag_vector = realloc(types_tag_vector, types_tag_size * sizeof(Type*));
  }
  types_tag_vector[type->tag_id] = type;

#if defined(UNITTESTS)
  hr_trace("register", "Register type: %s", type->name);
#endif
}


void
type_setup_dispatch_table(Type* ty, va_list vp)
{
  size_t i = 0;

  ty->isa = malloc(ty->isa_size * sizeof(Type*));
  ty->isa_set = hashtable_alloc(11);

  for (i = 0; i < ty->isa_size; i++) {
    Type* isa = va_arg(vp, Type*);
    if (isa != NULL) {
      ty->isa[i] = isa;

      hashtable_add(ty->isa_set, (void*)isa, (void*)isa);
      if (isa->isa_set != NULL)
        hashtable_add_all(ty->isa_set, isa->isa_set);
    }
  }
}


static Type*
type_base_alloc(const char* nm, size_t isa_size,
                size_t instance_size,
                const TypeSlotPair* slots)
{
  Type* ty = malloc(sizeof(Type));

  ty->name = nm;
  ty->isa_size = isa_size;
  ty->tag_id = 0;
  ty->isa_set = NULL;
  ty->instance_size = instance_size; /* 0 = not allocatable */
  ty->slots_offsets = NULL;
  ty->slots = slots;

  return ty;
}


Type*
type_alloc(const char* nm, size_t isa_size, ...)
{
  Type* ty = type_base_alloc(nm, isa_size, 0, NULL);

  if (isa_size > 0) {
    va_list vp;
    va_start(vp, isa_size);

    type_setup_dispatch_table(ty, vp);

    va_end(vp);
  }
  else
    ty->isa = NULL;

  return ty;
}


Type*
class_alloc(const char* nm,
            size_t instance_size,
            const TypeSlotPair* slots,
            size_t isa_size, ...)
{
  Type* ty = type_base_alloc(nm, isa_size, instance_size, slots);

  if (isa_size > 0) {
    va_list vp;
    va_start(vp, isa_size);

    type_setup_dispatch_table(ty, vp);

    va_end(vp);
  }
  else
    ty->isa = NULL;

  if (slots != NULL && slots->name != NULL)
  {
    ty->slots_offsets = hashtable_alloc(11);

    const TypeSlotPair* p = slots;
    for ( ; p->name != NULL; p++) {
      hashtable_add(ty->slots_offsets, (void*)p->name, (void*)p->offset);
    }
  }

  return ty;
}


Type*
type_lookup_by_name(const char* nm)
{
  Type* ty = hashtable_get(types_name_to_type_map, (void*)nm);
  if (ty == NULL)
    fprintf(stderr, "error: Type '%s' could not be resolved\n", nm);
  return ty;
}


Type*
type_lookup_by_tag(int tag_id)
{
  /* return hashtable_get(types_tag_to_type_map, (void*)tag_id); */
  return types_tag_vector[tag_id];
}


int
type_isa(Type* one, Type* two)
{
  if (one == two)
    return 1;

  if (one->isa_set != NULL) {
    /* fprintf(stderr, "Lookup %s in %s\n", one->name, two->name); */
    return hashtable_get(one->isa_set, two) != NULL;
  }

  return 0;
}


size_t
type_slot_get(Type* ty, const char* slot_name)
{
  if (ty->slots_offsets != NULL) {
    /* don't check whether the slot really exists.  This has to be done at
       compile time (realy?) */
    return (size_t)hashtable_get(ty->slots_offsets, (void*)slot_name);
  }

  return 0;
}


/* ---------------------------------------------------------------------------
   ------------------------------------------------------------------------ */

void
allocate(ATOM* instance, Type* ty)
{
#if defined(UNITTESTS)
  hr_trace("allocate", "Create instance of type '%s', size: %ld",
           ty->name, ty->instance_size);
#endif

  instance->typeid = ty->tag_id;
  if (ty->instance_size > 0) {
    instance->u.v_obj = malloc(ty->instance_size);
    memset(instance->u.v_obj, 0, ty->instance_size);
  }
  else
    instance->u.v_obj = NULL;
}


ATOM
allocate_array(Type* ty, ATOM init_value, size_t items)
{
  ATOM obj;
  obj.typeid = ty->tag_id;
  obj.u.v_obj = malloc(sizeof(size_t) + items * sizeof(ATOM));
  *((size_t*)obj.u.v_obj) = items;

  {
    ATOM* p = (ATOM*)(obj.u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }

  return obj;
}


ATOM
allocate_char_array(Type* ty, char init_value, size_t items)
{
  ATOM obj;
  obj.typeid = ty->tag_id;
  obj.u.v_obj = malloc(sizeof(size_t) + items * sizeof(char));
  *((size_t*)obj.u.v_obj) = items;

  {
    char* p = (char*)(obj.u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }

  return obj;
}


ATOM
allocate_short_array(Type* ty, short init_value, size_t items)
{
  ATOM obj;
  obj.typeid = ty->tag_id;
  obj.u.v_obj = malloc(sizeof(size_t) + items * sizeof(short));
  *((size_t*)obj.u.v_obj) = items;

  {
    short* p = (short*)(obj.u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }

  return obj;
}


ATOM
allocate_int_array(Type* ty, int init_value, size_t items)
{
  ATOM obj;
  obj.typeid = ty->tag_id;
  obj.u.v_obj = malloc(sizeof(size_t) + items * sizeof(int));
  *((size_t*)obj.u.v_obj) = items;

  {
    int* p = (int*)(obj.u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }

  return obj;
}


ATOM
allocate_float_array(Type* ty, float init_value, size_t items)
{
  ATOM obj;
  obj.typeid = ty->tag_id;
  obj.u.v_obj = malloc(sizeof(size_t) + items * sizeof(float));
  *((size_t*)obj.u.v_obj) = items;

  {
    float* p = (float*)(obj.u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }

  return obj;
}


ATOM
allocate_double_array(Type* ty, double init_value, size_t items)
{
  ATOM obj;
  obj.typeid = ty->tag_id;
  obj.u.v_obj = malloc(sizeof(size_t) + items * sizeof(double));
  *((size_t*)obj.u.v_obj) = items;

  {
    double* p = (double*)(obj.u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }

  return obj;
}




