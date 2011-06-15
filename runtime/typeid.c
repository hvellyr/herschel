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


#define TYPE_TAG_ANY     (int)'a'
#define TYPE_TAG_BOOL    (int)'b'
#define TYPE_TAG_CHAR    (int)'c'
#define TYPE_TAG_INT32   (int)'i'
#define TYPE_TAG_KEYW    (int)'k'

#define TYPE_TAG_ANY_ARRAY     (int)'A'
#define TYPE_TAG_BOOL_ARRAY    (int)'B'
#define TYPE_TAG_CHAR_ARRAY    (int)'C'
#define TYPE_TAG_INT32_ARRAY   (int)'I'
#define TYPE_TAG_KEYW_ARRAY    (int)'K'


static HashTable* types_name_to_type_map = NULL; /* hash<char*, Type*> */
/*static HashTable* types_tag_to_type_map = NULL;*/  /* hash<int, Type*> */
static int type_tag_id_counter = 127;

static Type** types_tag_vector = NULL;
static size_t types_tag_size = 0;


void
type_init()
{
  types_name_to_type_map = hashtable_alloc(27,
                                           hashtable_cstr_func,
                                           hashtable_cstr_cmp_func);
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
    return (int)'a';
  else if (strcmp(type->name, "__QN4lang7Keyword") == 0)
    return (int)'k';

  else if (strcmp(type->name, "__QN4lang5Int32[]") == 0)
    return (int)'I';
  else if (strcmp(type->name, "__QN4lang4Bool[]") == 0)
    return (int)'B';
  else if (strcmp(type->name, "__QN4lang4Char[]") == 0)
    return (int)'C';
  else if (strcmp(type->name, "__QN4lang3Any[]") == 0)
    return (int)'A';
  else if (strcmp(type->name, "__QN4lang7Keyword[]") == 0)
    return (int)'K';

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


size_t
type_setup_dispatch_table(Type* ty, va_list vp)
{
  size_t acc_size = 0;
  size_t i = 0;

  ty->isa = malloc(ty->isa_size * sizeof(Type*));
  ty->isa_set = hashtable_alloc(11, hashtable_sizet_func, hashtable_sizet_cmp_func);

  for (i = 0; i < ty->isa_size; i++) {
    Type* isa = va_arg(vp, Type*);
    if (isa != NULL) {
      ty->isa[i] = isa;

      hashtable_add(ty->isa_set, (void*)isa, (void*)isa);
      if (isa->isa_set != NULL)
        hashtable_add_all(ty->isa_set, isa->isa_set);

      acc_size += isa->acc_instance_size;
    }
  }

  return acc_size;
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
  ty->instance_size = instance_size;
  ty->acc_instance_size = 0;    /* to be set later */
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


void
class_add_super_slots_and_rewrite_offsets(HashTable* table, HashTable* other,
                                          size_t base_offset)
{
  size_t i;

  for (i = 0; i < other->fSize; i++) {
    HashNode* node = other->fNodes[i];

    while (node != NULL) {
      size_t offset = (size_t)node->fValue + base_offset;
      hashtable_add(table, node->fKey, (void*)offset);
      node = node->fTail;
    }
  }
}


Type*
class_alloc(const char* nm,
            size_t instance_size,
            const TypeSlotPair* slots,
            size_t isa_size, ...)
{
  Type* ty = type_base_alloc(nm, isa_size, instance_size, slots);

#if defined(UNITTESTS)
  hr_trace("register", "Alloc class '%s' [instance-size=%d]",
           nm, instance_size);
#endif

  size_t acc_super_size = 0;
  if (isa_size > 0) {
    va_list vp;
    va_start(vp, isa_size);

    acc_super_size = type_setup_dispatch_table(ty, vp);

    va_end(vp);
  }
  else
    ty->isa = NULL;

  ty->acc_instance_size = ty->instance_size + acc_super_size;

#if defined(UNITTESTS)
  hr_trace("register", "            [acc_instance-size=%ld, super=%ld]",
           ty->acc_instance_size, acc_super_size);
#endif

  ty->slots_offsets = hashtable_alloc(11,
                                      hashtable_cstr_func,
                                      hashtable_cstr_cmp_func);

  size_t acc_offset = 0;
  if (ty->isa_size > 0) {
    size_t i;
    for (i = 0; i < ty->isa_size; i++) {
      if (ty->isa[i]->slots_offsets != NULL) {
        class_add_super_slots_and_rewrite_offsets(ty->slots_offsets,
                                                  ty->isa[i]->slots_offsets,
                                                  acc_offset);
        acc_offset += ty->isa[i]->acc_instance_size;
      }
    }
  }

  if (slots != NULL && slots->name != NULL)
  {
    const TypeSlotPair* p = slots;
    for ( ; p->name != NULL; p++) {
#if defined(UNITTESTS)
      hr_trace("register", "Register slot '%s' with offset '%d'",
               p->name, p->offset);
#endif
      size_t offset = acc_offset + p->offset;
      hashtable_add(ty->slots_offsets, (void*)p->name, (void*)offset);
    }
  }

#if defined(UNITTESTS)
  {
    HashNode* node;
    size_t i;

    for (i = 0; i < ty->slots_offsets->fSize; i++) {
      node = ty->slots_offsets->fNodes[i];
      while (node != NULL) {
        hr_trace("instancelayout", "Slot offset: '%s::%s' @ '%ld'",
                 nm, (char*)node->fKey, (size_t)node->fValue);
        node = node->fTail;
      }
    }
  }
#endif

  // TODO: shouldn't the class slot table be extended by the slots of the
  // super classes?  It seems that slot() and slot!() are not really private
  // to the class at all; so from the outside it is otherwise rather diffcult
  // to find the right class to ask for the offset of the member.
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


/* ---------------------------------------------------------------------------
   access to instance slots
   ------------------------------------------------------------------------ */

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


void*
instance_slot(ATOM instance, const Keyword* keyw)
{
  const char* slot_name = keyw->name;
  Type* ty = type_lookup_by_tag(instance.typeid);

#if defined(UNITTESTS)
  hr_trace("slot", "Lookup slot '%s' for instance of type '%s'",
           slot_name, ty->name);
#endif

  if (ty->slots_offsets != NULL) {
    HashNode* node = hashtable_get_impl(ty->slots_offsets, (void*)slot_name);
    if (node != NULL)
      return (void*)((unsigned char*)instance.u.v_obj + (size_t)node->fValue);
  }

  fprintf(stderr, "No slot '%s' defined. Abort\n", slot_name);
  exit(1);

  return NULL;
}


/* ---------------------------------------------------------------------------
   allocating instances
   ------------------------------------------------------------------------ */

void
allocate(ATOM* instance, Type* ty)
{
#if defined(UNITTESTS)
  hr_trace("allocate", "Create instance of type '%s', size: %ld",
           ty->name, ty->acc_instance_size);
#endif

  instance->typeid = ty->tag_id;
  if (ty->acc_instance_size > 0) {
    instance->u.v_obj = malloc(ty->acc_instance_size);
    memset(instance->u.v_obj, 0, ty->acc_instance_size);
  }
  else
    instance->u.v_obj = NULL;
}


void
allocate_array(ATOM* instance, Type* ty, ATOM init_value, size_t items)
{
#if defined(UNITTESTS)
  hr_trace("allocate", "Create instance of type '%s'x%ld, size: %ld",
           ty->name, items, ty->acc_instance_size);
#endif

  instance->typeid = ty->tag_id;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(ATOM));
  *((size_t*)instance->u.v_obj) = items;

  {
    ATOM* p = (ATOM*)(instance->u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }
}


void
allocate_int32_array(ATOM* instance, TypeTag tag_id, int init_value, size_t items)
{
#if defined(UNITTESTS)
  hr_trace("allocate", "Create instance of type 'lang|Int32'x%ld (%d), size: %ld a %ld",
           items, tag_id, sizeof(size_t) + items * sizeof(int), sizeof(int));
#endif

  instance->typeid = tag_id;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(int));
  *((size_t*)instance->u.v_obj) = items;

  {
    int* p = (int*)(instance->u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }
}


void
allocate_char_array(ATOM* instance, Type* ty, char init_value, size_t items)
{
  instance->typeid = ty->tag_id;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(char));
  *((size_t*)instance->u.v_obj) = items;

  {
    char* p = (char*)(instance->u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }
}


void
allocate_short_array(ATOM* instance, Type* ty, short init_value, size_t items)
{
  instance->typeid = ty->tag_id;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(short));
  *((size_t*)instance->u.v_obj) = items;

  {
    short* p = (short*)(instance->u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }
}


void
allocate_float_array(ATOM* instance, Type* ty, float init_value, size_t items)
{
  instance->typeid = ty->tag_id;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(float));
  *((size_t*)instance->u.v_obj) = items;

  {
    float* p = (float*)(instance->u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }
}


void
allocate_double_array(ATOM* instance, Type* ty, double init_value, size_t items)
{
  instance->typeid = ty->tag_id;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(double));
  *((size_t*)instance->u.v_obj) = items;

  {
    double* p = (double*)(instance->u.v_obj + sizeof(size_t));
    size_t i;
    for (i = 0; i < items; i++, p++)
      *p = init_value;
  }
}


