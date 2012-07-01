/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#include "runtime/hash.h"
#include "runtime/herschel.h"
#include "runtime/trace.h"
#include "runtime/typeid.h"


static H7_HashTable* types_name_to_type_map = NULL; /* hash<char*, Type*> */
/*static H7_HashTable* types_tag_to_type_map = NULL;*/  /* hash<int, Type*> */
static int type_tag_id_counter = 127;

static H7_Type** types_tag_vector = NULL;
static size_t types_tag_size = 0;

static size_t arraytype_reference_size = 0;
static H7_TypeTag* arraytype_reference = NULL;


void
h7_type_init()
{
  types_name_to_type_map = h7_hashtable_alloc(27,
                                              h7_hashtable_cstr_func,
                                              h7_hashtable_cstr_cmp_func);
  /* types_tag_to_type_map = h7_hashtable_alloc(27); */

  types_tag_size = 256;
  types_tag_vector = malloc(types_tag_size * sizeof(H7_Type*));
  memset(types_tag_vector, 0, types_tag_size * sizeof(H7_Type*));

  arraytype_reference_size = 256;
  arraytype_reference = malloc(arraytype_reference_size * sizeof(H7_TypeTag));
  memset(arraytype_reference, 0,
         arraytype_reference_size * sizeof(H7_TypeTag));
}


static int
h7_tag_id_for_type(const H7_Type* type)
{
  /* int is_array = type->is_array; */
  /* printf("Typeid: %s\n", type->name); */

  if (strcmp(type->name, "__QN4lang5Int32") == 0)
    return TYPE_TAG_INT32;
  else if (strcmp(type->name, "__QN4lang6UInt32") == 0)
    return TYPE_TAG_UINT32;
  else if (strcmp(type->name, "__QN4lang4Bool") == 0)
    return TYPE_TAG_BOOL;
  else if (strcmp(type->name, "__QN4lang4Char") == 0)
    return TYPE_TAG_CHAR;
  else if (strcmp(type->name, "__QN4lang3Any") == 0)
    return TYPE_TAG_ANY;
  else if (strcmp(type->name, "__QN4lang7Keyword") == 0)
    return TYPE_TAG_KEYW;
  else if (strcmp(type->name, "__QN4lang5Int16") == 0)
    return TYPE_TAG_INT16;
  else if (strcmp(type->name, "__QN4lang6UInt16") == 0)
    return TYPE_TAG_UINT16;
  else if (strcmp(type->name, "__QN4lang4Int8") == 0)
    return TYPE_TAG_INT8;
  else if (strcmp(type->name, "__QN4lang5UInt8") == 0)
    return TYPE_TAG_UINT8;
  else if (strcmp(type->name, "__QN4lang5Int64") == 0)
    return TYPE_TAG_INT64;
  else if (strcmp(type->name, "__QN4lang6UInt64") == 0)
    return TYPE_TAG_UINT64;

  else if (strcmp(type->name, "__QN4lang7Float32") == 0)
    return TYPE_TAG_FLOAT32;
  else if (strcmp(type->name, "__QN4lang7Float64") == 0)
    return TYPE_TAG_FLOAT64;

  else if (strcmp(type->name, "__QN4lang10String") == 0)
    return TYPE_TAG_STRING;
  else if (strcmp(type->name, "__QN4lang10StringImpl") == 0)
    return TYPE_TAG_STRINGIMPL;

  if (strcmp(type->name, "__QN4lang5Int32[]") == 0)
    return TYPE_TAG_INT32 + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang6UInt32[]") == 0)
    return TYPE_TAG_UINT32 + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang4Bool[]") == 0)
    return TYPE_TAG_BOOL + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang4Char[]") == 0)
    return TYPE_TAG_CHAR + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang3Any[]") == 0)
    return TYPE_TAG_ANY + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang7Keyword[]") == 0)
    return TYPE_TAG_KEYW + TYPE_ARRAY_OFFSET;

  else if (strcmp(type->name, "__QN4lang5Int16[]") == 0)
    return TYPE_TAG_INT16 + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang6UInt16[]") == 0)
    return TYPE_TAG_UINT16 + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang4Int8[]") == 0)
    return TYPE_TAG_INT8 + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang5UInt8[]") == 0)
    return TYPE_TAG_UINT8 + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang5Int64[]") == 0)
    return TYPE_TAG_INT64 + TYPE_ARRAY_OFFSET;
  else if (strcmp(type->name, "__QN4lang6UInt64[]") == 0)
    return TYPE_TAG_UINT64 + TYPE_ARRAY_OFFSET;

  else if (strcmp(type->name, "__QN4lang7Float32[]") == 0)
    return TYPE_TAG_FLOAT32 + TYPE_ARRAY_OFFSET;

  type_tag_id_counter++;
  return type_tag_id_counter;
}


void
h7_register_type(H7_Type* type)
{
  type->tag_id = h7_tag_id_for_type(type);

  if (h7_hashtable_get(types_name_to_type_map, (void*)type->name) != NULL)
  {
    fprintf(stderr, "error: Multiple occurance of type '%s' clash.\n",
            type->name);
    return;
  }

  h7_hashtable_add(types_name_to_type_map, (void*)type->name, (void*)type);
  /* h7_hashtable_add(types_tag_to_type_map, (void*)type->tag_id, (void*)type); */

  if (type->tag_id >= types_tag_size) {
    types_tag_size *= 2;
    types_tag_vector = realloc(types_tag_vector, types_tag_size * sizeof(H7_Type*));
  }
  types_tag_vector[type->tag_id] = type;

#if defined(UNITTESTS)
  h7_trace("register", "Register type: %s", type->name);
#endif
}


static size_t
h7_type_setup_dispatch_table(H7_Type* ty, va_list vp)
{
  size_t acc_size = 0;
  size_t i = 0;

  ty->isa = malloc(ty->isa_size * sizeof(H7_Type*));
  ty->isa_set = h7_hashtable_alloc(11,
                                   h7_hashtable_sizet_func,
                                   h7_hashtable_sizet_cmp_func);

  for (i = 0; i < ty->isa_size; i++) {
    H7_Type* isa = va_arg(vp, H7_Type*);
    if (isa != NULL) {
      ty->isa[i] = isa;

      h7_hashtable_add(ty->isa_set, (void*)isa, (void*)isa);
      if (isa->isa_set != NULL)
        h7_hashtable_add_all(ty->isa_set, isa->isa_set);

      acc_size += isa->acc_instance_size;
    }
  }

  return acc_size;
}


static H7_Type*
h7_type_base_alloc(const char* nm, size_t isa_size,
                   size_t instance_size,
                   const H7_TypeSlotPair* slots,
                   int is_array)
{
  H7_Type* ty = malloc(sizeof(H7_Type));

  ty->name = nm;
  ty->isa_size = isa_size;
  ty->isa = NULL;
  ty->tag_id = 0;
  ty->isa_set = NULL;
  ty->instance_size = instance_size;
  ty->acc_instance_size = 0;    /* to be set later */
  ty->slots_offsets = NULL;
  ty->slots = slots;
  ty->is_array = is_array;

  return ty;
}


H7_Type*
h7_type_alloc(const char* nm, size_t isa_size, ...)
{
  H7_Type* ty = h7_type_base_alloc(nm, isa_size, 0, NULL, 0);

  if (isa_size > 0) {
    va_list vp;
    va_start(vp, isa_size);

    h7_type_setup_dispatch_table(ty, vp);

    va_end(vp);
  }

  return ty;
}


static H7_Type*
h7_type_clone_with_other_name(const H7_Type* base_type,
                              const char* full_tag_name)
{
  H7_Type* ty = h7_type_base_alloc(full_tag_name,
                                   base_type->isa_size,
                                   0, NULL, 0);

  ty->isa               = base_type->isa;               /* shared ptr */
  ty->isa_set           = base_type->isa_set;           /* shared ptr */
  ty->instance_size     = base_type->instance_size;
  ty->acc_instance_size = base_type->acc_instance_size;
  ty->slots_offsets     = base_type->slots_offsets;
  ty->slots             = base_type->slots;             /* shared ptr */
  ty->is_array          = base_type->is_array;

  return ty;
}


static void
h7_class_add_super_slots_and_rewrite_offsets(H7_HashTable* table,
                                             H7_HashTable* other,
                                             size_t base_offset)
{
  size_t i;

  for (i = 0; i < other->fSize; i++) {
    H7_HashNode* node = other->fNodes[i];

    while (node != NULL) {
      size_t offset = (size_t)node->fValue + base_offset;
      h7_hashtable_add(table, node->fKey, (void*)offset);
      node = node->fTail;
    }
  }
}


H7_Type*
h7_class_alloc(const char* nm,
               size_t instance_size,
               const H7_TypeSlotPair* slots,
               size_t isa_size, ...)
{
  H7_Type* ty = h7_type_base_alloc(nm,
                                   isa_size,
                                   instance_size,
                                   slots,
                                   0);

#if defined(UNITTESTS)
  h7_trace("register", "Alloc class '%s' [instance-size=%d]",
           nm, instance_size);
#endif

  size_t acc_super_size = 0;
  if (isa_size > 0) {
    va_list vp;
    va_start(vp, isa_size);

    acc_super_size = h7_type_setup_dispatch_table(ty, vp);

    va_end(vp);
  }

  ty->acc_instance_size = ty->instance_size + acc_super_size;

#if defined(UNITTESTS)
  h7_trace("register", "            [acc_instance-size=%ld, super=%ld]",
           ty->acc_instance_size, acc_super_size);
#endif

  ty->slots_offsets = h7_hashtable_alloc(11,
                                         h7_hashtable_cstr_func,
                                         h7_hashtable_cstr_cmp_func);

  size_t acc_offset = 0;
  if (ty->isa_size > 0) {
    size_t i;
    for (i = 0; i < ty->isa_size; i++) {
      if (ty->isa[i]->slots_offsets != NULL) {
        h7_class_add_super_slots_and_rewrite_offsets(ty->slots_offsets,
                                                     ty->isa[i]->slots_offsets,
                                                     acc_offset);
        acc_offset += ty->isa[i]->acc_instance_size;
      }
    }
  }

  if (slots != NULL && slots->name != NULL)
  {
    const H7_TypeSlotPair* p = slots;
    for ( ; p->name != NULL; p++) {
#if defined(UNITTESTS)
      h7_trace("register", "Register slot '%s' with offset '%d'",
               p->name, p->offset);
#endif
      size_t offset = acc_offset + p->offset;
      h7_hashtable_add(ty->slots_offsets, (void*)p->name, (void*)offset);
    }
  }

#if defined(UNITTESTS)
  {
    H7_HashNode* node;
    size_t i;

    for (i = 0; i < ty->slots_offsets->fSize; i++) {
      node = ty->slots_offsets->fNodes[i];
      while (node != NULL) {
        h7_trace("instancelayout", "Slot offset: '%s::%s' @ '%ld'",
                 nm, (char*)node->fKey, (size_t)node->fValue);
        node = node->fTail;
      }
    }
  }
#endif

  return ty;
}


H7_Type*
h7_type_lookup_by_name(const char* nm)
{
  H7_Type* ty = h7_hashtable_get(types_name_to_type_map, (void*)nm);
  if (ty == NULL)
    fprintf(stderr, "error: Type '%s' could not be resolved\n", nm);
  return ty;
}


H7_Type*
h7_type_lookup_by_tag(int tag_id)
{
  /* return h7_hashtable_get(types_tag_to_type_map, (void*)tag_id); */
  return types_tag_vector[tag_id];
}


int
h7_type_isa(H7_Type* one, H7_Type* two)
{
  if (one == two)
    return 1;

  if (one->isa_set != NULL) {
    /* fprintf(stderr, "Lookup %s in %s\n", one->name, two->name); */
    return h7_hashtable_get(one->isa_set, two) != NULL;
  }

  return 0;
}


/* ---------------------------------------------------------------------------
   special array type support
   ------------------------------------------------------------------------ */

H7_Type*
h7_type_lookup_array_type(H7_Type* base_type)
{
  if (base_type->tag_id >= arraytype_reference_size) {
    int old_array_size = arraytype_reference_size;

    arraytype_reference_size *= 2;
    arraytype_reference = realloc(arraytype_reference,
                                  arraytype_reference_size * sizeof(H7_TypeTag));

    memset(arraytype_reference + old_array_size, 0,
           (arraytype_reference_size - old_array_size) * sizeof(H7_TypeTag));
  }

  H7_TypeTag typeTag = arraytype_reference[base_type->tag_id];
  if (typeTag == 0) {
    // HACK: make the array name heap allocated.  This leaks ...
    char tmp[256];
    sprintf(tmp, "%s[]", base_type->name);

    H7_Type* array_type = h7_type_base_alloc(strdup(tmp),
                                             0,    /* isa-size */
                                             0,    /* instance-size */
                                             NULL, /* slots */
                                             1);   /* is_array */
#if defined(UNITTESTS)
  h7_trace("register", "Allocate array type for %s [tag: %ld]",
           base_type->name, array_type->tag_id);
#endif

    h7_register_type(array_type);
    arraytype_reference[base_type->tag_id] = array_type->tag_id;

    return array_type;
  }

  return types_tag_vector[typeTag];
}


/* ---------------------------------------------------------------------------
   access to instance slots
   ------------------------------------------------------------------------ */

size_t
h7_type_slot_get(H7_Type* ty, const char* slot_name)
{
  if (ty->slots_offsets != NULL) {
    /* don't check whether the slot really exists.  This has to be done at
       compile time (realy?) */
    return (size_t)h7_hashtable_get(ty->slots_offsets, (void*)slot_name);
  }

  return 0;
}


void*
h7_instance_slot(H7_ATOM instance, const H7_Keyword* keyw)
{
  const char* slot_name = keyw->name;
  H7_Type* ty = h7_type_lookup_by_tag(instance.typeid);

#if defined(UNITTESTS)
  h7_trace("slot", "Lookup slot '%s' for instance of type '%s'",
           slot_name, ty->name);
#endif

  if (ty->slots_offsets != NULL) {
    H7_HashNode* node = h7_hashtable_get_impl(ty->slots_offsets, (void*)slot_name);
    if (node != NULL)
      return (void*)((unsigned char*)instance.u.v_obj + (size_t)node->fValue);
  }

  fprintf(stderr, "No slot '%s' defined. Abort\n", slot_name);
  exit(1);

  return NULL;
}


/* ---------------------------------------------------------------------------
   check instances for type
   ------------------------------------------------------------------------ */

int
h7_instance_isa(H7_ATOM instance, H7_Type* ty)
{
#if defined(UNITTESTS)
  h7_trace("isa", "isa '%s'?", ty->name);
#endif

  H7_Type* instty = h7_type_lookup_by_tag(instance.typeid);

  return h7_type_isa(instty, ty);
}


/* ---------------------------------------------------------------------------
   allocating instances
   ------------------------------------------------------------------------ */

void
h7_allocate(H7_ATOM* instance, H7_Type* ty)
{
#if defined(UNITTESTS)
  h7_trace("allocate", "Create instance of type '%s', size: %ld",
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
h7_allocate_array(H7_ATOM* instance, H7_Type* ty, size_t items)
{
#if defined(UNITTESTS)
  h7_trace("allocate", "Create instance of type '%s'x%ld, size: %ld",
           ty->name, items, ty->acc_instance_size);
#endif

  instance->typeid = ty->tag_id;
  instance->u.v_obj = malloc(sizeof(size_t) + items * sizeof(H7_ATOM));
  *((size_t*)instance->u.v_obj) = items;

  /* initialization can only be done from outside.  We can't call
     allocates/initializers from here */
}


void
h7_allocate_int32_array(H7_ATOM* instance,
                     H7_TypeTag tag_id, int init_value, size_t items)
{
#if defined(UNITTESTS)
  h7_trace("allocate", "Create instance of type 'lang|Int32'x%ld (%d), size: %ld a %ld",
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
h7_allocate_int8_array(H7_ATOM* instance,
                       H7_TypeTag tag_id, char init_value, size_t items)
{
#if defined(UNITTESTS)
  h7_trace("allocate", "Create instance of type 'lang|Int8'x%ld (%d), size: %ld a %ld",
           items, tag_id, sizeof(size_t) + items * sizeof(int), sizeof(int));
#endif

  instance->typeid = tag_id;
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
h7_allocate_short_array(H7_ATOM* instance,
                        H7_Type* ty, short init_value, size_t items)
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
h7_allocate_float_array(H7_ATOM* instance,
                        H7_Type* ty, float init_value, size_t items)
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
h7_allocate_double_array(H7_ATOM* instance,
                         H7_Type* ty, double init_value, size_t items)
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


H7_Type*
h7_lookup_derived_type(H7_Type* base_type, const char* full_tag_name)
{
  H7_Type* new_ty;
  H7_Type* ty = h7_hashtable_get(types_name_to_type_map,
                                 (void*)full_tag_name);
  if (ty != NULL)
    return ty;

  new_ty = h7_type_clone_with_other_name(base_type, full_tag_name);
  h7_register_type(new_ty);

  return new_ty;
}
