/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef runtime_herschel_h
#define runtime_herschel_h

#include <stdint.h>

#include "runtime/hash.h"
#include "runtime/list.h"


/* public runtime API */

typedef long H7_TypeTag;

typedef struct H7_ATOM H7_ATOM;
struct H7_ATOM
{
  H7_TypeTag typeid;
  union {
    long  v_int;
    int64_t v_int64;
    float v_float;
    void* v_obj;                /* an heap allocated object */
  } u;
};

typedef struct H7_Keyword H7_Keyword;
struct H7_Keyword {
  const char* name;             /* utf8 repr of keyword string */
};


typedef struct H7_GenericFunction H7_GenericFunction;
struct H7_GenericFunction {
  const char* name;
  size_t      argc;             /* number of arguments */
  H7_List*    methods;
};


typedef int H7_TagId;

typedef struct H7_TypeSlotPair H7_TypeSlotPair;
struct H7_TypeSlotPair {
  const char* name;
  size_t      offset;
};

typedef struct H7_Type H7_Type;
struct H7_Type {
  const char*  name;
  H7_Type**    isa;
  size_t       isa_size;
  H7_HashTable* isa_set;          /* hash<Type*, Type*> i.e. a Set */
  H7_TagId     tag_id;
  int          is_array;

  size_t       instance_size;
  size_t       acc_instance_size; /* accumulated instance size for this class
                                  * and all superclasses. */
  H7_HashTable* slots_offsets;    /* hash<char*, size_t> */
  const H7_TypeSlotPair* slots;
};


void h7_runtime_init();

/* allocating instances */
void h7_allocate(H7_ATOM* instance, H7_Type* ty);
void h7_allocate_array(H7_ATOM* instance, H7_Type* ty, size_t items);
void h7_allocate_int32_array(H7_ATOM* instance,
                               H7_TypeTag tag_id, int init_value, size_t items);
void h7_allocate_int8_array(H7_ATOM* instance,
                              H7_TypeTag tag_id, char init_value, size_t items);
void h7_allocate_short_array(H7_ATOM* instance,
                               H7_Type* ty, short init_value, size_t items);
void h7_allocate_float_array(H7_ATOM* instance,
                               H7_Type* ty, float init_value, size_t items);
void h7_allocate_double_array(H7_ATOM* instance,
                                H7_Type* ty, double init_value, size_t items);


void* h7_instance_slot(H7_ATOM instance, const H7_Keyword* keyw);

/* register a keyword as string value and return a normalized handle.  Each
 * keyword is registered only once. */
void* h7_keyword_register(const char* keyword);

H7_GenericFunction* h7_generic_function_alloc(const char* name, size_t argc);
void h7_register_method(H7_GenericFunction* gf, void* func, size_t argc, ...);


H7_Type* h7_class_alloc(const char* nm,
                        size_t instance_size,
                        const H7_TypeSlotPair* slots,
                        size_t isa_size, ...);
H7_Type* h7_type_alloc(const char* nm, size_t isa_size, ...);
void h7_register_type(H7_Type* type);
H7_Type* h7_type_lookup_array_type(H7_Type* base_type);

int h7_instance_isa(H7_ATOM instance, H7_Type* ty);
H7_Type* h7_lookup_derived_type(H7_Type* base_type, const char* full_tag_name);


/* string type */
void h7_allocate_string(H7_ATOM* instance, const char* str);


int8_t h7_atom_2_int8(H7_ATOM a);
int16_t h7_atom_2_int16(H7_ATOM a);
int32_t h7_atom_2_int32(H7_ATOM a);
uint8_t h7_atom_2_uint8(H7_ATOM a);
uint16_t h7_atom_2_uint16(H7_ATOM a);
uint32_t h7_atom_2_uint32(H7_ATOM a);
uint32_t h7_atom_2_char(H7_ATOM a);
int h7_atom_2_bool(H7_ATOM a);
float h7_atom_2_float32(H7_ATOM a);
int64_t h7_atom_2_int64(H7_ATOM a);
uint64_t h7_atom_2_uint64(H7_ATOM a);
double h7_atom_2_float64(H7_ATOM a);
void* h7_atom_2_keyword(H7_ATOM a);

/* ----------------------------------------------------------------------

   function called from herschel code directly
   ---------------------------------------------------------------------- */

H7_ATOM h7_allocate_string_n(size_t items, unsigned int c);
int h7_lang_string_compare(H7_ATOM atom0, H7_ATOM atom1);
unsigned int h7_lang_string_length(H7_ATOM atom0);
unsigned int h7_lang_string_char_at_index(H7_ATOM atom0, unsigned int idx);
void h7_lang_string_set_char_at_index(H7_ATOM atom0,
                                      unsigned int idx,
                                      unsigned int c);


#endif
