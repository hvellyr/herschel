/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef runtime_rt_h
#define runtime_rt_h

#include <stdint.h>

#include "runtime/hash.h"
#include "runtime/list.h"

typedef long TypeTag;

typedef struct ATOM ATOM;
struct ATOM
{
  TypeTag typeid;
  union {
    long  v_int;
    float v_float;
    void* v_obj;                /* an heap allocated object */
  } u;
};


/* ------------------------------------------------------------------------
   types, generic functions, methods
   ------------------------------------------------------------------------ */
typedef int TagId;

typedef struct TypeSlotPair TypeSlotPair;
struct TypeSlotPair {
  const char* name;
  size_t      offset;
};

typedef struct Type Type;
struct Type {
  const char* name;
  Type**      isa;
  size_t      isa_size;
  HashTable*  isa_set;          /* hash<Type*, Type*> i.e. a Set */
  TagId       tag_id;

  size_t      instance_size;
  size_t      acc_instance_size; /* accumulated instance size for this class
                                  * and all superclasses. */
  HashTable*  slots_offsets;    /* hash<char*, size_t> */
  const TypeSlotPair* slots;
};


typedef struct AllocatableType AllocatableType;
struct AllocatableType {
  Type type;
};


typedef struct GenericFunction GenericFunction;
struct GenericFunction {
  const char* name;
  size_t      argc;             /* number of arguments */
  List*       methods;
};


typedef struct Method Method;
struct Method {
  Type** args;
  size_t argc;
  void*  func;                  /* function callback pointer */
};


typedef struct Keyword Keyword;
struct Keyword {
  const char* name;             /* utf8 repr of keyword string */
};


extern void runtime_init();

void type_init();
void methods_init();

void register_type(Type* type);
Type* type_alloc(const char* nm, size_t isa_size, ...);
Type* class_alloc(const char* nm,
                  size_t instance_size,
                  const TypeSlotPair* slots,
                  size_t isa_size, ...);

Type* type_lookup_by_name(const char* nm);
Type* type_lookup_by_tag(int tag_id);

/* checks whether one isa two */
int type_isa(Type* one, Type* two);

GenericFunction* generic_function_alloc(const char* name, size_t argc);

void register_generic_function(GenericFunction* genfun);
void register_method(GenericFunction* gf, void* func, size_t argc, ...);

Method* lookup_func1(GenericFunction* gf, TagId ty0);
Method* lookup_func2(GenericFunction* gf, TagId ty0, TagId ty1);
Method* lookup_func3(GenericFunction* gf, TagId ty0, TagId ty1, TagId ty2);

/* allocating instances */
void allocate(ATOM* instance, Type* ty);

void allocate_array(ATOM* instance, Type* ty, ATOM init_value, size_t items);

void allocate_int32_array(ATOM* instance, TypeTag tag_id, int init_value, size_t items);
void allocate_int8_array(ATOM* instance, TypeTag tag_id, char init_value, size_t items);

void allocate_short_array(ATOM* instance, Type* ty, short init_value, size_t items);
void allocate_float_array(ATOM* instance, Type* ty, float init_value, size_t items);
void allocate_double_array(ATOM* instance, Type* ty, double init_value, size_t items);

size_t type_slot_get(Type* ty, const char* slot_name);
void* instance_slot(ATOM instance, const Keyword* keyw);


/* register a keyword as string value and return a normalized handle.  Each
 * keyword is registered only once. */
void* keyword_register(const char* keyword);

#endif                          // runtime_rt_h
