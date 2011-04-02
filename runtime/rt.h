/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef runtime_rt_h
#define runtime_rt_h

#include <stdint.h>

#include "runtime/hash.h"
#include "runtime/list.h"

struct ATOM
{
  int typeid;
  union {
    int v_int;
    float v_float;
  } u;
};


/* ------------------------------------------------------------------------
   types, generic functions, methods
   ------------------------------------------------------------------------ */

typedef struct Type Type;
struct Type {
  const char* name;
  Type**      isa;
  size_t      isa_size;
  HashTable*  isa_set;          /* hash<Type*, Type*> i.e. a Set */
  int         tag_id;
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


void type_init();
void methods_init();

void register_type(Type* type);
Type* type_alloc(const char* nm, size_t size, ...);
Type* type_lookup_by_name(const char* nm);
Type* type_lookup_by_tag(int tag_id);

/* checks whether one isa two */
int type_isa(Type* one, Type* two);

GenericFunction* generic_function_alloc(const char* name, size_t argc);

void register_generic_function(GenericFunction* genfun);
void register_method(GenericFunction* gf, void* func, size_t argc, ...);

Method* lookup_func1(GenericFunction* gf, Type* ty0);
Method* lookup_func2(GenericFunction* gf, Type* ty0, Type* ty1);
Method* lookup_func3(GenericFunction* gf, Type* ty0, Type* ty1, Type* ty2);

#endif                          // runtime_rt_h
