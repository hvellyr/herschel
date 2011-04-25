/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include <stdarg.h>
#include <assert.h>
#include <stdio.h>

#include "runtime/rt.h"
#include "runtime/hash.h"
#include "runtime/list.h"

static HashTable* generic_functions = NULL; /* hash<char*, GenericFunction*> */


void
methods_init()
{
  generic_functions = hashtable_alloc(27);
}


GenericFunction*
generic_function_alloc(const char* name, size_t argc)
{
  GenericFunction* gf = malloc(sizeof(GenericFunction));
  gf->name = name;
  gf->argc = argc;
  return gf;
}


void
register_generic_function(GenericFunction* genfun)
{
  hashtable_add(generic_functions, (void*)genfun->name, (void*)genfun);
  fprintf(stderr, "Register generic function: %s [arity %ld]\n", genfun->name, genfun->argc);
}


static void
print_method(const char* funname, Method* m)
{
  int i;

  fprintf(stderr, "%s(", funname);

  for (i = 0; i < m->argc; i++) {
    if (i > 0)
      fprintf(stderr, ", ");
    fprintf(stderr, "%s", m->args[i]->name);
  }
  fprintf(stderr, ")");
}


/* static void */
/* print_generic_func(GenericFunction* gf) */
/* { */
/*   List *l = gf->methods; */
/*   fprintf(stderr, "------------------------------------------\n"); */
/*   while (l) { */
/*     fprintf(stderr, "  "); */
/*     print_method(gf->name, (Method*)l->fValue); */
/*     fprintf(stderr, "\n"); */
/*     l = l->fTail; */
/*   } */
/* } */


void
register_method(GenericFunction* gf, void* func, size_t argc, ...)
{
  Method* m = malloc(sizeof(Method));
  List* insert_before = NULL;
  List* l = gf->methods;

  m->argc = argc;
  m->func = func;

  if (argc > 0) {
    va_list vp;
    size_t i = 0;

    m->args = malloc(argc * sizeof(Type*));

    va_start(vp, argc);
    for (i = 0; i < argc; i++) {
      m->args[i] = va_arg(vp, Type*);
    }
    va_end(vp);
  }
  else
    m->args = NULL;

  while (l) {
    Method* m0 = (Method*)l->fValue;
    size_t i;
    int ambiguous = 0;
    int is_all_super = 0;
    int is_all_sub = 0;

    for (i = 0; i < argc; i++) {
      if (m0->args[i] == m->args[i]) {
        /* nop */
      }
      else if (type_isa(m0->args[i], m->args[i])) {
        if (is_all_sub)
          ambiguous = 1;
        else
          is_all_super = 1;
      }
      else if (type_isa(m->args[i], m0->args[i])) {
        if (is_all_super)
          ambiguous = 1;
        else
          is_all_sub = 1;
      }
    }

    if (ambiguous) {
      fprintf(stderr, "Ambiguous method registration: ");
      print_method(gf->name, m);
      fprintf(stderr, "  conflicts with: ");
      print_method(gf->name, m0);
      fprintf(stderr, "\n");
    }
    else if (is_all_sub) {
      insert_before = l;
      break;
    }
    l = l->fTail;
  }

  if (insert_before)
    gf->methods = list_insert(insert_before, m);
  else
    gf->methods = list_append(gf->methods, m);
}



Method*
lookup_func1(GenericFunction* gf, TagId ty0_id)
{
  Type* ty0 = type_lookup_by_tag(ty0_id);
  List* l = gf->methods;

  assert(gf->argc == 1);

  while (l) {
    Method* m = (Method*)l->fValue;
    assert(m->argc == 1);

    if (type_isa(ty0, m->args[0]))
      return m;
    l = l->fTail;
  }

  return NULL;
}


Method*
lookup_func2(GenericFunction* gf, TagId ty0_id, TagId ty1_id)
{
  Type* ty0 = type_lookup_by_tag(ty0_id);
  Type* ty1 = type_lookup_by_tag(ty1_id);
  List* l = gf->methods;

  assert(gf->argc == 2);

  while (l) {
    Method* m = (Method*)l->fValue;
    assert(m->argc == 2);

    if (type_isa(ty0, m->args[0]) &&
        type_isa(ty1, m->args[1]))
      return m;
    l = l->fTail;
  }

  return NULL;
}


Method*
lookup_func3(GenericFunction* gf, TagId ty0_id, TagId ty1_id, TagId ty2_id)
{
  Type* ty0 = type_lookup_by_tag(ty0_id);
  Type* ty1 = type_lookup_by_tag(ty1_id);
  Type* ty2 = type_lookup_by_tag(ty2_id);

  List* l = gf->methods;

  assert(gf->argc == 3);

  while (l) {
    Method* m = (Method*)l->fValue;
    assert(m->argc == 3);

    if (type_isa(ty0, m->args[0]) &&
        type_isa(ty1, m->args[1]) &&
        type_isa(ty2, m->args[2]))
      return m;
    l = l->fTail;
  }

  return NULL;
}