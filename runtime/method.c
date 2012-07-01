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

#include "runtime/hash.h"
#include "runtime/list.h"
#include "runtime/method.h"
#include "runtime/trace.h"
#include "runtime/typeid.h"

static H7_HashTable* generic_functions = NULL; /* hash<char*, H7_GenericFunction*> */


void
h7_methods_init()
{
  generic_functions = h7_hashtable_alloc(27,
                                         h7_hashtable_cstr_func,
                                         h7_hashtable_cstr_cmp_func);
}


H7_GenericFunction*
h7_generic_function_alloc(const char* name, size_t argc)
{
  H7_GenericFunction* gf = malloc(sizeof(H7_GenericFunction));
  gf->name = name;
  gf->argc = argc;
  gf->methods = NULL;
  return gf;
}


void
h7_register_generic_function(H7_GenericFunction* genfun)
{
  h7_hashtable_add(generic_functions, (void*)genfun->name, (void*)genfun);

#if defined(UNITTESTS)
  h7_trace("register", "Register generic function: %s [arity %ld]",
           genfun->name, genfun->argc);
#endif
}


static void
h7_print_method(const char* funname, H7_Method* m)
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


void
print_generic_func(H7_GenericFunction* gf)
{
  H7_List *l = gf->methods;
  fprintf(stderr, "-- %ld ----------------------------------------\n", h7_list_items(l));
  while (l) {
    fprintf(stderr, "  ");
    h7_print_method(gf->name, (H7_Method*)l->fValue);
    fprintf(stderr, "\n");
    l = l->fTail;
  }
}


void
h7_register_method(H7_GenericFunction* gf, void* func, size_t argc, ...)
{
  H7_Method* m = malloc(sizeof(H7_Method));
  H7_List* insert_before = NULL;
  H7_List* l = gf->methods;

  m->argc = argc;
  m->func = func;

  if (argc > 0) {
    va_list vp;
    size_t i = 0;

    m->args = malloc(argc * sizeof(H7_Type*));

    va_start(vp, argc);
    for (i = 0; i < argc; i++) {
      m->args[i] = va_arg(vp, H7_Type*);
    }
    va_end(vp);
  }
  else
    m->args = NULL;

#if defined(UNITTESTS)
  h7_trace("register", "Register method: %s [%p, arity %d]",
           gf->name, func, argc);
  {
    size_t i;
    for (i = 0; i < argc; i++) {
      h7_trace("register", "    arg %d: %s", i, m->args[i]->name);
    }
  }
#endif


  while (l != NULL) {
    H7_Method* m0 = (H7_Method*)l->fValue;
    size_t i;
    int ambiguous = 0;
    int is_all_super = 0;
    int is_all_sub = 0;

    for (i = 0; i < argc; i++) {
      if (m0->args[i] == m->args[i]) {
        /* nop */
      }
      else if (h7_type_isa(m0->args[i], m->args[i])) {
        if (is_all_sub)
          ambiguous = 1;
        else
          is_all_super = 1;
      }
      else if (h7_type_isa(m->args[i], m0->args[i])) {
        if (is_all_super)
          ambiguous = 1;
        else
          is_all_sub = 1;
      }
    }

    if (ambiguous) {
      fprintf(stderr, "Ambiguous method registration: ");
      h7_print_method(gf->name, m);
      fprintf(stderr, "  conflicts with: ");
      h7_print_method(gf->name, m0);
      fprintf(stderr, "\n");
    }
    else if (is_all_sub) {
      insert_before = l;
      break;
    }
    l = l->fTail;
  }

  if (insert_before)
    gf->methods = h7_list_insert_before(gf->methods, insert_before->fValue, m);
  else
    gf->methods = h7_list_append(gf->methods, m);

  /* print_generic_func(gf); */
}


static void
h7_no_such_method_cb()
{
  fprintf(stderr, "No such method. Abort\n");
  exit(1);
}


#define h7_ty_name(_ty) \
  ((_ty) != NULL ? (_ty)->name : "<unknown type>")


H7_Method*
h7_lookup_func1(H7_GenericFunction* gf, H7_TagId ty0_id)
{
  H7_Type* ty0 = h7_type_lookup_by_tag(ty0_id);
  if (ty0 != NULL) {
    H7_List* l = gf->methods;

    assert(gf->argc == 1);

    while (l) {
      H7_Method* m = (H7_Method*)l->fValue;
      assert(m->argc == 1);

      if (h7_type_isa(ty0, m->args[0]))
        return m;
      l = l->fTail;
    }
  }

#if defined(UNITTESTS)
  h7_trace("lookup", "lookup_func1: no method found for %s in %s",
           h7_ty_name(ty0), gf->name);
#endif

  static H7_Method no_such_method;
  no_such_method.args = NULL;
  no_such_method.argc = 1;
  no_such_method.func = &h7_no_such_method_cb;

  return &no_such_method;
}


H7_Method*
h7_lookup_func2(H7_GenericFunction* gf, H7_TagId ty0_id, H7_TagId ty1_id)
{
  H7_Type* ty0 = h7_type_lookup_by_tag(ty0_id);
  H7_Type* ty1 = h7_type_lookup_by_tag(ty1_id);

  if (ty0 != NULL && ty1 != NULL) {
    H7_List* l = gf->methods;
    assert(gf->argc == 2);

    while (l) {
      H7_Method* m = (H7_Method*)l->fValue;
      assert(m != NULL && m->argc == 2);

      if (h7_type_isa(ty0, m->args[0]) &&
          h7_type_isa(ty1, m->args[1]))
        return m;

      l = l->fTail;
    }
  }

#if defined(UNITTESTS)
  h7_trace("lookup", "lookup_func2: no method found for (%s, %s) in %s",
           h7_ty_name(ty0), h7_ty_name(ty1), gf->name);
#endif

  static H7_Method no_such_method;
  no_such_method.args = NULL;
  no_such_method.argc = 2;
  no_such_method.func = &h7_no_such_method_cb;

  return &no_such_method;
}


H7_Method*
h7_lookup_func3(H7_GenericFunction* gf,
                H7_TagId ty0_id, H7_TagId ty1_id, H7_TagId ty2_id)
{
  H7_Type* ty0 = h7_type_lookup_by_tag(ty0_id);
  H7_Type* ty1 = h7_type_lookup_by_tag(ty1_id);
  H7_Type* ty2 = h7_type_lookup_by_tag(ty2_id);

  if (ty0 != NULL && ty1 != NULL && ty2 != NULL) {
    H7_List* l = gf->methods;

    assert(gf->argc == 3);

    while (l) {
      H7_Method* m = (H7_Method*)l->fValue;
      assert(m->argc == 3);

      if (h7_type_isa(ty0, m->args[0]) &&
          h7_type_isa(ty1, m->args[1]) &&
          h7_type_isa(ty2, m->args[2]))
        return m;
      l = l->fTail;
    }
  }

#if defined(UNITTESTS)
  h7_trace("lookup", "lookup_func3: no method found for (%s, %s, %s) in %s",
           h7_ty_name(ty0), h7_ty_name(ty1), h7_ty_name(ty2), gf->name);
#endif

  static H7_Method no_such_method;
  no_such_method.args = NULL;
  no_such_method.argc = 3;
  no_such_method.func = &h7_no_such_method_cb;

  return &no_such_method;
}
