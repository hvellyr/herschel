/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

/* ---------------------------------------------------------------------------- */

#include <assert.h>
#include <string.h>

#include "runtime/rt.h"

static Type* ty_A   = NULL;
static Type* ty_AB  = NULL;
static Type* ty_AC  = NULL;
static Type* ty_ABC = NULL;

static Type* ty_X   = NULL;
static Type* ty_XY  = NULL;
static Type* ty_XZ  = NULL;
static Type* ty_XYZ = NULL;

/* classes */
static Type* ty_D = NULL;
static Type* ty_E = NULL;

static void
prepare()
{
  type_init();
  methods_init();

  ty_A   = type_alloc("xi|A", 0);
  ty_AB  = type_alloc("xi|AB", 1, ty_A);
  ty_AC  = type_alloc("xi|AC", 1, ty_A);
  ty_ABC = type_alloc("xi|ABC", 2, ty_AB, ty_AC);

  register_type(ty_ABC);
  register_type(ty_AB);
  register_type(ty_AC);
  register_type(ty_A);

  ty_X   = type_alloc("xi|X", 0);
  ty_XY  = type_alloc("xi|XY", 1, ty_X);
  ty_XZ  = type_alloc("xi|XZ", 1, ty_X);
  ty_XYZ = type_alloc("xi|XYZ", 2, ty_XY, ty_XZ);

  register_type(ty_XYZ);
  register_type(ty_XZ);
  register_type(ty_XY);
  register_type(ty_X);

  {
    static TypeSlotPair D_slots[] = {
      { "s1", 0},               /* ATOM */
      { "s2", sizeof(ATOM) },   /* short */
      { NULL, 0 }               /* sentinel */
    };

    size_t instance_size = sizeof(ATOM) + sizeof(short);
    ty_D = class_alloc("foo|D", instance_size, D_slots, 1, ty_X);
    register_type(ty_D);

    assert(ty_D->tag_id > 0);
    assert(ty_D->instance_size == instance_size);
    assert(ty_D->slots_offsets != NULL);
    assert(ty_D->slots_offsets->fItems == 2);
    assert(ty_D->slots == D_slots);
    assert(type_slot_get(ty_D, "s1") == 0);
    assert(type_slot_get(ty_D, "s2") == sizeof(ATOM));
  }

  {
    static TypeSlotPair E_slots[] = {
      { "s1", 0},                                            /* ATOM */
      { "s2", sizeof(ATOM) },                                /* short */
      { "s3", sizeof(ATOM) + sizeof(short) },                /* char */
      { "s4", sizeof(ATOM) + sizeof(short) + sizeof(char) }, /* int */
      { NULL, 0 }                                            /* sentinel */
    };
    size_t instance_size = sizeof(ATOM) + sizeof(short) + sizeof(char) + sizeof(int);

    ty_E = class_alloc("foo|E", instance_size, E_slots, 1, ty_D);
    register_type(ty_E);

    assert(ty_E->tag_id > 0);
    assert(ty_E->instance_size == instance_size);
    assert(ty_E->slots_offsets != NULL);
    assert(ty_E->slots_offsets->fItems == 4);
    assert(ty_E->slots == E_slots);
    assert(type_slot_get(ty_E, "s2") == sizeof(ATOM));
    assert(type_slot_get(ty_E, "s1") == 0);
    assert(type_slot_get(ty_E, "s4") == sizeof(ATOM) + sizeof(short) + sizeof(char));
    assert(type_slot_get(ty_E, "s3") == sizeof(ATOM) + sizeof(short));
  }
}


static void
test_func1()
{
  GenericFunction* m1 = generic_function_alloc("m1", 1);
  GenericFunction* m2 = generic_function_alloc("m2", 1);
  GenericFunction* m3 = generic_function_alloc("m3", 1);

  register_generic_function(m1);
  register_generic_function(m2);
  register_generic_function(m3);

  register_method(m1, "on_m1a", 1, ty_A);
  register_method(m2, "on_m2a", 1, ty_A);

  register_method(m3, "on_m3ab", 1, ty_AB);

  register_method(m2, "on_m2ac", 1, ty_AC);
  register_method(m3, "on_m3ac", 1, ty_AC);

  register_method(m1, "on_m1abc", 1, ty_ABC);

  /* print_generic_func(m1); */
  /* print_generic_func(m2); */
  /* print_generic_func(m3); */


  {
    Method* _m = lookup_func1(m1, ty_ABC->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m1abc") == 0);
  }

  {
    Method* _m = lookup_func1(m2, ty_A->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m2a") == 0);
  }

  {
    Method* _m = lookup_func1(m3, ty_AB->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ab") == 0);
  }

  {
    Method* _m = lookup_func1(m3, ty_AC->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ac") == 0);
  }

  /* this should actually complain about ambiguous method declaration */
  {
    Method* _m = lookup_func1(m3, ty_ABC->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ab") == 0);
  }
}


static void
test_func2()
{
  GenericFunction* n1 = generic_function_alloc("n1", 2);
  GenericFunction* n3 = generic_function_alloc("n3", 2);
  GenericFunction* n4 = generic_function_alloc("n4", 2);
  GenericFunction* n5 = generic_function_alloc("n5", 2);
  GenericFunction* n6 = generic_function_alloc("n6", 2);

  register_generic_function(n1);
  register_generic_function(n3);
  register_generic_function(n4);
  register_generic_function(n5);
  register_generic_function(n6);

  register_method(n1, "on_m1a_x", 2, ty_A, ty_X);
  register_method(n1, "on_m1a_xz", 2, ty_A, ty_XZ);

  register_method(n5, "on_m5a_xz", 2, ty_A, ty_XZ);
  register_method(n3, "on_m3ab_xy", 2, ty_AB, ty_XY);
  register_method(n5, "on_m5ab_x", 2, ty_AB, ty_X);

  register_method(n4, "on_m4abc_x", 2, ty_ABC, ty_X);
  register_method(n6, "on_m4ac_xyz", 2, ty_AC, ty_XYZ);

  /* print_generic_func(n1); */
  /* print_generic_func(n3); */
  /* print_generic_func(n4); */
  /* print_generic_func(n5); */
  /* print_generic_func(n6); */


  {
    Method* _m = lookup_func2(n1, ty_ABC->tag_id, ty_XYZ->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m1a_xz") == 0);
  }

  {
    Method* _m = lookup_func2(n3, ty_AB->tag_id, ty_XY->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ab_xy") == 0);
  }

  {
    /* method not understood, n4 is more specifiy than ty_AB */
    Method* _m = lookup_func2(n4, ty_AB->tag_id, ty_XY->tag_id);
    assert(_m == NULL);
  }

  {
    Method* _m = lookup_func2(n5, ty_ABC->tag_id, ty_XYZ->tag_id);
    assert(_m != NULL);
    /* this should actually throw since it's ambiguous */
    assert(strcmp(_m->func, "on_m5a_xz") == 0);
  }

  {
    Method* _m = lookup_func2(n6, ty_AC->tag_id, ty_X->tag_id);
    assert(_m == NULL);
  }

  {
    Method* _m = lookup_func2(n6, ty_AC->tag_id, ty_XYZ->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m4ac_xyz") == 0);
  }

  {
    Method* _m = lookup_func2(n6, ty_X->tag_id, ty_X->tag_id);
    assert(_m == NULL);
  }

  {
    int i;
    for (i = 0; i < 10000000; i++) {
      Method* _m = lookup_func2(n6, ty_AC->tag_id, ty_XYZ->tag_id);
      assert(_m != NULL);
      assert(strcmp(_m->func, "on_m4ac_xyz") == 0);
    }
  }
}


static void
test_func3()
{
  GenericFunction* t1 = generic_function_alloc("t1", 3);

  {
    Method* _m = lookup_func3(t1, ty_AC->tag_id, ty_XYZ->tag_id, ty_ABC->tag_id);
    assert(_m == NULL);
  }
}


static void
test_allocate1()
{
  ATOM x1;
  ATOM x2;

  allocate(&x1, ty_D);
  assert(x1.typeid == ty_D->tag_id);

  allocate(&x2, ty_E);
  assert(x2.typeid == ty_E->tag_id);
}


int main(int args, char** argv)
{
  prepare();

  test_func1();
  test_func2();
  test_func3();

  test_allocate1();

  return 0;
}
