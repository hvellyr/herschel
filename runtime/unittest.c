/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

/* ---------------------------------------------------------------------------- */

#include <assert.h>

#include "runtime/rt.h"

static Type* ty_A   = NULL;
static Type* ty_AB  = NULL;
static Type* ty_AC  = NULL;
static Type* ty_ABC = NULL;

static Type* ty_X   = NULL;
static Type* ty_XY  = NULL;
static Type* ty_XZ  = NULL;
static Type* ty_XYZ = NULL;

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
    Method* _m = lookup_func1(m1, ty_ABC);
    assert(_m != NULL);
    assert(_m->func == "on_m1abc");
  }

  {
    Method* _m = lookup_func1(m2, ty_A);
    assert(_m != NULL);
    assert(_m->func == "on_m2a");
  }

  {
    Method* _m = lookup_func1(m3, ty_AB);
    assert(_m != NULL);
    assert(_m->func == "on_m3ab");
  }

  {
    Method* _m = lookup_func1(m3, ty_AC);
    assert(_m != NULL);
    assert(_m->func == "on_m3ac");
  }

  /* this should actually complain about ambiguous method declaration */
  {
    Method* _m = lookup_func1(m3, ty_ABC);
    assert(_m != NULL);
    assert(_m->func == "on_m3ab");
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
    Method* _m = lookup_func2(n1, ty_ABC, ty_XYZ);
    assert(_m != NULL);
    assert(_m->func == "on_m1a_xz");
  }

  {
    Method* _m = lookup_func2(n3, ty_AB, ty_XY);
    assert(_m != NULL);
    assert(_m->func == "on_m3ab_xy");
  }

  {
    /* method not understood, n4 is more specifiy than ty_AB */
    Method* _m = lookup_func2(n4, ty_AB, ty_XY);
    assert(_m == NULL);
  }

  {
    Method* _m = lookup_func2(n5, ty_ABC, ty_XYZ);
    assert(_m != NULL);
    /* this should actually throw since it's ambiguous */
    assert(_m->func == "on_m5a_xz");
  }

  {
    Method* _m = lookup_func2(n6, ty_AC, ty_X);
    assert(_m == NULL);
  }

  {
    Method* _m = lookup_func2(n6, ty_AC, ty_XYZ);
    assert(_m != NULL);
    assert(_m->func == "on_m4ac_xyz");
  }

  {
    Method* _m = lookup_func2(n6, ty_X, ty_X);
    assert(_m == NULL);
  }

  {
    int i;
    for (i = 0; i < 10000000; i++) {
      Method* _m = lookup_func2(n6, ty_AC, ty_XYZ);
      assert(_m != NULL);
      assert(_m->func == "on_m4ac_xyz");
    }
  }
}


static void
test_func3()
{
  GenericFunction* t1 = generic_function_alloc("t1", 3);

  {
    Method* _m = lookup_func3(t1, ty_AC, ty_XYZ, ty_ABC);
    assert(_m == NULL);
  }
}


int main(int args, char** argv)
{
  prepare();

  test_func1();
  test_func2();
  test_func3();

  return 0;
}
