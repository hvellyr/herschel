/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

/* ---------------------------------------------------------------------------- */

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "runtime/herschel.h"
#include "runtime/method.h"
#include "runtime/typeid.h"

static H7_Type* ty_A   = NULL;
static H7_Type* ty_AB  = NULL;
static H7_Type* ty_AC  = NULL;
static H7_Type* ty_ABC = NULL;

static H7_Type* ty_X   = NULL;
static H7_Type* ty_XY  = NULL;
static H7_Type* ty_XZ  = NULL;
static H7_Type* ty_XYZ = NULL;

/* classes */
static H7_Type* ty_D = NULL;
static H7_Type* ty_E = NULL;

static const char* s1_key = "s1";
static const char* s2_key = "s2";
static const char* s3_key = "s3";
static const char* s4_key = "s4";

static void
prepare()
{
  h7_type_init();
  h7_methods_init();

  ty_A   = h7_type_alloc("xi|A", 0);
  ty_AB  = h7_type_alloc("xi|AB", 1, ty_A);
  ty_AC  = h7_type_alloc("xi|AC", 1, ty_A);
  ty_ABC = h7_type_alloc("xi|ABC", 2, ty_AB, ty_AC);

  h7_register_type(ty_ABC);
  h7_register_type(ty_AB);
  h7_register_type(ty_AC);
  h7_register_type(ty_A);

  ty_X   = h7_type_alloc("xi|X", 0);
  ty_XY  = h7_type_alloc("xi|XY", 1, ty_X);
  ty_XZ  = h7_type_alloc("xi|XZ", 1, ty_X);
  ty_XYZ = h7_type_alloc("xi|XYZ", 2, ty_XY, ty_XZ);

  h7_register_type(ty_XYZ);
  h7_register_type(ty_XZ);
  h7_register_type(ty_XY);
  h7_register_type(ty_X);

  {
    H7_TypeSlotPair D_slots[] = {
      { s1_key, 0},                /* H7_ATOM */
      { s2_key, sizeof(H7_ATOM) }, /* short */
      { NULL, 0 }                  /* sentinel */
    };

    size_t instance_size = sizeof(H7_ATOM) + sizeof(short);
    ty_D = h7_class_alloc("foo|D", instance_size, D_slots, 1, ty_X);
    h7_register_type(ty_D);

    assert(ty_D->tag_id > 0);
    assert(ty_D->instance_size == instance_size);
    assert(ty_D->slots_offsets != NULL);
    assert(ty_D->slots_offsets->fItems == 2);
    assert(ty_D->slots == D_slots);
    assert(h7_type_slot_get(ty_D, s1_key) == 0);
    assert(h7_type_slot_get(ty_D, s2_key) == sizeof(H7_ATOM));
  }

  {
    H7_TypeSlotPair E_slots[] = {
      { s1_key, 0},                                               /* H7_ATOM */
      { s2_key, sizeof(H7_ATOM) },                                /* short */
      { s3_key, sizeof(H7_ATOM) + sizeof(short) },                /* char */
      { s4_key, sizeof(H7_ATOM) + sizeof(short) + sizeof(char) }, /* int */
      { NULL, 0 }                                                 /* sentinel */
    };
    size_t instance_size = sizeof(H7_ATOM) + sizeof(short) + sizeof(char) + sizeof(int);

    ty_E = h7_class_alloc("foo|E", instance_size, E_slots, 1, ty_D);
    h7_register_type(ty_E);

    size_t ofs = sizeof(H7_ATOM) + sizeof(short);
    assert(ty_E->tag_id > 0);
    assert(ty_E->instance_size == instance_size);
    assert(ty_E->slots_offsets != NULL);
    assert(ty_E->slots_offsets->fItems == 4);
    assert(ty_E->slots == E_slots);
    assert(h7_type_slot_get(ty_E, s2_key) == ofs + sizeof(H7_ATOM));
    assert(h7_type_slot_get(ty_E, s1_key) == ofs + 0);
    assert(h7_type_slot_get(ty_E, s4_key) == ofs + sizeof(H7_ATOM) + sizeof(short) + sizeof(char));
    assert(h7_type_slot_get(ty_E, s3_key) == ofs + sizeof(H7_ATOM) + sizeof(short));
  }
}


static void
test_func1()
{
  H7_GenericFunction* m1 = h7_generic_function_alloc("m1", 1);
  H7_GenericFunction* m2 = h7_generic_function_alloc("m2", 1);
  H7_GenericFunction* m3 = h7_generic_function_alloc("m3", 1);

  h7_register_generic_function(m1);
  h7_register_generic_function(m2);
  h7_register_generic_function(m3);

  h7_register_method(m1, "on_m1a", 1, ty_A);
  h7_register_method(m2, "on_m2a", 1, ty_A);

  h7_register_method(m3, "on_m3ab", 1, ty_AB);

  h7_register_method(m2, "on_m2ac", 1, ty_AC);
  h7_register_method(m3, "on_m3ac", 1, ty_AC);

  h7_register_method(m1, "on_m1abc", 1, ty_ABC);

  /* print_generic_func(m1); */
  /* print_generic_func(m2); */
  /* print_generic_func(m3); */


  {
    H7_Method* _m = h7_lookup_func1(m1, ty_ABC->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m1abc") == 0);
  }

  {
    H7_Method* _m = h7_lookup_func1(m2, ty_A->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m2a") == 0);
  }

  {
    H7_Method* _m = h7_lookup_func1(m3, ty_AB->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ab") == 0);
  }

  {
    H7_Method* _m = h7_lookup_func1(m3, ty_AC->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ac") == 0);
  }

  /* this should actually complain about ambiguous method declaration */
  {
    H7_Method* _m = h7_lookup_func1(m3, ty_ABC->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ab") == 0);
  }
}


static void
test_func2()
{
  H7_GenericFunction* n1 = h7_generic_function_alloc("n1", 2);
  H7_GenericFunction* n3 = h7_generic_function_alloc("n3", 2);
  H7_GenericFunction* n4 = h7_generic_function_alloc("n4", 2);
  H7_GenericFunction* n5 = h7_generic_function_alloc("n5", 2);
  H7_GenericFunction* n6 = h7_generic_function_alloc("n6", 2);

  h7_register_generic_function(n1);
  h7_register_generic_function(n3);
  h7_register_generic_function(n4);
  h7_register_generic_function(n5);
  h7_register_generic_function(n6);

  h7_register_method(n1, "on_m1a_x", 2, ty_A, ty_X);
  h7_register_method(n1, "on_m1a_xz", 2, ty_A, ty_XZ);

  h7_register_method(n5, "on_m5a_xz", 2, ty_A, ty_XZ);
  h7_register_method(n3, "on_m3ab_xy", 2, ty_AB, ty_XY);
  h7_register_method(n5, "on_m5ab_x", 2, ty_AB, ty_X);

  h7_register_method(n4, "on_m4abc_x", 2, ty_ABC, ty_X);
  h7_register_method(n6, "on_m4ac_xyz", 2, ty_AC, ty_XYZ);

  /* print_generic_func(n1); */
  /* print_generic_func(n3); */
  /* print_generic_func(n4); */
  /* print_generic_func(n5); */
  /* print_generic_func(n6); */


  {
    H7_Method* _m = h7_lookup_func2(n1, ty_ABC->tag_id, ty_XYZ->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m1a_xz") == 0);
  }

  {
    H7_Method* _m = h7_lookup_func2(n3, ty_AB->tag_id, ty_XY->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m3ab_xy") == 0);
  }

  {
    /* method not understood, n4 is more specifiy than ty_AB */
    H7_Method* _m = h7_lookup_func2(n4, ty_AB->tag_id, ty_XY->tag_id);
    assert(_m != NULL);
  }

  {
    H7_Method* _m = h7_lookup_func2(n5, ty_ABC->tag_id, ty_XYZ->tag_id);
    assert(_m != NULL);
    /* this should actually throw since it's ambiguous */
    assert(strcmp(_m->func, "on_m5a_xz") == 0);
  }

  {
    H7_Method* _m = h7_lookup_func2(n6, ty_AC->tag_id, ty_X->tag_id);
    assert(_m != NULL);
  }

  {
    H7_Method* _m = h7_lookup_func2(n6, ty_AC->tag_id, ty_XYZ->tag_id);
    assert(_m != NULL);
    assert(strcmp(_m->func, "on_m4ac_xyz") == 0);
  }

  {
    H7_Method* _m = h7_lookup_func2(n6, ty_X->tag_id, ty_X->tag_id);
    assert(_m != NULL);
  }

  {
    int i;
    for (i = 0; i < 10000000; i++) {
      H7_Method* _m = h7_lookup_func2(n6, ty_AC->tag_id, ty_XYZ->tag_id);
      assert(_m != NULL);
      assert(strcmp(_m->func, "on_m4ac_xyz") == 0);
    }
  }
}


static void
test_func3()
{
  H7_GenericFunction* t1 = h7_generic_function_alloc("t1", 3);

  {
    H7_Method* _m = h7_lookup_func3(t1,
                                    ty_AC->tag_id,
                                    ty_XYZ->tag_id,
                                    ty_ABC->tag_id);
    assert(_m != NULL);
  }
}


static void
test_allocate1()
{
  H7_ATOM x1;
  H7_ATOM x2;

  h7_allocate(&x1, ty_D);
  assert(x1.typeid == ty_D->tag_id);

  h7_allocate(&x2, ty_E);
  assert(x2.typeid == ty_E->tag_id);
}


static void
test_slots()
{
  H7_ATOM x1;

  H7_Keyword keyw_s1 = { "s1" };
  H7_Keyword keyw_s2 = { "s2" };

  h7_allocate(&x1, ty_D);
  H7_ATOM* s1 = (H7_ATOM*)h7_instance_slot(x1, &keyw_s1);
  assert(s1 != NULL);

  s1->typeid = 'i';
  s1->u.v_int = 0xdeadface;

  short* s2 = (short*)h7_instance_slot(x1, &keyw_s2);
  assert(s2 != NULL);

  *s2 = 0x4711;

  assert(x1.u.v_obj == (void*)s1);
  assert(x1.u.v_obj + sizeof(H7_ATOM) == (void*)s2);
}


int main(int args, char** argv)
{
  prepare();

  test_func1();
  test_func2();
  test_func3();

  test_allocate1();
  test_slots();

  return 0;
}
