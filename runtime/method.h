/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef runtime_method_h
#define runtime_method_h

#include "runtime/herschel.h"
#include "runtime/method.h"

typedef struct H7_Method H7_Method;
struct H7_Method {
  H7_Type** args;
  size_t    argc;
  void*     func;                  /* function callback pointer */
};

void h7_methods_init();

H7_Method* h7_lookup_func1(H7_GenericFunction* gf,
                           H7_TagId ty0);
H7_Method* h7_lookup_func2(H7_GenericFunction* gf,
                           H7_TagId ty0, H7_TagId ty1);
H7_Method* h7_lookup_func3(H7_GenericFunction* gf,
                           H7_TagId ty0, H7_TagId ty1, H7_TagId ty2);

void h7_register_generic_function(H7_GenericFunction* genfun);

#endif  /* runtime_method_h */
