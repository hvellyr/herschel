/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef runtime_list_h
#define runtime_list_h

#include <stdlib.h>
#include <stdint.h>

typedef struct H7_List H7_List;
struct H7_List
{
  H7_List* fTail;
  void*    fValue;
};


H7_List* h7_list_insert(H7_List* l, void* value);
H7_List* h7_list_append(H7_List* l, void* value);
H7_List* h7_list_insert_before(H7_List* l, void *ref, void *obj);

size_t h7_list_items(H7_List* l);

#endif  /* runtime_insert_h */
