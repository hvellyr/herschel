/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef runtime_list_h
#define runtime_list_h

#include <stdlib.h>
#include <stdint.h>

typedef struct List List;
struct List
{
  List*     fTail;
  void*     fValue;
};


List* list_insert(List* l, void* value);
List* list_append(List* l, void* value);
List* list_insert_before(List* l, void *ref, void *obj);

size_t list_items(List* l);

#endif  /* runtime_insert_h */
