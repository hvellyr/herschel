/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <string.h>

#include "list.h"


List*
list_insert(List* l, void* value)
{
  List* nl = malloc(sizeof(List));
  nl->fValue = value;
  nl->fTail = l;
  return nl;
}


List*
list_append(List* l, void* value)
{
  List *nl = malloc(sizeof(List));
  nl->fValue = value;
  nl->fTail = NULL;

  if (l) {
    List* p = l;
    while (p->fTail)
      p = p->fTail;
    p->fTail = nl;
    return l;
  }

  return nl;
}


List*
list_insert_before(List* l, void *ref, void *obj)
{
  List* root = l;

  if (l) {
    List* p = l;
    List* last = NULL;

    while (p != NULL) {
      if (p->fValue == ref) {
        List* nl = malloc(sizeof(List));
        nl->fValue = obj;
        nl->fTail = p;

        if (last)
          last->fTail = nl;
        else
          root = nl;

        return root;
      }

      last = p;
      p = p->fTail;
    }
  }

  return root;
}


size_t
list_items(List* l)
{
  size_t count = 0;
  List* p = l;

  while (p != NULL) {
    count++;
    p = p->fTail;
  }
  return count;
}
