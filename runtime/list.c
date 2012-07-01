/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <string.h>

#include "list.h"


H7_List*
h7_list_insert(H7_List* l, void* value)
{
  H7_List* nl = malloc(sizeof(H7_List));
  nl->fValue = value;
  nl->fTail = l;
  return nl;
}


H7_List*
h7_list_append(H7_List* l, void* value)
{
  H7_List *nl = malloc(sizeof(H7_List));
  nl->fValue = value;
  nl->fTail = NULL;

  if (l) {
    H7_List* p = l;
    while (p->fTail)
      p = p->fTail;
    p->fTail = nl;
    return l;
  }

  return nl;
}


H7_List*
h7_list_insert_before(H7_List* l, void *ref, void *obj)
{
  H7_List* root = l;

  if (l) {
    H7_List* p = l;
    H7_List* last = NULL;

    while (p != NULL) {
      if (p->fValue == ref) {
        H7_List* nl = malloc(sizeof(H7_List));
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
h7_list_items(H7_List* l)
{
  size_t count = 0;
  H7_List* p = l;

  while (p != NULL) {
    count++;
    p = p->fTail;
  }
  return count;
}
