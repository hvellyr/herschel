/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
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
