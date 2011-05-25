/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef runtime_hash_h
#define runtime_hash_h

#include <stdlib.h>
#include <stdint.h>

typedef struct HashNode HashNode;
struct HashNode
{
  HashNode* fTail;
  void*     fKey;
  void*     fValue;
};


typedef struct HashTable HashTable;
struct HashTable
{
  HashNode**  fNodes;
  size_t      fSize;
  size_t      fItems;
};


HashTable* hashtable_alloc(size_t size);
void hashtable_free(HashTable* table);
void hashtable_add(HashTable* table, void* key, void* value);
void hashtable_add_all(HashTable* table, HashTable* other);
void* hashtable_get(HashTable* table, void* key);
void hashtable_remove(HashTable* table, void* key);

#endif                          // runtime_hash_h
