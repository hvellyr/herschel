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


typedef unsigned int (*hashtable_hash_func)(void* key);
typedef int (*hashtable_cmp_func)(void* one, void* two);

typedef struct HashTable HashTable;
struct HashTable
{
  HashNode**  fNodes;
  size_t      fSize;
  size_t      fItems;
  hashtable_hash_func fHashFunc;
  hashtable_cmp_func  fKeyCmp;
};


HashTable* hashtable_alloc(size_t size,
                           hashtable_hash_func hashFunc,
                           hashtable_cmp_func cmpFunc);
void hashtable_free(HashTable* table);
void hashtable_add(HashTable* table, void* key, void* value);
void hashtable_add_all(HashTable* table, HashTable* other);
void* hashtable_get(HashTable* table, void* key);
void hashtable_remove(HashTable* table, void* key);

unsigned int hashtable_sizet_func(void* key);
int hashtable_sizet_cmp_func(void* one, void* two);

unsigned int hashtable_cstr_func(void* key);
int hashtable_cstr_cmp_func(void* one, void* two);

#endif                          // runtime_hash_h
