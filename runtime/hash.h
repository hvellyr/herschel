/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef runtime_hash_h
#define runtime_hash_h

#include <stdlib.h>
#include <stdint.h>

typedef struct H7_HashNode H7_HashNode;
struct H7_HashNode
{
  H7_HashNode* fTail;
  void*       fKey;
  void*       fValue;
};


typedef unsigned int (*h7_hashtable_hash_func)(void* key);
typedef int (*h7_hashtable_cmp_func)(void* one, void* two);

typedef struct H7_HashTable H7_HashTable;
struct H7_HashTable
{
  H7_HashNode**  fNodes;
  size_t        fSize;
  size_t        fItems;
  h7_hashtable_hash_func fHashFunc;
  h7_hashtable_cmp_func  fKeyCmp;
};


H7_HashTable* h7_hashtable_alloc(size_t size,
                                h7_hashtable_hash_func hashFunc,
                                h7_hashtable_cmp_func cmpFunc);
void h7_hashtable_free(H7_HashTable* table);
void h7_hashtable_add(H7_HashTable* table, void* key, void* value);
void h7_hashtable_add_all(H7_HashTable* table, H7_HashTable* other);
void* h7_hashtable_get(H7_HashTable* table, void* key);
H7_HashNode* h7_hashtable_get_impl(H7_HashTable* table, void* key);
void h7_hashtable_remove(H7_HashTable* table, void* key);

unsigned int h7_hashtable_sizet_func(void* key);
int h7_hashtable_sizet_cmp_func(void* one, void* two);

unsigned int h7_hashtable_cstr_func(void* key);
int h7_hashtable_cstr_cmp_func(void* one, void* two);

#endif                          // runtime_hash_h
