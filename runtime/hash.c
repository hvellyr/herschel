/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <string.h>

#include "runtime/hash.h"


H7_HashTable*
h7_hashtable_alloc(size_t size,
                   h7_hashtable_hash_func hashFunc,
                   h7_hashtable_cmp_func cmpFunc)
{
  H7_HashTable* table = (H7_HashTable*)malloc(sizeof(H7_HashTable));
  memset(table, 0, sizeof(H7_HashTable));

  table->fNodes = (H7_HashNode**)malloc(size * sizeof(H7_HashNode*));
  memset(table->fNodes, 0, sizeof(size * sizeof(H7_HashNode*)));

  table->fSize = size;
  table->fItems = 0;
  table->fHashFunc = hashFunc;
  table->fKeyCmp = cmpFunc;

  return table;
}


void
h7_hashtable_free(H7_HashTable* table)
{
  H7_HashNode* node;
  H7_HashNode* next;
  size_t i;

  for (i = 0; i < table->fSize; i++) {
    node = table->fNodes[i];
    while (node) {
      next = node->fTail;
      free(node);
      node = next;
    }
  }

  free(table->fNodes);
  free(table);
}


void
h7_hashtable_add(H7_HashTable* table, void* key, void* value)
{
  size_t idx = (size_t)(table->fHashFunc(key) & (table->fSize - 1));
  H7_HashNode* node;

  H7_HashNode* n = table->fNodes[idx];
  while (n) {
    if (table->fKeyCmp(n->fKey, key) == 0) {
      n->fValue = value;
      return;
    }
    n = n->fTail;
  }

  node = (H7_HashNode*)malloc(sizeof(H7_HashNode));
  node->fKey = key;
  node->fValue = value;
  node->fTail = table->fNodes[idx];

  table->fNodes[idx] = node;
  table->fItems++;

  if ((table->fSize * 75) / 100 <= table->fItems) {
    size_t new_size = table->fSize * 2;
    H7_HashNode** new_nodes = (H7_HashNode**)malloc(new_size * sizeof(H7_HashNode*));

    unsigned int mask = new_size - 1;
    size_t i;

    /* resize by moving the old nodes into a new table, thus avoiding memory
       reallocation */
    for (i = 0; i < table->fSize; i++) {
      H7_HashNode* n = table->fNodes[i];
      while (n) {
        H7_HashNode* next = n->fTail;

        size_t new_idx = (size_t)(table->fHashFunc(n->fKey) & mask);
        n->fTail = new_nodes[new_idx];
        new_nodes[idx] = n;

        n = next;
      }
    }

    free(table->fNodes);
    table->fNodes = new_nodes;
    table->fSize = new_size;
  }
}


void*
h7_hashtable_get(H7_HashTable* table, void* key)
{
  H7_HashNode* node = table->fNodes[(size_t)(table->fHashFunc(key) & (table->fSize - 1))];

  while (node != NULL) {
    if (table->fKeyCmp(node->fKey, key) == 0)
      return node->fValue;

    node = node->fTail;
  }

  return NULL;
}


H7_HashNode*
h7_hashtable_get_impl(H7_HashTable* table, void* key)
{
  H7_HashNode* node = table->fNodes[(size_t)(table->fHashFunc(key) & (table->fSize - 1))];

  while (node != NULL) {
    if (table->fKeyCmp(node->fKey, key) == 0)
      return node;

    node = node->fTail;
  }

  return NULL;
}


void
h7_hashtable_remove(H7_HashTable* table, void* key)
{
  size_t idx = (size_t)(table->fHashFunc(key) & (table->fSize - 1));
  H7_HashNode* node = table->fNodes[idx];

  if (node) {
    if (table->fKeyCmp(node->fKey, key) == 0) {
      table->fNodes[idx] = node->fTail;
      free(node);
      table->fItems--;
      return;
    }
    else {
      H7_HashNode* prev = node;
      node = node->fTail;
      while (node) {
        if (table->fKeyCmp(node->fKey, key) == 0) {
          prev->fTail = node->fTail;
          free(node);
          table->fItems--;
          return;
        }
        prev = node;
        node = node->fTail;
      }
    }
  }
}


void
h7_hashtable_add_all(H7_HashTable* table, H7_HashTable* other)
{
  H7_HashNode* node;
  size_t i;

  for (i = 0; i < other->fSize; i++) {
    node = other->fNodes[i];
    while (node != NULL) {
      h7_hashtable_add(table, node->fKey, node->fValue);
      node = node->fTail;
    }
  }
}


unsigned int
h7_hashtable_sizet_func(void* key)
{
  return (unsigned int)((size_t)key / sizeof (void *));
}


int
h7_hashtable_sizet_cmp_func(void* one, void* two)
{
  return (size_t)one - (size_t)two;
}


unsigned int
h7_hashtable_cstr_func(void* key)
{
  unsigned int hash = 0x9e3779b9;
  const unsigned char* p = (const unsigned char*)key;

  for ( ; *p; p++)
    hash = (hash << 4) ^ (hash >> 28) ^ *p;

  return (hash ^ (hash >> 10) ^ (hash >> 20));
}


int
h7_hashtable_cstr_cmp_func(void* one, void* two)
{
  return strcmp((char*)one, (char*)two);
}


