/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <string.h>

#include "runtime/hash.h"
#include "runtime/rt.h"


HashTable*
hashtable_alloc(size_t size)
{
  HashTable* table = (HashTable*)malloc(sizeof(HashTable));
  memset(table, 0, sizeof(HashTable));

  table->fNodes = (HashNode**)malloc(size * sizeof(HashNode*));
  memset(table->fNodes, 0, sizeof(size * sizeof(HashNode*)));

  table->fSize = size;
  table->fItems = 0;

  return table;
}


void
hashtable_free(HashTable* table)
{
  HashNode* node;
  HashNode* next;
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


#define hashtable_func(_key, _mask) \
  ((size_t)(((size_t)_key / sizeof (void *)) & (_mask)))


void
hashtable_add(HashTable* table, void* key, void* value)
{
  size_t idx = hashtable_func(key, table->fSize - 1);
  HashNode* node;

  HashNode* n = table->fNodes[idx];
  while (n) {
    if (n->fKey == key) {
      n->fValue = value;
      return;
    }
    n = n->fTail;
  }

  node = (HashNode*)malloc(sizeof(HashNode));
  node->fKey = key;
  node->fValue = value;
  node->fTail = table->fNodes[idx];

  table->fNodes[idx] = node;
  table->fItems++;

  if ((table->fSize * 75) / 100 <= table->fItems) {
    size_t new_size = table->fSize * 2;
    HashNode** new_nodes = (HashNode**)malloc(new_size * sizeof(HashNode*));

    unsigned int mask = new_size - 1;
    size_t i;

    /* resize by moving the old nodes into a new table, thus avoiding memory
       reallocation */
    for (i = 0; i < table->fSize; i++) {
      HashNode* n = table->fNodes[i];
      while (n) {
        HashNode* next = n->fTail;

        size_t new_idx = hashtable_func(n->fKey, mask);
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
hashtable_get(HashTable* table, void* key)
{
  HashNode* node = table->fNodes[hashtable_func(key, table->fSize - 1)];

  while (node != NULL) {
    if (node->fKey == key)
      return node->fValue;

    node = node->fTail;
  }

  return NULL;
}


void
hashtable_remove(HashTable* table, void* key)
{
  size_t idx = hashtable_func(key, table->fSize - 1);
  HashNode* node = table->fNodes[idx];

  if (node) {
    if (node->fKey == key) {
      table->fNodes[idx] = node->fTail;
      free(node);
      table->fItems--;
      return;
    }
    else {
      HashNode* prev = node;
      node = node->fTail;
      while (node) {
        if (node->fKey == key) {
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
hashtable_add_all(HashTable* table, HashTable* other)
{
  HashNode* node;
  HashNode* next;
  size_t i;

  for (i = 0; i < other->fSize; i++) {
    node = other->fNodes[i];
    while (node) {
      next = node->fTail;

      hashtable_add(table, node->fKey, node->fValue);

      node = next;
    }
  }
}
