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
hashtable_alloc(size_t size,
                hashtable_hash_func hashFunc,
                hashtable_cmp_func cmpFunc)
{
  HashTable* table = (HashTable*)malloc(sizeof(HashTable));
  memset(table, 0, sizeof(HashTable));

  table->fNodes = (HashNode**)malloc(size * sizeof(HashNode*));
  memset(table->fNodes, 0, sizeof(size * sizeof(HashNode*)));

  table->fSize = size;
  table->fItems = 0;
  table->fHashFunc = hashFunc;
  table->fKeyCmp = cmpFunc;

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


void
hashtable_add(HashTable* table, void* key, void* value)
{
  size_t idx = (size_t)(table->fHashFunc(key) & (table->fSize - 1));
  HashNode* node;

  HashNode* n = table->fNodes[idx];
  while (n) {
    if (table->fKeyCmp(n->fKey, key) == 0) {
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
hashtable_get(HashTable* table, void* key)
{
  HashNode* node = table->fNodes[(size_t)(table->fHashFunc(key) & (table->fSize - 1))];

  while (node != NULL) {
    if (table->fKeyCmp(node->fKey, key) == 0)
      return node->fValue;

    node = node->fTail;
  }

  return NULL;
}


HashNode*
hashtable_get_impl(HashTable* table, void* key)
{
  HashNode* node = table->fNodes[(size_t)(table->fHashFunc(key) & (table->fSize - 1))];

  while (node != NULL) {
    if (node->fKey == key)
      return node;

    node = node->fTail;
  }

  return NULL;
}


void
hashtable_remove(HashTable* table, void* key)
{
  size_t idx = (size_t)(table->fHashFunc(key) & (table->fSize - 1));
  HashNode* node = table->fNodes[idx];

  if (node) {
    if (table->fKeyCmp(node->fKey, key) == 0) {
      table->fNodes[idx] = node->fTail;
      free(node);
      table->fItems--;
      return;
    }
    else {
      HashNode* prev = node;
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
hashtable_add_all(HashTable* table, HashTable* other)
{
  HashNode* node;
  size_t i;

  for (i = 0; i < other->fSize; i++) {
    node = other->fNodes[i];
    while (node != NULL) {
      hashtable_add(table, node->fKey, node->fValue);
      node = node->fTail;
    }
  }
}


unsigned int
hashtable_sizet_func(void* key)
{
  return (unsigned int)((size_t)key / sizeof (void *));
}


int
hashtable_sizet_cmp_func(void* one, void* two)
{
  return (size_t)one - (size_t)two;
}


unsigned int
hashtable_cstr_func(void* key)
{
  unsigned int hash = 0x9e3779b9;
  const unsigned char* p = (const unsigned char*)key;

  for ( ; *p; p++)
    hash = (hash << 4) ^ (hash >> 28) ^ *p;

  return (hash ^ (hash >> 10) ^ (hash >> 20));
}


int
hashtable_cstr_cmp_func(void* one, void* two)
{
  return strcmp((char*)one, (char*)two);
}


