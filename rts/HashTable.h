#ifndef _LAMBDACHINE_HASH_TABLE_H
#define _LAMBDACHINE_HASH_TABLE_H

#include "Common.h"

typedef struct _HashEntry {
  const char        *key;
  void              *value;
  struct _HashEntry *next;
} HashEntry;

typedef struct _HashTable {
  u4          size;		/* Must be power of 2 */
  u4          threshold;
  u4          entries;
  HashEntry **table;
} HashTable;

typedef void (*HashFreeFunc)(const char *key, void *value);
typedef void (*ValueFreeFunc)(void *value);
typedef void (*HashValuePrinter)(void *value);
typedef void (*HashValueCallback)(void *env, const char *const key, void *value);

HashTable *HashTable_create();
void      *HashTable_insert(HashTable *, const char *key, void *value);
void      *HashTable_lookup(HashTable *, const char *key);
void      *HashTable_update(HashTable *, char *key, void *value);
void       HashTable_print(HashTable *, HashValuePrinter);
u4         HashTable_entries(HashTable *);
void       HashTable_destroy(HashTable *, HashFreeFunc);
void       HashTable_foreach(HashTable *ht, HashValueCallback f, void *env);

/* Default hash table size.  Must be power of 2 */
#define HASHTABLE_DEFAULT_SIZE          16

/* Hash table resize threshold in percent.  This number divided by 100
   gives the expected average list length of hash table entries.  */
#define HASHTABLE_DEFAULT_THRESHOLD     200

#endif
