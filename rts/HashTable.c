#include "HashTable.h"
#include "Common.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

u4 hashString(const char *str);
void HashTable_rebuild(HashTable *);

HashTable *
HashTable_create()
{
  HashTable *ht = xmalloc(sizeof(HashTable));
  ht->size = HASHTABLE_DEFAULT_SIZE;
  ht->threshold = HASHTABLE_DEFAULT_THRESHOLD;
  ht->entries = 0;
  ht->table = xmalloc(sizeof(HashEntry*) * ht->size);
  memset(ht->table, 0, sizeof(HashEntry*) * ht->size);
  return ht;
}

u4
HashTable_entries(HashTable *ht)
{
  return ht->entries;
}

void *
HashTable_insert(HashTable *ht, const char *key, void *value)
{
  u4 size, mask, hash;
  HashEntry *entry;
  
  if ((ht->entries * 100) / ht->size >= ht->threshold) {
    HashTable_rebuild(ht);
  }

  size = ht->size;
  mask = size - 1;
  hash = hashString(key) & mask;
  entry = xmalloc(sizeof(HashEntry));

  entry->key = key;
  entry->value = value;
  entry->next = ht->table[hash];
  ht->table[hash] = entry;
  ++ht->entries;

  return value;
}

void
HashTable_rebuild(HashTable *ht)
{
  u4 oldsize = ht->size;
  HashEntry **old_table = ht->table;
  int i;

  ht->size *= 2;
  ht->entries = 0;
  ht->table = xmalloc(sizeof(HashEntry*) * ht->size);
  memset(ht->table, 0, sizeof(HashEntry*) * ht->size);

  for (i = 0; i < oldsize; ++i) {
    HashEntry *p, *next;
    
    for (p = old_table[i]; p != NULL; p = next) {
      next = p->next;
      HashTable_insert(ht, p->key, p->value);
      xfree(p);
    }
  }

  xfree(old_table);
}

void *
HashTable_lookup(HashTable *ht, const char *key)
{
  u4 size = ht->size;
  u4 mask = size - 1;
  u4 hash = hashString(key) & mask;
  HashEntry *p;
  
  for (p = ht->table[hash]; p != NULL; p = p->next) {
    if (strcmp(p->key, key) == 0)
      return p->value;
  }
  return NULL;
}

void *
HashTable_update(HashTable *ht, char *key, void *value)
{
  u4 size = ht->size;
  u4 mask = size - 1;
  u4 hash = hashString(key) & mask;
  void *old_value = NULL;
  HashEntry *p;
  for (p = ht->table[hash]; p != NULL; p = p->next) {
    if (strcmp(p->key, key) == 0) {
      old_value = p->value;
      p->value = value;
    }
  }
  return old_value;
}

void
HashTable_destroy(HashTable *ht, HashFreeFunc func)
{
  u4 size = ht->size;
  u4 i;
  for (i = 0; i < size; ++i) {
    HashEntry *p, *next;
    for (p = ht->table[i]; p != NULL; p = next) {
      next = p->next;
      if (func != NULL)
	func(p->key, p->value);
      xfree(p);
    }
  }
  xfree(ht->table);
  xfree(ht);
}

void
HashTable_foreach(HashTable *ht, HashValueCallback f, void *env)
{
  u4 size = ht->size;
  u4 i;

  if (f == NULL)
    return;

  for (i = 0; i < size; ++i) {
    HashEntry *p = ht->table[i];
    if (!p)
      continue;
    for ( ; p != NULL; p = p->next) {
      f(env, p->key, p->value);
    }
  }
}

void
HashTable_print(HashTable *ht, HashValuePrinter printValue)
{
  u4 size = ht->size;
  u4 i;
  
  printf("HashTable [size: %d, ents: %d] {\n", size, ht->entries);
  
  for (i = 0; i < size; ++i) {
    HashEntry *p = ht->table[i];
    if (!p)
      continue;
    //    printf("  %d -> [ ", i);
    for ( ; p != NULL; p = p->next) {
      printf("\"%s\":", p->key);
      if (printValue != NULL) {
        printf("\n");
        printValue(p->value);
      } else
        printf("%p ", p->value);
    }
    //    printf("]\n");
  }
  printf("}\n");
}


// FNV
u4
hashString(const char *str)
{
  u1 *bp = (u1*) str;
  u4 hval = 0x811c9dc5;

  while (*bp) {
    hval += (hval<<1) + (hval<<4) + (hval<<7) + (hval<<8) + (hval<<24);
    hval ^= (u4)*bp++;
  }
  
  return hval;
}

/*
int
main(int argc, char *argv[])
{
  HashTable *ht = HashTable_create();
  HashTable_insert(ht, "foobar", "test");
  HashTable_insert(ht, "foobar1", "test1");
  HashTable_insert(ht, "fobar1", "test3");
  HashTable_insert(ht, "foobaz", "test4");
  HashTable_insert(ht, "fooba2", "test");
  HashTable_insert(ht, "fooba4", "test1");
  HashTable_insert(ht, "fobarg", "test3");
  HashTable_insert(ht, "foobns", "test4");

  HashTable_print(ht);

  printf("%s -> %s\n", "foobar", (char*)HashTable_lookup(ht, "foobar"));

  HashTable_destroy(ht, NULL);
  
  return 0;
}
*/
