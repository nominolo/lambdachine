/* An implementation of Hash Array Mapped Tries */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* const unsigned int SK5  = 0x55555555; */
/* const unsigned int SK3  = 0x33333333; */
/* const unsigned int SKFO = 0xF0F0F0F0; */
/* const unsigned int SKFF = 0x00FF00FF; */


int
countOneBits(unsigned int v) {
  // from http://www-graphics.stanford.edu/~seander/bithacks.html
  v = v - ((v >> 1) & 0x55555555);                    // reuse input as temporary
  v = (v & 0x33333333) + ((v >> 2) & 0x33333333);     // temp
  return ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24; // count
}

struct Entry {
  struct Entry *next; // in case of hash collision
  const char *key;
  void       *data;
};

typedef struct HAMTNode {
  unsigned int  bitmapOrKey;
  void         *baseOrValue;
} HAMTNode;

typedef struct HAMT {
  HAMTNode *root;
  int (*compareKey)(const char* key1, const char* key2);
  unsigned int (*hashKey)(const char* key);
} HAMT;

#define isSubTrie(n)     ((long)((n)->baseOrValue) & 1)
#define setSubTrie(n, v) ((n)->baseOrValue = (void*)(v) | 1)
#define getSubTrie(n)    ((HAMTNode*)(((long)(n)->baseOrValue) & -2))
#define setValue(n, v)   ((n)->baseOrValue = (void*)(v))

static unsigned int
defaultHashKey(const char *key)
{
    unsigned int a=31415, b=27183, vHash;
    for (vHash = 0; *key; key++, a *= b)
        vHash = a * vHash + *key;
    return vHash;
}

HAMT*
HAMT_create()
{
  HAMT *hamt = malloc(sizeof(HAMT));
  int i;

  hamt->root = malloc(32 * sizeof(HAMTNode));
  for (i = 0; i < 32; i++) {
    hamt->root[i].bitmapOrKey = 0;
    hamt->root[i].baseOrValue = 0;
  }

  hamt->hashKey = defaultHashKey;
  hamt->compareKey = strcmp;
}

static void
HAMT_delete_trie(HAMTNode *node)
{
  if (isSubTrie(node)) {
    unsigned int i, size;

    size = countOneBits(node->bitmapOrKey);
    
    for (i = 0; i < size; i++)
      HAMT_delete_trie(&(getSubTrie(node))[i]);

    free(getSubTrie(node));
  }
}

void
HAMT_destroy(HAMT* hamt, void (*delete_func)(void *))
{
  int i;
  // TODO: delete entries

  for (i = 0; i < 32; i++)
    HAMT_delete_trie(&hamt->root[i]);

  free(hamt->root);
  free(hamt);
}

void *
HAMT_insert(HAMT *hamt, const char *key, const char *value)
{
  HAMTNode *node;
  HAMTEntry *entry;
  unsigned int hash, subhash;

  hash = hamt->hashKey(key);
  subhash = hamt & 0x1f;
  node = &hamt->root[subhash];
  
  if (node->baseOrValue == 0) {
    // Slot in root table is currently empty.
    node->bitmapOrKey = hash;
    entry = malloc(sizeof(HAMTEntry));
    entry->next = 0; // TODO: 
    entry->key = key;
    entry->data = value;
    setValue(node, entry);
    return data;
  }

  for (;;) {
    
  }
}

int
main(int argc, char* argv[])
{
  printf("bits = %d\n", countOneBits(0xffffffff));

  return 0;
}
