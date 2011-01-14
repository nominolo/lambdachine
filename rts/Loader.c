#include "Loader.h"
#include "HashTable.h"
#include "InfoTables.h"
#include "FileUtils.h"
#include "PrintClosure.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <assert.h>

GlobalLoaderState *G_loader = NULL;
HashTable *G_modules;
char *G_basepath = NULL;

#define VERSION_MAJOR  0
#define VERSION_MINOR  1

#define BUFSIZE 512

void printInfoTable(InfoTable* info0);


void
bufferOverflow(int sz, int bufsize)
{
  fprintf(stderr, "FATAL: buffer overflow (%d, max: %d)\n", 
          sz, bufsize - 1);
  exit(1);
}

void printModule(Module *mdl);
void printStringTable(StringTabEntry *tbl, u4 len);


void
initBasepath()
{
  char *home = getenv("HOME");
  char buf[BUFSIZE];
  int res;

  res = snprintf(buf, BUFSIZE, "%s/Dropbox/code/lambdachine/tests/", home);
  if (res >= BUFSIZE) bufferOverflow(res, BUFSIZE);

  G_basepath = strdup(buf);
}

void
initLoader()
{
  G_loader = malloc(sizeof(*G_loader));
  G_loader->loadedModules = HashTable_create();
  G_loader->infoTables = HashTable_create();
  G_loader->closures = HashTable_create();

  initBasepath();
}

int
isModuleLoaded(const char *moduleName)
{
  return HashTable_lookup(G_loader->loadedModules, moduleName) != NULL;
}

/* Turn "Foo.Bar.Baz" into "Foo/Bar/Baz". Copies input string. */
char *
modulePath(const char *moduleName)
{
  char *path = strdup(moduleName);
  char *p = path;
  while (*p) {
    if (*p == '.') *p = '/';
    ++p;
  }
  return path;
}

/* TODO: make Windows compatible */
int
fileExists(const char *path)
{
  struct stat st;
  if (stat(path, &st) != 0) return 0;
  return S_ISREG(st.st_mode);
}

/* String arguments must be UTF-8 encoded. */
FILE *
openModuleFile(const char *packageName, const char *moduleName)
{
  char path[BUFSIZE];
  int res;
  char *base = G_basepath;

  if (base == NULL) {
    fprintf(stderr, "ERROR: Couldn't find base path.\n");
    exit(1);
  }

  res = snprintf(path, BUFSIZE, "%s/lib/khc/packages/%s/%s.kbc",
                 base, packageName, modulePath(moduleName));
  if (res >= BUFSIZE) bufferOverflow(res, BUFSIZE);
  
  if (fileExists(path)) {
    return fopen(path, "rb");
  }
  
  fprintf(stderr, "ERROR: Could not find module `%s' in package `%s'.\n",
          moduleName, packageName);
  fprintf(stderr, "  tried: %s\n", path);
  return NULL;
}

/* char *
fget_string(FILE *f)
{
  u4 len = fget_varuint(f);
  char *str = malloc(len + 1);
  fread(str, 1, len, f);
  str[len] = '\0';
  return str;
} 
*/

void
loadStringTabEntry(FILE *f, StringTabEntry* e)
{
  e->len = fget_varuint(f);
  e->str = malloc(e->len + 1);
  fread(e->str, 1, e->len, f);
  e->str[e->len] = '\0';
}

char *
loadId(FILE *f, const StringTabEntry *strings, const char* sep);

void
loadCode(FILE *f, LcCode * /* out */,
         const StringTabEntry *strings, void **fwd_refs);

void
loadCode2(FILE *f, LcCode *code/*out*/,
          const StringTabEntry *strings,
          HashTable *itbls, HashTable *closures);

void
loadLiteral(FILE *f, u1 *littype /*out*/, Word *literal /*out*/,
            const StringTabEntry *strings, void **fwd_refs);

InfoTable *
loadInfoTable(FILE *, const StringTabEntry *strings, void **fwd_refs);
InfoTable *
loadInfoTable2(FILE *, const StringTabEntry *strings,
               HashTable *itbls, HashTable *closures);

Closure *
loadClosure(FILE *, const StringTabEntry *strings,
            HashTable *itbls, HashTable *closures);

#define allocInfoTable(size)     (malloc(size))
#define allocStaticClosure(size) (malloc(size))

char *
moduleNameToFile(const char *basepath, const char *name)
{
  size_t   baselen  = strlen(basepath);
  size_t   len      = strlen(name);
  size_t   rsltlen;
  char    *filename, *p;
  size_t   i;

  if (baselen == 0) {
    baselen = 1;
    basepath = ".";
  }

  rsltlen = baselen + 1 + len + 5;
  filename = malloc(rsltlen + 1);

  strcpy(filename, basepath);
  filename[baselen] = '/';

  p = &filename[baselen + 1];

  for (i = 0; i < len; i++) {
    if (name[i] == '.')
      *p = '/';
    else
      *p = name[i];
    ++p;
  }
  strcpy(p, ".lcbc");

  // assert(rsltlen == strlen(filename));

  return filename;
}

void loadModuleRec1(const char *moduleName, HashTable *q, u4 level);
Module *loadModuleHeader(FILE *f, const char *filename);
void loadModuleBody(FILE *f, Module *mdl);

void freeTodoEntry(char *key, void *value)
{
  free(key);
}

// Load the given module and all its dependencies.
void
loadModuleRec(const char *moduleName)
{
  HashTable *q;

  q = HashTable_create();

  loadModuleRec1(moduleName, q, 0);

  HashTable_destroy(q, freeTodoEntry);
}

// Postorder traversal
void
loadModuleRec1(const char *moduleName, HashTable* q, u4 level)
{
  char     *filename;
  Module   *mdl;
  FILE     *f;
  int       i;
  Word      todo;

  for (i = 0; i < level; i++) putchar(' ');
  printf("> Loading %s ...\n", moduleName);

  filename = moduleNameToFile(G_basepath, moduleName);

  todo = (Word)HashTable_lookup(q, filename);

  // Do nothing if this isn't the first time we're loading this
  // module.
  if (todo != 0) {
      free(filename);
      return;
  }

  if (!fileExists(filename)) {
    fprintf(stderr, "ERROR: File does not exist: %s\n",
            filename);
    exit(13);
  }

  f = fopen(filename, "rb");

  mdl = loadModuleHeader(f, filename);

  HashTable_insert(q, filename, (void*)~0);

  for (i = 0; i < mdl->numImports; i++)
    loadModuleRec1(mdl->imports[i], q, level + 1);

  loadModuleBody(f, mdl);

  fclose(f);

  HashTable_insert(G_loader->loadedModules, mdl->name, mdl);

  for (i = 0; i < level; i++) putchar(' ');
  printf("< DONE   (%s)\n", moduleName);
}

// Load the module and
Module *
loadModuleHeader(FILE *f, const char *filename)
{
  Module *mdl;
  char magic[5];
  u2 major, minor;
  u4 flags;
  u4 secmagic;
  u4 i;

  LC_ASSERT(f != NULL);

  fread(magic, 4, 1, f);
  magic[4] = '\0';
  if (strcmp(magic, "KHCB") != 0) {
    fprintf(stderr, "ERROR: Module '%s' is not a bytecode file. %s\n",
            filename, magic);
    exit(1);
  }

  mdl = malloc(sizeof(Module));

  major = fget_u2(f);
  minor = fget_u2(f);

  if (major != VERSION_MAJOR || minor != VERSION_MINOR) {
    fprintf(stderr, "ERROR: Module '%s' version mismatch.  Version: %d.%d, Expected: %d.%d\n",
            filename, major, minor, VERSION_MAJOR, VERSION_MINOR);
    exit(1);
  }

  flags = fget_u4(f);
  mdl->numStrings    = fget_u4(f);
  mdl->numInfoTables = fget_u4(f);
  mdl->numClosures   = fget_u4(f);
  mdl->numImports    = fget_u4(f);

  //printf("strings = %d, itbls = %d, closures = %d\n",
  //       mdl->numStrings, mdl->numInfoTables, mdl->numClosures);

  // String table starts with a 4 byte magic.
  secmagic = fget_u4(f);
  assert(secmagic == STR_SEC_HDR_MAGIC);

  mdl->strings = malloc(sizeof(StringTabEntry) * mdl->numStrings);
  for (i = 0; i < mdl->numStrings; i++) {
    loadStringTabEntry(f, &mdl->strings[i]);
  }

  //printStringTable(mdl->strings, mdl->numStrings);

  mdl->name = loadId(f, mdl->strings, ".");
  // printf("mdl name = %s\n", mdl->name);

  mdl->imports = malloc(sizeof(*mdl->imports) * mdl->numImports);
  for (i = 0; i < mdl->numImports; i++) {
    mdl->imports[i] = loadId(f, mdl->strings, ".");
    // printf("import: %s\n", mdl->imports[i]);
  }

  return mdl;
}

void
loadModuleBody(FILE *f, Module *mdl)
{
  u4 i;
  u4 secmagic;
  // Load closures
  secmagic = fget_u4(f);
  assert(secmagic == CLOS_SEC_HDR_MAGIC);

  for (i = 0; i < mdl->numInfoTables; ++i) {
    loadInfoTable2(f, mdl->strings,
                   G_loader->infoTables, G_loader->closures);
  }

  for (i = 0; i < mdl->numClosures; i++) {
    loadClosure(f, mdl->strings,
                G_loader->infoTables, G_loader->closures);
  }
}

Module *
loadModule(FILE *f, char *moduleName)
{
  Module *mdl;
  char magic[5];
  u2 major, minor;
  u4 flags;
  u4 secmagic;
  u4 i;
  
  LC_ASSERT(f != NULL);

  fread(magic, 4, 1, f);
  magic[4] = '\0';
  if (strcmp(magic, "KHCB") != 0) {
    fprintf(stderr, "ERROR: Module '%s' is not a bytecode file. %s\n",
            moduleName, magic);
    exit(1);
  }

  mdl = malloc(sizeof(Module));

  major = fget_u2(f);
  minor = fget_u2(f);
  
  if (major != VERSION_MAJOR || minor != VERSION_MINOR) {
    fprintf(stderr, "ERROR: Module '%s' version mismatch.  Version: %d.%d, Expected: %d.%d\n",
            moduleName, major, minor, VERSION_MAJOR, VERSION_MINOR);
    exit(1);
  }

  flags = fget_u4(f);
  mdl->numStrings    = fget_u4(f);
  mdl->numInfoTables = fget_u4(f);
  mdl->numClosures   = fget_u4(f);
  mdl->numImports    = fget_u4(f);

  printf("strings = %d, itbls = %d, closures = %d\n",
         mdl->numStrings, mdl->numInfoTables, mdl->numClosures);

  // String table starts with a 4 byte magic.
  secmagic = fget_u4(f);
  assert(secmagic == STR_SEC_HDR_MAGIC);

  mdl->strings = malloc(sizeof(StringTabEntry) * mdl->numStrings);
  for (i = 0; i < mdl->numStrings; i++) {
    loadStringTabEntry(f, &mdl->strings[i]);
  }

  printStringTable(mdl->strings, mdl->numStrings);

  mdl->name = loadId(f, mdl->strings, ".");
  printf("mdl name = %s\n", mdl->name);

  mdl->imports = malloc(sizeof(*mdl->imports) * mdl->numImports);
  for (i = 0; i < mdl->numImports; i++) {
    mdl->imports[i] = loadId(f, mdl->strings, ".");
    printf("import: %s\n", mdl->imports[i]);
  }

  // Load closures
  secmagic = fget_u4(f);
  assert(secmagic == CLOS_SEC_HDR_MAGIC);

  mdl->infoTables = HashTable_create();
  mdl->closures = HashTable_create();

  for (i = 0; i < mdl->numInfoTables; ++i) {
    loadInfoTable2(f, mdl->strings, mdl->infoTables, mdl->closures);
  }

  for (i = 0; i < mdl->numClosures; i++) {
    loadClosure(f, mdl->strings, mdl->infoTables, mdl->closures);
  }

  return mdl;
}

void
printCode(LcCode *code)
{
  u4 i; u4 nc = 0; BCIns *c = code->code;
  printf("  arity: %d\n", code->arity);
  printf("  frame: %d\n", code->framesize);
  printf("  literals:\n");
  for (i = 0; i < code->sizelits; i++) {
    printf("   %3d: ", i);
    switch (code->littypes[i]) {
    case LIT_INT:
      printf("%" FMT_Int, (WordInt)code->lits[i]);
      break;
    case LIT_STRING:
      printf("\"%s\"", (char*)code->lits[i]);
      break;
    case LIT_CLOSURE:
      printf("closure %" FMT_WordX, code->lits[i]);
      break;
    case LIT_INFO:
      printf("info %" FMT_WordX, code->lits[i]);
      break;
    default:
      printf("???");
    }
    printf("\n");
  }
  printf("  code:\n");
  while (nc < code->sizecode) {
    i = printInstruction(c);
    c += i;
    nc += i;
  }
}

void
printInfoTable(InfoTable* info0)
{
  switch (info0->type) {
  case CONSTR:
    {
      ConInfoTable* info = (ConInfoTable*)info0;
      printf("Constructor: %s, (%p)\n", info->name, info);
      printf("  tag: %d\n", info->i.tagOrBitmap);
      printf("  ptrs/nptrs: %d/%d\n",
             info->i.layout.payload.ptrs, info->i.layout.payload.nptrs);
    }
    break;
  case FUN:
    {
      FuncInfoTable *info = (FuncInfoTable*)info0;
      printf("Function: %s (%p)\n", info->name, info);
      printf("  ptrs/nptrs: %d/%d\n",
             info->i.layout.payload.ptrs, info->i.layout.payload.nptrs);
      printCode(&info->code);
    }
    break;
  case THUNK:
    {
      ThunkInfoTable *info = (ThunkInfoTable*)info0;
      printf("Thunk: %s (%p)\n", info->name, info);
      printf("  ptrs/nptrs: %d/%d\n",
             info->i.layout.payload.ptrs, info->i.layout.payload.nptrs);
      printCode(&info->code);
    }
    break;
  default:
    printf("Unknown info table\n");
  }
  printf("\n");
}

void
printModule(Module* mdl)
{
  printf("--- Module: %s ---\n", mdl->name);
  printf("  info tables: %d\n", mdl->numInfoTables);
  printf("  closures:    %d\n", mdl->numClosures);
  printf("--- Info Tables ----------------\n");
  HashTable_print(G_loader->infoTables, (HashValuePrinter)printInfoTable);
  printf("--- Closures -------------------\n");
  HashTable_print(G_loader->closures, (HashValuePrinter)printClosure);
}

void
printLoaderState()
{
  printf("--- Info Tables ----------------\n");
  HashTable_print(G_loader->infoTables, (HashValuePrinter)printInfoTable);
  printf("--- Closures -------------------\n");
  HashTable_print(G_loader->closures, (HashValuePrinter)printClosure);
}

/*
  // Closures are loaded using a two-pass process. First we load all
  // info tables.  These may contain pointers to closures.  We put the
  // addresses of all these pointers into a linked list.  If
  // `fwd_refs[i]' is `NULL' then there is no forward reference.
  // Otherwise, `*fwd_refs[i]' points to the next element in the list
  // and `fwd_refs[i]' is the address to be updated.
  void **fwd_refs = malloc(sizeof(void*) * mdl->numClosures);
  for (i = 0; i < mdl->numClosures; i++) fwd_refs[i] = NULL;

  InfoTable **itbls = malloc(sizeof(InfoTable*) * mdl->numInfoTables);
  
  for (i = 0; i < mdl->numInfoTables; ++i) {
    itbls[i] = loadInfoTable(f, mdl->strings, fwd_refs);
  }

  // TODO: literals may reference closures from other modules --
  // we need a way to deal with this.  (Including circular dependencies.)
  
  Closure **closures = malloc(sizeof(Closure*) * mdl->numClosures);
  // Load closures.
  mdl->closures = HashTable_create();
  for (i = 0; i < mdl->numClosures; i++) {
    char *clos_name = fget_string(f);
    u4 payloadsize = fget_varuint(f);
    u4 itbl = fget_varuint(f);
    Closure *cl = allocStaticClosure(sizeof(ClosureHeader) + 
                                     payloadsize * sizeof(Word));
    setInfo(cl, itbls[itbl]);
    for (j = 0; j < payloadsize; j++) {
      u1 dummy;
      loadLiteral(f, &dummy, &cl->payload[j], mdl->strings, fwd_refs);
    }
    closures[i] = cl;
    HashTable_insert(mdl->closures, clos_name, cl);
  }

  // fixup forward refs
  for (i = 0; i < mdl->numClosures; i++) {
    void **p, *next;
    for (p = fwd_refs[i]; p != NULL; p = next) {
      next = *p;
      *p = (void*)closures[i];
    }
  }
  free(itbls);
  free(closures);

  return mdl;
}
*/
#define MAX_PARTS  255

// Load an identifier from the file.  It is encoded as a non-empty
// sequence of references to the string table. 
//
// The separator is put between each string name.
char *
loadId(FILE *f, const StringTabEntry *strings, const char* sep)
{
  u4 numparts;
  u4 parts[MAX_PARTS];
  u4 seplen = strlen(sep);
  u4 i, len = 0;
  char *ident, *p;

  numparts = fget_varuint(f);
  assert(numparts > 0);

  for (i = 0; i < numparts; ++i) {
    u4 idx = fget_varuint(f);
    len += strings[idx].len + seplen;
    parts[i] = idx;
  }
  len -= seplen;
  
  ident = malloc(sizeof(char) * len + 1);
  p = ident;
  for (i = 0; i < numparts; i++) {
    len = strings[parts[i]].len;
    memcpy(p, strings[parts[i]].str, len);
    p += len;
    if (i != numparts - 1) {
      memcpy(p, sep, seplen);
      p += seplen;
    }
  }
  *p = '\0';
  //printf("loadId: %lx, %s\n", ftell(f), ident);
  return ident;
}

BCIns
loadBCIns(FILE *f)
{
  return fget_u4(f);
}

InfoTable *
loadInfoTable(FILE *f, const StringTabEntry *strings, void **fwd_refs)
{
  u2 cl_type = fget_varuint(f);
  switch (cl_type) {
  case CONSTR:
    // A statically allocated constructor
    {
      ConInfoTable *info = allocInfoTable(sizeof(ConInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = fget_varuint(f);  // tag
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      return (InfoTable*)info;
    }
    break;
  case FUN:
    {
      FuncInfoTable *info = allocInfoTable(sizeof(FuncInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = 0; // TODO: anything useful to put in here?
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      loadCode(f, &info->code, strings, fwd_refs);
      return (InfoTable*)info;
    }
    break;
  case THUNK:
    {
      ThunkInfoTable *info = allocInfoTable(sizeof(ThunkInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = 0; // TODO: anything useful to put in here?
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      loadCode(f, &info->code, strings, fwd_refs);
      return (InfoTable*)info;
    }
    break;
  default:
    fprintf(stderr, "ERROR: Unknown info table type (%d)", cl_type);
    exit(1);
  }
}

InfoTable *
loadInfoTable2(FILE *f, const StringTabEntry *strings,
               HashTable *itbls, HashTable *closures)
{
  u4 magic = fget_u4(f);
  assert(magic == INFO_MAGIC);

  char *itbl_name = loadId(f, strings, ".");
  u2 cl_type = fget_varuint(f);
  InfoTable *new_itbl = NULL;
  FwdRefInfoTable *old_itbl = HashTable_lookup(itbls, itbl_name);

  //printf("Loading info table: %s\n", itbl_name);
  if (old_itbl != NULL) {
    printf("Old itbl: ");
    printInfoTable((InfoTable*)old_itbl);
  }

  switch (cl_type) {
  case CONSTR:
    // A statically allocated constructor
    {
      ConInfoTable *info = allocInfoTable(sizeof(ConInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = fget_varuint(f);  // tag
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      new_itbl = (InfoTable*)info;
    }
    break;
  case FUN:
    {
      FuncInfoTable *info = allocInfoTable(sizeof(FuncInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = 0; // TODO: anything useful to put in here?
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      loadCode2(f, &info->code, strings, itbls, closures);
      new_itbl = (InfoTable*)info;
    }
    break;
  case THUNK:
    {
      ThunkInfoTable *info = allocInfoTable(sizeof(ThunkInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = 0; // TODO: anything useful to put in here?
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      loadCode2(f, &info->code, strings, itbls, closures);
      new_itbl = (InfoTable*)info;
    }
    break;
  default:
    fprintf(stderr, "ERROR: Unknown info table type (%d)", cl_type);
    exit(1);
  }
  // new_itbl is the new info table.  There may have been forward
  // references (even during loading the code for this info table).
  if (old_itbl != NULL) {
    printf("Forward reference for: %s\n", itbl_name);
    void **p, *next;
    LC_ASSERT(old_itbl->i.type == INVALID_OBJECT);
    
    for (p = old_itbl->next; p != NULL; p = next) {
      next = *p;
      *p = (void*)new_itbl;
    }

    // TODO: fixup forward refs
    free(old_itbl);
    HashTable_update(itbls, itbl_name, new_itbl);
    free(itbl_name);
  } else {
    HashTable_insert(itbls, itbl_name, new_itbl);
  }
    
  return new_itbl;
}

void
loadLiteral(FILE *f, u1 *littype /*out*/, Word *literal /*out*/,
            const StringTabEntry *strings, void **fwd_refs)
{
  u4 i;
  *littype = fget_u1(f);
  switch (*littype) {
  case LIT_INT:
    *literal = (Word)fget_varsint(f);
    break;
  case LIT_STRING:
    i = fget_varuint(f);
    *literal = (Word)strings[i].str;
    break;
  case LIT_CLOSURE:
    i = fget_varuint(f);
    // Closures not yet added.  Add to linked FIXUP list.
    *literal = (Word)fwd_refs[i];
    fwd_refs[i] = literal;
    break;
  default:
    fprintf(stderr, "ERROR: Unknown literal type (%d).", *littype);
    exit(1);
  }
}

void
loadLiteral2(FILE *f, u1 *littype /*out*/, Word *literal /*out*/,
             const StringTabEntry *strings, HashTable *itbls,
	     HashTable *closures)
{
  u4 i;
  *littype = fget_u1(f);
  switch (*littype) {
  case LIT_INT:
    *literal = (Word)fget_varsint(f);
    break;
  case LIT_STRING:
    i = fget_varuint(f);
    *literal = (Word)strings[i].str;
    break;
  case LIT_CLOSURE:
    { char *clname = loadId(f, strings, ".");
      Closure *cl = HashTable_lookup(closures, clname);
      if (cl == NULL) {
        // 1st forward ref, create the link
        cl = malloc(sizeof(ClosureHeader) + sizeof(Word));
        setInfo(cl, NULL);
        cl->payload[0] = (Word)literal;
        *literal = (Word)NULL;
        HashTable_insert(closures, clname, cl);
      } else if (getInfo(cl) == NULL) {
        // forward ref (not the first), insert into linked list
        *literal = (Word)cl->payload[0];
        cl->payload[0] = (Word)literal;
        free(clname);
      } else {
        *literal = (Word)cl;
        free(clname);
      }
    }
    break;
  case LIT_INFO:
    { char *infoname = loadId(f, strings, ".");
      InfoTable *info = HashTable_lookup(itbls, infoname);
      FwdRefInfoTable *info2;
      if (info == NULL) {
	// 1st forward ref
	info2 = malloc(sizeof(FwdRefInfoTable));
	info2->i.type = INVALID_OBJECT;
	info2->next = (void**)literal;
	*literal = (Word)NULL;
	HashTable_insert(itbls, infoname, info2);
      } else if (info->type == INVALID_OBJECT) {
	// subsequent forward ref
	info2 = (FwdRefInfoTable*)info;
	*literal = (Word)info2->next;
	info2->next = (void**)literal;
	free(infoname);
      } else {
	*literal = (Word)info;
	free(infoname);
      }
    } 
    break;
  default:
    fprintf(stderr, "ERROR: Unknown literal type (%d).", *littype);
    exit(1);
  }
}

Closure *
loadClosure(FILE *f, const StringTabEntry *strings,
            HashTable *itbls, HashTable *closures)
{
  u4 i;
  u4 magic = fget_u4(f);
  assert(magic == CLOSURE_MAGIC);
  char *clos_name = loadId(f, strings, ".");
  u4 payloadsize = fget_varuint(f);
  char *itbl_name = loadId(f, strings, ".");

  Closure *cl = allocStaticClosure(sizeof(ClosureHeader) + 
                                   payloadsize * sizeof(Word));
  Closure *fwd_ref; 
  InfoTable* info = HashTable_lookup(itbls, itbl_name);
  LC_ASSERT(info != NULL && info->type != INVALID_OBJECT);

  // Fill in closure payload.  May create forward references to
  // the current closure.
  setInfo(cl, info);
  free(itbl_name);
  for (i = 0; i < payloadsize; i++) {
    u1 dummy;
    loadLiteral2(f, &dummy, &cl->payload[i], strings, itbls, closures);
  }
  
  fwd_ref = HashTable_lookup(closures, clos_name);
  if (fwd_ref != NULL) {

    // fixup forward refs
    void **p, *next;
    for (p = (void**)fwd_ref->payload[0]; p != NULL; p = next) {
      next = *p;
      *p = (void*)cl;
    }

    free(fwd_ref);
    HashTable_update(closures, clos_name, cl);
    // The key has been allocated by whoever installed the first
    // forward reference.
    free(clos_name);

  } else {

    HashTable_insert(closures, clos_name, cl);

  }
  return cl;
}

void
loadCode2(FILE *f, LcCode *code/*out*/,
          const StringTabEntry *strings,
          HashTable *itbls, HashTable *closures)
{
  u4 i;
  code->framesize = fget_varuint(f);
  code->arity = fget_varuint(f);
  code->sizelits = fget_varuint(f);
  code->sizecode = fget_varuint(f);

  code->lits = malloc(sizeof(*code->lits) * code->sizelits);
  code->littypes = malloc(sizeof(u1) * code->sizelits);
  for (i = 0; i < code->sizelits; ++i) {
    loadLiteral2(f, &code->littypes[i], &code->lits[i], strings, itbls, closures);
  }
  code->code = malloc(sizeof(BCIns) * code->sizecode);
  for (i = 0; i < code->sizecode; i++) {
    code->code[i] = loadBCIns(f);
  }
}

void
loadCode(FILE *f, LcCode *code,
         const StringTabEntry *strings, void **fwd_refs)
{
  u4 i;
  code->framesize = fget_varuint(f);
  code->arity = fget_varuint(f);
  code->sizelits = fget_varuint(f);
  code->sizecode = fget_varuint(f);
  
  code->lits = malloc(sizeof(*code->lits) * code->sizelits);
  code->littypes = malloc(sizeof(u1) * code->sizelits);
  for (i = 0; i < code->sizelits; ++i) {
    loadLiteral(f, &code->littypes[i], &code->lits[i], strings, fwd_refs);
  }
  code->code = malloc(sizeof(BCIns) * code->sizecode);
  fread(code->code, sizeof(BCIns), code->sizecode, f);
}

void
printStringTable(StringTabEntry *tbl, u4 len)
{
  u4 i;
  printf("String Table:\n");
  for (i = 0; i < len; i++) {
    printf("  %5d: %s\n", i, tbl[i].str);
  }
}


int
main(int argc, char *argv[])
{
  const char *input_file;

  if (argc <= 1)
    input_file = "Bc0005";
  else
    input_file = argv[1];

  initLoader();

  //  printf("%s\n\n", moduleNameToFile("test","Foo.Bar.Baz"));

  //  FILE *f = fopen(input_file, "rb");

  //  Module *m = loadModule(f, "MyModule");

  loadModuleRec(input_file);

  //  printf("Name: %s\n", m->name);
  //  printModule(m);
  printLoaderState();

  return 0;
}
