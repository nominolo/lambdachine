#include "Loader.h"
#include "HashTable.h"
#include "InfoTables.h"
#include "FileUtils.h"
#include "PrintClosure.h"
#include "StorageManager.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>

#define VERSION_MAJOR  0
#define VERSION_MINOR  1

#define BUFSIZE 512

//--------------------------------------------------------------------
// Global variables

GlobalLoaderState *G_loader = NULL;
char *G_basepath = NULL;

//--------------------------------------------------------------------

//
void printModule(Module *mdl);
void printStringTable(StringTabEntry *tbl, u4 len);
void bufferOverflow(int sz, int bufsize);

void initBasepath();

void loadStringTabEntry(FILE *, StringTabEntry */*out*/);
char *loadId(FILE *, const StringTabEntry *, const char* sep);
void loadCode(const char *filename, FILE *, LcCode */*out*/, const StringTabEntry *,
               HashTable *itbls, HashTable *closures);
void loadLiteral(const char *filename,
                 FILE *, u1 *littype /*out*/, Word *literal /*out*/,
                  const StringTabEntry *,
                  HashTable *itbls, HashTable *closures);
InfoTable *loadInfoTable(const char *filename,
                         FILE *, const StringTabEntry*,
                          HashTable *itbls, HashTable *closures);
Closure *loadClosure(const char *filename, FILE *, const StringTabEntry *,
                     HashTable *itbls, HashTable *closures);

//--------------------------------------------------------------------

void
bufferOverflow(int sz, int bufsize)
{
  fprintf(stderr, "FATAL: buffer overflow (%d, max: %d)\n",
          sz, bufsize - 1);
  exit(1);
}

void
initBasepath()
{
  char buf[BUFSIZE];
  int res;
  char *cwd = getcwd(buf, BUFSIZE);

  if (cwd == NULL) {
    fprintf(stderr, "Could not get working directory\n");
    exit(1);
  }

  res = asprintf(&G_basepath, "%s/tests/", cwd);

  if (res <= 0) {
    fprintf(stderr, "Could not initialise base path.\n");
    exit(1);
  }
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

Closure *
lookupClosure(const char *name)
{
  HashTable_lookup(G_loader->closures, name);
}

int
isModuleLoaded(const char *moduleName)
{
  Module *mdl;
  mdl = HashTable_lookup(G_loader->loadedModules, moduleName);

  // If the module is currently being loaded, then its string table
  // will be non-empty.

  return (mdl != NULL) && (mdl->strings == NULL);
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

  res = snprintf(path, BUFSIZE, "%s/lib/lambdachine/packages/%s/%s.kbc",
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

void
loadStringTabEntry(FILE *f, StringTabEntry *e /*out*/)
{
  e->len = fget_varuint(f);
  e->str = malloc(e->len + 1);
  fread(e->str, 1, e->len, f);
  e->str[e->len] = '\0';
}

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

void loadModule_aux(const char *moduleName, u4 level);
Module *loadModuleHeader(FILE *f, const char *filename);
void loadModuleBody(const char *filename, FILE *f, Module *mdl);
void ensureNoForwardRefs();

// Load the given module and all its dependencies.
void
loadModule(const char *moduleName)
{
  HashTable *q;

  loadModule_aux(moduleName, 0);

  ensureNoForwardRefs();
}

const char *wired_in_packages[] =
  { "ghc-prim", "integer-gmp", "base" };

char *
findModule(const char *moduleName)
{
  u4     i;
  char  *filename, *base;
  // 1. Try to find module in base directory

  filename = moduleNameToFile(G_basepath, moduleName);

  if (fileExists(filename)) {
    return filename;
  }
  free(filename);

  for (i = 0; i < countof(wired_in_packages); i++) {
    asprintf(&base, "%s/%s", G_basepath, wired_in_packages[i]);
    filename = moduleNameToFile(base, moduleName);
    free(base);
    if (fileExists(filename)) {
      return filename;
    } else {
      free(filename);
    }
  }

  fprintf(stderr, "ERROR: Could not find module: %s\n",
          moduleName);
  exit(13);
}

// Postorder traversal
void
loadModule_aux(const char *moduleName, u4 level)
{
  char     *filename;
  Module   *mdl;
  FILE     *f;
  int       i;
  Word      todo;

  mdl = (Module*)HashTable_lookup(G_loader->loadedModules, moduleName);

  if (mdl != NULL) {
    // Module is either already loaded, or currently in process of
    // being loaded.
      return;
  }

  filename = findModule(moduleName);

  for (i = 0; i < level; i++) putchar(' ');
  printf("> Loading %s ...(%s)\n", moduleName, filename);

  f = fopen(filename, "rb");
  if (f == NULL) {
    fprintf(stderr, "ERROR: Could not open file: %s\n",
            filename);
    exit(14);
  }

  mdl = loadModuleHeader(f, filename);

  HashTable_insert(G_loader->loadedModules, moduleName, mdl);

  // Load dependencies first.  This avoids creating many forward
  // references.  The downside is that we keep more file descriptors
  // open.
  for (i = 0; i < mdl->numImports; i++)
    loadModule_aux(mdl->imports[i], level + 1);

  loadModuleBody(filename, f, mdl);

  fclose(f);

  // We now don't need the string table anymore.
  free(mdl->strings);
  mdl->strings = NULL;

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
loadModuleBody(const char *filename, FILE *f, Module *mdl)
{
  u4 i;
  u4 secmagic;
  // Load closures
  secmagic = fget_u4(f);
  assert(secmagic == CLOS_SEC_HDR_MAGIC);

  for (i = 0; i < mdl->numInfoTables; ++i) {
    loadInfoTable(filename, f, mdl->strings,
                   G_loader->infoTables, G_loader->closures);
  }

  for (i = 0; i < mdl->numClosures; i++) {
    loadClosure(filename, f, mdl->strings,
                G_loader->infoTables, G_loader->closures);
  }
}


void
printModule(Module* mdl)
{
  printf("--- Module: %s ---\n", mdl->name);
  printf("  info tables: %d\n", mdl->numInfoTables);
  printf("  closures:    %d\n", mdl->numClosures);
  printf("--- Info Tables ----------------\n");
  HashTable_print(G_loader->infoTables, (HashValuePrinter)printInfoTable);
  printf("--- Closures (%d) ---------------\n", HashTable_entries(G_loader->closures));
  HashTable_print(G_loader->closures, (HashValuePrinter)printClosure);
}

void
printClosure1(void *unused, const char *const name, Closure *cl)
{
  printf("%s: [%p]: ", name, cl);
  printClosure(cl);
}

void
printLoaderState()
{
  printf("--- Info Tables ----------------\n");
  HashTable_print(G_loader->infoTables, (HashValuePrinter)printInfoTable);
  printf("--- Closures (%d) ---------------\n", HashTable_entries(G_loader->closures));
  HashTable_foreach(G_loader->closures,
                    (HashValueCallback)printClosure1, NULL);
  //  HashTable_print(G_loader->closures, (HashValuePrinter)printClosure);
}

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
loadInfoTable(const char *filename,
              FILE *f, const StringTabEntry *strings,
              HashTable *itbls, HashTable *closures)
{
  u4 magic = fget_u4(f);
  assert(magic == INFO_MAGIC);

  char *itbl_name = loadId(f, strings, ".");
  u2 cl_type = fget_varuint(f);
  InfoTable *new_itbl = NULL;
  FwdRefInfoTable *old_itbl = HashTable_lookup(itbls, itbl_name);

  if (old_itbl && old_itbl->i.type != INVALID_OBJECT) {
    fprintf(stderr, "ERROR: Duplicate info table: %s\n",
            itbl_name);
    exit(1);
  }

  switch (cl_type) {
  case CONSTR:
    // A statically allocated constructor
    {
      ConInfoTable *info = allocInfoTable(wordsof(ConInfoTable));
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
      FuncInfoTable *info = allocInfoTable(wordsof(FuncInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = 0; // TODO: anything useful to put in here?
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      loadCode(filename, f, &info->code, strings, itbls, closures);
      new_itbl = (InfoTable*)info;
    }
    break;
  case THUNK:
    {
      ThunkInfoTable *info = allocInfoTable(wordsof(ThunkInfoTable));
      info->i.type = cl_type;
      info->i.tagOrBitmap = 0; // TODO: anything useful to put in here?
      info->i.layout.payload.ptrs = fget_varuint(f);
      info->i.layout.payload.nptrs = fget_varuint(f);
      info->name = loadId(f, strings, ".");
      loadCode(filename, f, &info->code, strings, itbls, closures);
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
    //printf("Forward reference for: %s\n", itbl_name);
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
loadLiteral(const char *filename,
            FILE *f, u1 *littype /*out*/, Word *literal /*out*/,
            const StringTabEntry *strings, HashTable *itbls,
            HashTable *closures)
{
  u4 i;
  *littype = fget_u1(f);
  switch (*littype) {
  case LIT_INT:
    *literal = (Word)fget_varsint(f);
    break;
  case LIT_CHAR:
    *literal = (Word)fget_varuint(f);
    break;
  case LIT_WORD:
    *literal = (Word)fget_varuint(f);
    break;
  case LIT_FLOAT:
    *literal = (Word)fget_u4(f);
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
    fprintf(stderr, "ERROR: Unknown literal type (%d) "
            "when loading file: %s\n",
            *littype, filename);
    exit(1);
  }
}

Closure *
loadClosure(const char *filename,
            FILE *f, const StringTabEntry *strings,
            HashTable *itbls, HashTable *closures)
{
  u4 i;
  u4 magic = fget_u4(f);
  assert(magic == CLOSURE_MAGIC);
  char *clos_name = loadId(f, strings, ".");
  u4 payloadsize = fget_varuint(f);
  char *itbl_name = loadId(f, strings, ".");

  Closure *cl = allocStaticClosure(wordsof(ClosureHeader) + payloadsize);
  Closure *fwd_ref;
  InfoTable* info = HashTable_lookup(itbls, itbl_name);
  LC_ASSERT(info != NULL && info->type != INVALID_OBJECT);

  // Fill in closure payload.  May create forward references to
  // the current closure.
  setInfo(cl, info);
  free(itbl_name);
  for (i = 0; i < payloadsize; i++) {
    u1 dummy;
    loadLiteral(filename, f, &dummy, &cl->payload[i], strings, itbls, closures);
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
ensureNoForwardRefInfo(int *i, const char *const name, InfoTable *info)
{
  if (info->type == INVALID_OBJECT) {
    fprintf(stderr, "Unresolved info table: %s\n", name);
    (*i)++;
  }
}

void
ensureNoForwardRefClosure(int *i, const char *const name, Closure *cl)
{
  if (getInfo(cl) == NULL) {
    fprintf(stderr, "Unresolved closure: %s\n", name);
    (*i)++;
  }
}

void ensureNoForwardRefs()
{
  int i = 0;

  HashTable_foreach(G_loader->infoTables,
                    (HashValueCallback)ensureNoForwardRefInfo,
                    &i);

  HashTable_foreach(G_loader->closures,
                    (HashValueCallback)ensureNoForwardRefClosure,
                    &i);

  if (i > 0) {
    fprintf(stderr, "ERROR: There %d were unresolved references.\n", i);
    exit(15);
  }
}

void
loadCode(const char *filename,
         FILE *f, LcCode *code/*out*/,
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
    loadLiteral(filename, f, &code->littypes[i], &code->lits[i], strings, itbls, closures);
  }
  code->code = malloc(sizeof(BCIns) * code->sizecode);
  for (i = 0; i < code->sizecode; i++) {
    code->code[i] = loadBCIns(f);
  }
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
