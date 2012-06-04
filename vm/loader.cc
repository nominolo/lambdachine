#include "loader.hh"
#include "fileutils.hh"

#include <iostream>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

using namespace std;

#define DLOG(...) \
  if (DEBUG_COMPONENTS & DEBUG_LOADER) { \
    fprintf(stderr, "LD: " __VA_ARGS__); }

_START_LAMBDACHINE_NAMESPACE

BytecodeFile::BytecodeFile(const char *filename)
  : name_(filename), f_(NULL) {
  LC_ASSERT(filename != NULL);
}

bool BytecodeFile::open() {
  f_ = fopen(name_, "rb");
  if (!f_) {
    fprintf(stderr, "ERROR: Could not open file %s\n", name_);
    return false;
  }
  return true;
}

void BytecodeFile::close() {
  fclose(f_);
}

uint32_t BytecodeFile::get_u4() {
  uint32_t hh = get_u1();
  uint32_t hl = get_u1();
  uint32_t lh = get_u1();
  uint32_t ll = get_u1();
  return MSB_u4(hh, hl, lh, ll);
}

// Decode an unsigned variable length integer.
Word BytecodeFile::get_varuint_slow(Word i) {
  uint8_t shift = 7;
  Word b;
  do {
    b = get_u1();
    i = i | (b & 0x7f) << shift;
    shift += 7;
  } while (b & 0x80);

  return i;
}

bool BytecodeFile::magic(const char *bytes) {
  for (const char *p = bytes; *p != '\0'; ++p) {
    char c = get_u1();
    if (c != *p) return false;
  }
  return true;
}


#define VERSION_MAJOR  0
#define VERSION_MINOR  1

Loader::Loader(MemoryManager *mm, const char* basepaths) 
  : mm_(mm), loadedModules_(10), basepaths_(NULL) {
  initBasePath(basepaths);
}

Loader::~Loader() {
  STRING_MAP(Module*)::iterator it;
  for (it = loadedModules_.begin();
       it != loadedModules_.end(); ++it) {
    delete it->second;
  }
}

// Linked list of strings
struct _BasePathEntry {
  const char    *path;
  BasePathEntry *next;
};

void Loader::appendBasePathEntry(BasePathEntry *entry) {
  BasePathEntry **p = &basepaths_;
  while (*p != NULL) {
    p = &(*p)->next;
  }
  *p = entry;
  entry->next = NULL;
}

void Loader::addBasePath(const char *path)
{
  BasePathEntry *b;
  char buf[PATH_MAX + 1];
  const char *real = realpath(path, buf);

  if (real == NULL) {
    fprintf(stderr, "WARNING: Could not resolve base path: %s\n",
            path);
    return;
  }

  b = new BasePathEntry();
  b->next = NULL;
  size_t len = strlen(real);
  b->path = static_cast<const char*>(mm_->allocString(len));
  memmove((void*)b->path, real, len + 1);

  appendBasePathEntry(b);
}

void Loader::initBasePath(const char *path)
{
  if (path == NULL) path = "";
  const char *path_end;
  char buf[PATH_MAX + 1];

  while (1) {
    int path_len;
    path_end = strchr(path, ':');
    path_len = (path_end != NULL) ? path_end - path : strlen(path);
    int is_last_path = path_end == NULL;
    
    if (path_len == 0) {
      if (is_last_path) {
        // Add default path (current working directory)
        addBasePath(".");
      }
    } else {
      if (path_len > PATH_MAX) {
        // Skip if path is too long
        fprintf(stderr, "WARNING: Path too long - ignoring: %s\n", path);
      } else {
        memmove((void*)buf, path, path_len);
        buf[path_len] = '\0';
        addBasePath(buf);
      }
    }

    if (is_last_path)
      break;
    else
      path = path_end + 1;
  }
}

const char *Loader::basePath(unsigned int index) const {
  BasePathEntry *e = basepaths_;
  while (index > 0 && e != NULL) {
    --index;
    e = e->next;
  }
  if (e == NULL)
    return NULL;
  return e->path;
}

static char *
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
  filename = new char[rsltlen + 1];

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

const char *const wired_in_packages[] =
  { "ghc-prim", "integer-gmp", "base" };

char *Loader::findModule(const char *moduleName) {
  char  *filename;
  char   base[PATH_MAX];
  BasePathEntry *b = basepaths_;
  
  while (b) {
    // 1. Try to find module in base directory
    filename = moduleNameToFile(b->path, moduleName);
    
    DLOG(".. Searching for `%s' in `%s'\n", moduleName, filename);

    if (fileExists(filename)) {
      return filename;
    }
    delete[](filename);

    // TODO: wired-in packages should probably only live in one
    // directory.  Otherwise, the user could accidentally shadow them.

    for (size_t i = 0; i < countof(wired_in_packages); i++) {
      snprintf(base, PATH_MAX, "%s/%s",
               b->path, wired_in_packages[i]);
      filename = moduleNameToFile(base, moduleName);
      DLOG(".. Searching for `%s' in `%s'\n", moduleName, filename);
      if (fileExists(filename)) {
        return filename;
      } else {
        delete[](filename);
      }
    }

    b = b->next;
  }

  fprintf(stderr, "ERROR: Could not find module: %s\n",
          moduleName);
  return NULL;
}

static bool ensureNoForwardRefs() {
  return true;
}

bool Loader::loadModule(const char *moduleName)
{
  return loadModule(moduleName, 0) && ensureNoForwardRefs();
}

bool Loader::loadModule(const char *moduleName, int level) {
  char *filename;
  Module *mdl;
  
  mdl = loadedModules_[moduleName];
  
  if (mdl != NULL) {            // Already loaded
    DLOG("[%d] Already loaded: %s\n", level, moduleName);
    return true;
  }
 
  filename = findModule(moduleName);
  if (!filename)
    return false;

  DLOG("[%d] Loading %s ... (%s)\n", level, moduleName, filename);

  BytecodeFile f(filename);
  if (!f.open())
    return false;

  mdl = loadModuleHeader(f);
  if (!mdl)
    return false;

  loadedModules_[moduleName] = mdl;

  // Load dependencies first.  This avoids creating many forward
  // references.  The downside is that we keep more file descriptors
  // open.
  for (uint32_t i = 0; i < mdl->numImports_; i++)
    loadModule(mdl->imports_[i], level + 1);

  // loadModuleBody(filename, f, mdl);

  f.close();
  DLOG("[%d] DONE (%s)\n", level, moduleName);
  delete[] filename;
  return true;
}

void Loader::loadStringTabEntry(BytecodeFile &f, StringTabEntry *e /*out*/)
{
  e->len = f.get_varuint();
  e->str = f.get_string(e->len);
}

#define MAX_PARTS  255

// Load an identifier from the file.  It is encoded as a non-empty
// sequence of references to the string table.
//
// The separator is put between each string name.
//
// TODO: There is currently some duplication when loading IDs,
// because each module has its own string table.
const char *Loader::loadId(BytecodeFile &f, const StringTabEntry *strings,
                           const char* sep)
{
  u4 numparts;
  u4 parts[MAX_PARTS];
  u4 seplen = strlen(sep);
  u4 i, len = 0;
  char *ident, *p;

  numparts = f.get_varuint();
  assert(numparts > 0);

  for (i = 0; i < numparts; ++i) {
    u4 idx = f.get_varuint();
    len += strings[idx].len + seplen;
    parts[i] = idx;
  }
  len -= seplen;

  ident = mm_->allocString(len);
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
  DLOG("loadId: %lx, %s, " COLOURED(COL_BLUE, "%p") "\n",
       f.offset(), ident, ident);
  return ident;
}


Module *Loader::loadModuleHeader(BytecodeFile &f)
{
  Module *mdl;
  u2 major, minor;
  u4 secmagic;
  u4 i;

  if (!f.magic("KHCB")) {
    fprintf(stderr, "ERROR: Module '%s' is not a bytecode file.\n",
            f.filename());
    return NULL;
  }

  mdl = new Module();

  major = f.get_u2();
  minor = f.get_u2();

  if (major != VERSION_MAJOR || minor != VERSION_MINOR) {
    fprintf(stderr,
            "ERROR: Module '%s' version mismatch. "
            "Version: %d.%d, Expected: %d.%d\n",
            f.filename(), major, minor, VERSION_MAJOR, VERSION_MINOR);
    return NULL;
  }

  mdl->flags_         = f.get_u4();
  mdl->numStrings_    = f.get_u4();
  mdl->numInfoTables_ = f.get_u4();
  mdl->numClosures_   = f.get_u4();
  mdl->numImports_    = f.get_u4();

  //printf("strings = %d, itbls = %d, closures = %d\n",
  //       mdl->numStrings, mdl->numInfoTables, mdl->numClosures);

  // String table starts with a 4 byte magic.
  secmagic = f.get_u4();
  assert(secmagic == STR_SEC_HDR_MAGIC);

  mdl->strings_ = new StringTabEntry[mdl->numStrings_];
  for (i = 0; i < mdl->numStrings_; i++) {
    loadStringTabEntry(f, &mdl->strings_[i]);
  }

  //printStringTable(mdl->strings, mdl->numStrings);

  mdl->name_ = loadId(f, mdl->strings_, ".");
  // printf("mdl name = %s\n", mdl->name);

  mdl->imports_ = new const char*[mdl->numImports_];
  for (i = 0; i < mdl->numImports_; i++) {
    mdl->imports_[i] = loadId(f, mdl->strings_, ".");
    // printf("import: %s\n", mdl->imports[i]);
  }

  return mdl;
}

// void loadModule

_END_LAMBDACHINE_NAMESPACE
