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
  : mm_(mm), loadedModules_(10), infoTables_(100), closures_(100),
    basepaths_(NULL) {
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

  loadModuleBody(f, mdl);

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
  // DLOG("loadId: %lx, %s, " COLOURED(COL_BLUE, "%p") "\n",
  //      f.offset(), ident, ident);
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

void Loader::loadModuleBody(BytecodeFile &f, Module *mdl) {
  bool module_header_magic_ok = f.magic("BCCL");
  assert(module_header_magic_ok);

  DLOG("Loading module body...");

  for (u4 i = 0; i < mdl->numInfoTables_; ++i) {
    loadInfoTable(f, mdl->strings_);
  }

  for (u4 i = 0; i < mdl->numClosures_; ++i) {
    loadClosure(f, mdl->strings_);
  }
}

#define FMT_INFO_PTR  COLOURED(COL_YELLOW, "%p")
#define FMT_FWD_PTR   COLOURED(COL_RED, "%p")
#define FMT_CLOS_PTR  COLOURED(COL_GREEN, "%p")

InfoTable *Loader::loadInfoTable(BytecodeFile &f,
                                 const StringTabEntry *strings) {
  if (!f.magic("ITBL")) {
    fprintf(stderr, "Wrong magic for info table\n");
    exit(1);
  }

  const char *itbl_name = loadId(f, strings, ".");
  u2 cl_type = f.get_varuint();
  InfoTable *new_itbl = NULL;
  FwdRefInfoTable *old_itbl =
    static_cast<FwdRefInfoTable*>(infoTables_[itbl_name]);

  if (old_itbl && old_itbl->type() != INVALID_OBJECT) {
    fprintf(stderr, "ERROR: Duplicate info table: %s\n", itbl_name);
    exit(1);
  }

  switch (cl_type) {
  case CONSTR:
    // A statically allocated constructor
    {
      DLOG("itbl.CONSTR %s\n", itbl_name);
      ConInfoTable *info = static_cast<ConInfoTable*>
        (mm_->allocInfoTable(wordsof(ConInfoTable)));
      info->type_ = cl_type;
      info->tagOrBitmap_ = f.get_varuint();  // tag
      Word sz = f.get_varuint();
      assert(sz <= 32);
      info->size_ = sz;
      info->layout_.bitmap = sz > 0 ? f.get_u4() : 0;
      // info->i.layout.payload.ptrs = fget_varuint(f);
      // info->i.layout.payload.nptrs = fget_varuint(f);
      info->name_ = loadId(f, strings, ".");
      new_itbl = (InfoTable*)info;
    }
    break;
  case CAF:
  case THUNK:
  case FUN:
    {
      DLOG("itbl.FUN/CAF/THK %s\n", itbl_name);
      CodeInfoTable *info = static_cast<CodeInfoTable*>
        (mm_->allocInfoTable(wordsof(CodeInfoTable)));
      info->type_ = cl_type;
      info->tagOrBitmap_ = 0; // TODO: anything useful to put in here?
      Word sz = f.get_varuint();
      assert(sz <= 32);
      info->size_ = sz;
      info->layout_.bitmap = sz > 0 ? f.get_u4() : 0;
      info->name_ = loadId(f, strings, ".");
      loadCode(f, &info->code_, strings);
      new_itbl = (InfoTable*)info;
    }
    break;
  default:
    fprintf(stderr, "ERROR: Unknown info table type (%d)", cl_type);
    exit(1);
  }
  
  fixInfoTableForwardReference(itbl_name, new_itbl);

  DLOG("loadInfoTable: %s " COLOURED(COL_YELLOW, "%p") "\n",
       itbl_name, new_itbl);
  infoTables_[itbl_name] = new_itbl;

  return new_itbl;
}

void Loader::loadCode(BytecodeFile &f, Code *code /* out */,
                      const StringTabEntry *strings) {
  u2 *bitmaps = NULL;
  DLOG("loadCode: %p\n", code);
  code->framesize = f.get_varuint();
  code->arity = f.get_varuint();
  code->sizelits = f.get_varuint();
  code->sizecode = f.get_u2();
  code->sizebitmaps = f.get_u2();
  code->lits = new Word[code->sizelits];
  code->littypes = new u1[code->sizelits];
  for (u2 i = 0; i < code->sizelits; ++i) {
    loadLiteral(f, &code->littypes[i], &code->lits[i], strings);
  }
  code->code = static_cast<BcIns*>
    (mm_->allocCode(code->sizecode, code->sizebitmaps));
  for (u2 i = 0; i < code->sizecode; ++i) {
    code->code[i] = f.get_u4();
  }
  bitmaps = (u2*)&code->code[code->sizecode];
  for (u2 i = 0; i < code->sizebitmaps; i++) {
    *bitmaps = f.get_u2();
    ++bitmaps;
  }
}

void Loader::loadLiteral(BytecodeFile &f,
                         u1 *littype, Word *literal,
                         const StringTabEntry *strings) {
  u4 i;
  *littype = f.get_u1();
  switch (*littype) {
  case LIT_INT:
    *literal = (Word)f.get_varsint();
    break;
  case LIT_CHAR:
    *literal = (Word)f.get_varuint();
    break;
  case LIT_WORD:
    *literal = (Word)f.get_varuint();
    break;
  case LIT_FLOAT:
    *literal = (Word)f.get_u4();
    break;
  case LIT_STRING:
    i = f.get_varuint();
    *literal = (Word)strings[i].str;
    break;
  case LIT_CLOSURE:
    { const char *clname = loadId(f, strings, ".");
      loadClosureReference(clname, literal);
    }
    break;
  case LIT_INFO:
    { const char *infoname = loadId(f, strings, ".");
      loadInfoTableReference(infoname, literal);
    }
    break;
  default:
    fprintf(stderr, "ERROR: Unknown literal type (%d) "
            "when loading file: %s\n",
            *littype, f.filename());
    exit(1);
  }
}

// Forward References
// ------------------
//
// Like in any linker, it is possible that we load a reference to a
// closure or info table that we haven't loaded yet and therefore
// don't know the address of.  There may be multiple of such places;
// once we know the real address we have to update all of them.
//
// We use an in-place scheme that avoids additional memory allocation
// (but is perhaps a bit too clever).  We use info tables as an
// example.  If we discover a reference to an info table that hasn't
// been loaded yet we create a dummy info table entry which points to
// the first location to be updated.  And initialize the value of that
// location to NULL:
//
//     reference1:            infoTables_[infoTableName] =
//
//       +---------+             +----------------+
//       |   NULL  |<---.        | type = INVALID | mark as fwd. ref
//       +---------+    |        :                :
//                      '--------* next           |
//                               +----------------+
// 
// Now we encounter another forward reference:
//
//     reference1:            infoTables_[infoTableName] =
//
//       +---------+             +----------------+
//       |   NULL  |<---.        | type = INVALID | mark as fwd. ref
//       +---------+    |        :                :
//                      |    .---* next           |
//     reference2:      |    |   +----------------+
//                      |    |
//       +---------+    |    |
//       |         *----'    |
//       +---------+<--------'
//
// That is each location that needs to be update points to the next location
// that needs to be updated.  The dummy entry in the info table hash map
// points to the beginning of this linked list.
//
// Once the actual info table is loaded this list is traversed and each
// reference is updated to point to the real location and the dummy info
// table is discarded:
//
//     reference1:            infoTables_[infoTableName] =
//
//       +---------+             +----------------+
//       |         *---------*-->| type = CONSTR  |
//       +---------+         |   | ...            |
//                           |   | name = "Foo"   |
//     reference2:           |   +----------------+
//                           |
//       +---------+         |
//       |         *---------'
//       +---------+
//
// The same technique is used for closures, only that dummy closure references
// are marked by having NULL as the info table pointer.
//

void Loader::loadClosureReference(const char *name, Word *literal /* out */) {
  Closure *cl = closures_[name];
  if (cl == NULL) {
    // 1st forward ref, create the link
    cl = reinterpret_cast<Closure*>
      (new Word[(wordsof(ClosureHeader) + 1)]);
    cl->setInfo(NULL);
    cl->payload_[0] = (Word)literal;
    *literal = (Word)NULL;
    DLOG("Creating forward reference %p for `%s', " FMT_FWD_PTR "\n",
         cl, name, literal);
    closures_[name] = cl;
  } else if (cl->info() == NULL) {
    // forward ref (not the first), insert into linked list
    DLOG("Addinging forward reference %p for `%s', " FMT_FWD_PTR ")\n",
         cl, name, literal);
    *literal = (Word)cl->payload_[0];
    cl->payload_[0] = (Word)literal;
  } else {
    *literal = (Word)cl;
  }
}

void Loader::fixClosureForwardReference(const char *name, Closure *cl) {
  Closure *fwd_ref = closures_[name];
  if (fwd_ref != NULL) {
    // fixup forward refs
    void **p, *next;
    for (p = (void**)fwd_ref->payload_[0]; p != NULL;
         p = (void**)next) {
      next = *p;
      DLOG("Fixing closure forward ref: %s, "
           FMT_FWD_PTR " -> " FMT_CLOS_PTR "\n",
           name, p, cl);
      *p = (void*)cl;
    }

    delete[] (Word*)fwd_ref;
  }
}

void Loader::loadInfoTableReference(const char *name, Word *literal) {
  InfoTable *info = infoTables_[name];
  FwdRefInfoTable *info2;
  if (info == NULL) {
    // 1st forward ref
    info2 = new FwdRefInfoTable();
    info2->type_ = INVALID_OBJECT;
    info2->next = (void**)literal;
    *literal = (Word)NULL;
    infoTables_[name] = info2;
  } else if (info->type() == INVALID_OBJECT) {
    // subsequent forward ref
    info2 = (FwdRefInfoTable*)info;
    *literal = (Word)info2->next;
    info2->next = (void**)literal;
  } else {
    *literal = (Word)info;
  }
}

void Loader::fixInfoTableForwardReference(const char *name, InfoTable *info) {
  FwdRefInfoTable *old_itbl =
    static_cast<FwdRefInfoTable*>(infoTables_[name]);
  // new_itbl is the new info table.  There may have been forward
  // references (even during loading the code for this info table).
  if (old_itbl != NULL) {
    DLOG("Fixing itable forward reference for: %s, %p\n", name, info);
    void **p, *next;
    LC_ASSERT(old_itbl->type() == INVALID_OBJECT);

    for (p = old_itbl->next; p != NULL; ) {
      next = *p;
      *p = (void*)info;
      p = (void**)next;
    }

    delete old_itbl;
  }
}


void Loader::loadClosure(BytecodeFile &f,
                         const StringTabEntry *strings) {
  if (!f.magic("CLOS"))
    exit(2);
  const char *clos_name = loadId(f, strings, ".");
  u4 payloadsize = f.get_varuint();
  const char *itbl_name = loadId(f, strings, ".");
  Closure *cl = mm_->allocStaticClosure(payloadsize);
  InfoTable *info = infoTables_[itbl_name];
  
  // Info tables must all be fully loaded by now.
  LC_ASSERT(info != NULL && info->type() != INVALID_OBJECT);

  // Fill in closure payload.  May create forward references to the
  // current closure.
  cl->setInfo(info);
  for (u4 i = 0; i < payloadsize; i++) {
    DLOG("Loading payload for: %s [%d]\n", clos_name, i);
    u1 dummy;
    loadLiteral(f, &dummy, &cl->payload_[i], strings);
  }

  fixClosureForwardReference(clos_name, cl);

  DLOG("loadClosure: %s " COLOURED(COL_GREEN, "%p") "\n",
       clos_name, cl);
  closures_[clos_name] = cl;
}

_END_LAMBDACHINE_NAMESPACE
