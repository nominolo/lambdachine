#ifndef _LOADER_H_
#define _LOADER_H_

#include "config.hh"
#include "memorymanager.hh"
#include "objects.hh"
#include "fileutils.hh"

#include <string.h>
#include <stdio.h>
#include <iostream>
#include HASH_MAP_H

_START_LAMBDACHINE_NAMESPACE

#define STR_SEC_HDR_MAGIC       MSB_u4('B','C','S','T')
#define CLOS_SEC_HDR_MAGIC      MSB_u4('B','C','C','L')
#define INFO_MAGIC              MSB_u4('I','T','B','L') 
#define CLOSURE_MAGIC           MSB_u4('C','L','O','S')

typedef struct _StringTabEntry {
  Word len;
  char *str;
} StringTabEntry;

  // If only C++ had type classes.  Or concepts...

struct eqstr {
  bool operator()(const char* s1, const char* s2) const
  {
    return (s1 == s2) || (s1 && s2 && strcmp(s1, s2) == 0);
  }
};

// A simple hash function.
struct hashstr {
  size_t operator()(const char *str) const {
    uint8_t *bp = (uint8_t*) str;
    uint32_t hval = 0x811c9dc5;

    while (*bp) {
      hval += (hval<<1) + (hval<<4) + (hval<<7) +
        (hval<<8) + (hval<<24);
      hval ^= (uint32_t)*bp++;
    }
  
    return hval;
  }
};

#define STRING_MAP(valueType) \
  HASH_NAMESPACE::HASH_MAP_CLASS<const char*, valueType, hashstr, eqstr>

class Module {
public:
  inline const char *name() const { return name_; }
private:
  const char *name_;
  uint32_t flags_;              // Currently unused
  uint32_t numInfoTables_;
  uint32_t numClosures_;
  uint32_t numStrings_;
  uint32_t numImports_;

  StringTabEntry *strings_;
  const char    **imports_;
  friend class Loader;
};

typedef struct _BasePathEntry BasePathEntry;  // Defined in loader.cc

class BytecodeFile {
public:
  BytecodeFile(const char *filename);
  bool open();
  void close();
  inline const char *filename() const { return name_; }
  inline uint8_t get_u1() { return (uint8_t)fgetc(f_); }
  inline uint16_t get_u2() {
    uint16_t hi = get_u1();
    uint16_t lo = get_u1();
    return hi << 8 | lo;
  }
  inline Word get_varuint() {
    Word b = get_u1();
    if ((b & 0x80) == 0)
      return b;
    else
      return get_varuint_slow(b & 0x7f);
  }

  inline WordInt get_varsint() {
    return zigZagDecode(get_varuint());
  }

  inline char *get_string(size_t len) {
    char *p = new char[len + 1];
    fread(p, 1, len, f_);
    p[len] = '\0';
    return p;
  }
  // Require the file to contain the exact byte sequence.
  inline bool magic(const char *bytes);
  inline void get_string(char *buf, size_t len) {
    fread(buf, 1, len, f_);
  }
  uint32_t get_u4();
  inline long offset() { return ftell(f_); }
private:
  Word get_varuint_slow(Word first);

  const char *name_;
  FILE *f_;
};

typedef const StringTabEntry *StringTable;

class Loader {
public:
  Loader(MemoryManager *mm, const char* basepaths);
  ~Loader();

  const char *basePath(unsigned int index) const;
  char *findModule(const char *moduleName);
  bool loadModule(const char *moduleName);
  inline const Module *module(const char *moduleName) {
    return loadedModules_[moduleName];
  }
  void printInfoTables(std::ostream&);
  void printClosures(std::ostream&);
  void printMiscClosures(std::ostream&);
  inline Closure *closure(const char *name) {
    Closure *cl = closures_[name];
    return cl;
  }

private:
  void initBasePath(const char *);
  void addBasePath(const char *);
  void appendBasePathEntry(BasePathEntry *entry);

  void loadStringTabEntry(BytecodeFile&, StringTabEntry *e /*out*/);
  const char *loadId(BytecodeFile&, const StringTabEntry *strings,
                     const char* sep);
  bool loadModule(const char *moduleName, int);
  Module *loadModuleHeader(BytecodeFile&);
  void loadModuleBody(BytecodeFile &f, Module *mdl);
  InfoTable *loadInfoTable(BytecodeFile &f, const StringTabEntry *strings);
  void loadCode(BytecodeFile &, Code * /* out */,
                const StringTabEntry *strings);
  void loadLiteral(BytecodeFile &, u1 *littypes, Word *lits,
                   const StringTabEntry *strings);
  void loadClosure(BytecodeFile &, const StringTabEntry *strings);
  void loadClosureReference(const char *name, Word *literal /* out */);
  void fixClosureForwardReference(const char *name, Closure *cl);
  void loadInfoTableReference(const char *name, Word *literal /* out */);
  void fixInfoTableForwardReference(const char *name, InfoTable *info);
  bool checkNoForwardRefs();

  MemoryManager *mm_;
  STRING_MAP(Module*) loadedModules_;
  STRING_MAP(InfoTable*) infoTables_;
  STRING_MAP(Closure*) closures_;
  BasePathEntry *basepaths_;
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _LOADER_H_ */
