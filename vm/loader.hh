#ifndef _LOADER_H_
#define _LOADER_H_

#include "config.hh"
#include "memorymanager.hh"

#include <string.h>
#include HASH_MAP_H

using namespace HASH_NAMESPACE;

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
  HASH_MAP_CLASS<const char*, valueType, hashstr, eqstr>

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

class Loader {
public:
  Loader(MemoryManager *mm, const char* basepaths);
  ~Loader();

  const char *basePath(unsigned int index) const;
  char *findModule(const char *moduleName);
  bool loadModule(const char *moduleName);
  const Module *module(const char *moduleName) {
    return loadedModules_[moduleName];
  }

private:
  void initBasePath(const char *);
  void addBasePath(const char *);
  void appendBasePathEntry(BasePathEntry *entry);
  void loadStringTabEntry(FILE *f, StringTabEntry *e /*out*/);
  const char *loadId(FILE *f, const StringTabEntry *strings,
                     const char* sep);
  bool loadModule(const char *moduleName, int);
  Module *loadModuleHeader(FILE *f, const char *filename);
  MemoryManager *mm_;
  STRING_MAP(Module*) loadedModules_;
  BasePathEntry *basepaths_;
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _LOADER_H_ */
