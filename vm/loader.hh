#ifndef _LOADER_H_
#define _LOADER_H_

#include "config.hh"
#include "memorymanager.hh"

#include HASH_MAP_H

using namespace HASH_NAMESPACE;

namespace lambdachine {

class Module {
public:
  inline const char *name() const { return name_; }
private:
  const char *name_;
};

typedef struct _BasePathEntry BasePathEntry;  // Defined in loader.cc

class Loader {
public:
  Loader(MemoryManager *mm, const char* basepaths);
  ~Loader() {}

  const char *basePath(unsigned int index);
private:
  void initBasePath(const char *);
  void addBasePath(const char *);
  void appendBasePathEntry(BasePathEntry *entry);
  MemoryManager *mm_;
  HASH_MAP_CLASS<const char*, Module> loadedModules_;
  BasePathEntry *basepaths_;
};



}

#endif /* _LOADER_H_ */
