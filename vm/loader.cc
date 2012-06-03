#include "loader.hh"

#include <iostream>

using namespace std;

namespace lambdachine {

Loader::Loader(MemoryManager *mm, const char* basepaths) 
  : mm_(mm), loadedModules_(10), basepaths_(NULL) {
  initBasePath(basepaths);
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

const char *Loader::basePath(unsigned int index) {
  BasePathEntry *e = basepaths_;
  while (index > 0 && e != NULL) {
    --index;
    e = e->next;
  }
  if (e == NULL)
    return NULL;
  return e->path;
}

}
