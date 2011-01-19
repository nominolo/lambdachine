#include "Loader.h"
#include "Thread.h"
#include "HashTable.h"
#include "PrintClosure.h"

#include <stdio.h>
#include <stdlib.h>

typedef struct {
  const char  *input_file;
  const char  *main_closure;
  int          print_loader_state;
} Opts;

#define MAX_CLOSURE_NAME_LEN 512

int
main(int argc, char *argv[])
{
  Thread   *T0;
  Closure  *clos0;
  Opts      opts = {
    .input_file = "Bc0005",
    .main_closure = NULL,
    .print_loader_state = 1
  };
  char main_clos_name[MAX_CLOSURE_NAME_LEN];

  // TODO: Parse flags

  if (argc >= 2)
    opts.input_file = argv[1];
  if (argc >= 3) {
    opts.main_closure = argv[2];
  } else {
    int n = snprintf(main_clos_name, MAX_CLOSURE_NAME_LEN,
		     "%s.test!closure", opts.input_file);
    if (n <= MAX_CLOSURE_NAME_LEN) {
      opts.main_closure = main_clos_name;
    } else {
      fprintf(stderr, "ERROR: Main closure name too long.\n");
      exit(1);
    }
  }

  initVM();
  initLoader();

  loadModule(opts.input_file);

  if (opts.print_loader_state) {
    printLoaderState();
    fflush(stdout);
  }

  if (opts.main_closure == NULL) // Nothing to run, just quit.
    return 0;

  printf("----------------------------------------------------\n"
	 "Loaded %s, now evaluating '%s'\n", opts.input_file, opts.main_closure);

  clos0 = lookupClosure(opts.main_closure);
  if (clos0 == NULL) {
    fprintf(stderr, "ERROR: Closure not found: %s\n", opts.main_closure);
    exit(1);
  }

  T0 = createThread(cap0, 1024);
  clos0 = startThread(T0, clos0);
  printClosure(clos0);
  
  return 0;
}
