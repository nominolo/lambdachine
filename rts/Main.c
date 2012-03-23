#include "Loader.h"
#include "Thread.h"
#include "HashTable.h"
#include "PrintClosure.h"
#include "StorageManager.h"
#include "MiscClosures.h"
#include "Stats.h"
#include "Opts.h"
#include "Jit.h"

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <ctype.h>
#include <limits.h>

#define MAX_CLOSURE_NAME_LEN 512
#define MAX_STACK_SIZE (1024*1024)
#define MIN_STACK_SIZE (1024*sizeof(Word))

static Opts opts = {
  .input_file = "Bc0005",
  .base_path  = ".",
  .main_closure = "test",
  .print_loader_state = NULL,
  .disable_jit = 0,
  .enable_asm  = 0,
  .step_opts = 0,
  .stack_size = 1024,
};

typedef enum {
  OPT_PRINT_LOADER_STATE = 0x1000,
} OptionFlags;

u4 G_jitstep = 0;

void loadWiredInModules();
long parseMemorySize(const char *str);


int
main(int argc, char *argv[])
{
  Thread   *T0;
  Closure  *clos0;
  char main_clos_name[MAX_CLOSURE_NAME_LEN];

  // TODO: Parse flags

  static struct option long_options[] = {
    {"print-loader-state", optional_argument, NULL, OPT_PRINT_LOADER_STATE},
    {"no-jit",             no_argument, &opts.disable_jit, 1},
    {"asm",                no_argument, &opts.enable_asm, 1},
    {"no-run",             no_argument, 0, 'l'},
    {"entry",              required_argument, 0, 'e'},
    {"base",               required_argument, 0, 'B'},
    {"help",               no_argument, 0, 'h'},
    {"step",               required_argument, 0, 'S'},
    {"stack",              required_argument, 0, 's'},
    {0, 0, 0, 0}
  };

  int c;

  while (1) {
    int option_index = 0;
    c = getopt_long(argc, argv, "he:B:", long_options, &option_index);

    if (c == -1)
      break;

    switch (c) {
    case 0:
      /* If this option set a flag, do nothing else now. */
      if (long_options[option_index].flag != 0)
        break;
      break;
    case OPT_PRINT_LOADER_STATE:
      if (optarg == NULL) {
	opts.print_loader_state = "";
      } else {
	opts.print_loader_state = optarg;
      }
      break;
    case 'e':
      fprintf(stderr, "entry = %s\n", optarg);
      opts.main_closure = optarg;
      break;
    case 'B':
      fprintf(stderr, "base = %s\n", optarg);
      opts.base_path = optarg;
      break;
    case 's':
      opts.stack_size = parseMemorySize(optarg);
      if (opts.stack_size < 0) {
	fprintf(stderr, "Could not parse stack size.  Using default.\n");
	opts.stack_size = MIN_STACK_SIZE;
      }
      break;
    case 'S':
      opts.step_opts = optarg;
      break;
    case 'l':
      opts.main_closure = NULL;
      break;
    case 'h':
      printf("Usage: %s [options] MODULE_NAME\n\n"
             "Options:\n"
             "  -h --help       Print this help.\n"
             "  -e --entry      Set entry point (default: test)\n"
             "     --no-run     Load module only.\n"
             "     --print-loader-state[=FILE]\n"
             "                  Print static closures and info tables after loading (to stderr or given file).\n"
             "     --no-jit     Don't enable JIT.\n"
             "     --asm        Generate native code.\n"
             "  -B --base       Set loader base dir (default: cwd).\n"
             "                  Separate multiple paths with \":\""
	     "     --stack=SIZE Specify the stack size in bytes, valid units are K,M,b,G.\n"
             "\n",
             argv[0]);
      exit(0);
    default:
      fprintf(stderr, "c = %d\n", c);
      abort();
    }
  }

  int nmodules = argc - optind;

  if (nmodules == 1) {
    opts.input_file = argv[optind];
  } else if (nmodules == 0) {
    fprintf(stderr, "No module specified.\n");
    exit(1);
  } else { // modules > 1
    fprintf(stderr, "Too many modules specified (%d).\n",
            nmodules);
    while (optind < argc)
      fprintf(stderr, "%s\n", argv[optind++]);
    exit(1);
  }

  if (opts.main_closure != NULL) {
    int n = snprintf(main_clos_name, MAX_CLOSURE_NAME_LEN,
                     "%s.%s`closure", opts.input_file,
                     opts.main_closure);
    if (n <= MAX_CLOSURE_NAME_LEN) {
      opts.main_closure = main_clos_name;
    } else {
      fprintf(stderr, "ERROR: Main closure name too long.\n");
      exit(1);
    }
  }

  if (opts.step_opts != 0) {
    const char *p = opts.step_opts;
    while (*p != '\0') {
      switch (*p) {
      case '*': G_jitstep |= 0xffffffff; break;
      case 'r': G_jitstep |= STEP_START_RECORDING; break;
      case 'R': G_jitstep |= STEP_FINISH_RECORDING; break;
      case 't': G_jitstep |= STEP_ENTER_TRACE; break;
      case 'T': G_jitstep |= STEP_ENTER_TRACE; break;
      default:
        fprintf(stderr, "Unknown option to --step: %c", *p);
      }
      p++;
    }
  }

  initStorageManager();
  initVM(&opts);
  initLoader(&opts);
#ifdef LC_SELF_CHECK_MODE
  initShadowHeap();
#endif
  loadWiredInModules();
  loadModule(opts.input_file);

  if (opts.print_loader_state != NULL) {
    FILE *out;
    if (*opts.print_loader_state != '\0') {
      out = fopen(opts.print_loader_state, "w");
      if (out == NULL) {
	fprintf(stderr, "Could not write to file: %s",
		opts.print_loader_state);
	exit(1);
      }
    } else {
      out = stderr;
    }
    printLoaderState(out);
    fflush(out);
    if (out != stderr) {
      fclose(out);
    }
  }

  if (opts.main_closure == NULL) // Nothing to run, just quit.
    return 0;

  fprintf(stderr, "----------------------------------------------------\n"
	 "Loaded %s, now evaluating '%s'\n", opts.input_file, opts.main_closure);

  clos0 = lookupClosure(opts.main_closure);
  if (clos0 == NULL) {
    fprintf(stderr, "ERROR: Closure not found: %s\n", opts.main_closure);
    exit(1);
  }

  if (opts.disable_jit)
    G_cap0->flags |= CF_NO_JIT;

  if (opts.stack_size > MAX_STACK_SIZE) {
    fprintf(stderr, "Stack size too large, using 1M\n");
    opts.stack_size = MAX_STACK_SIZE;
  } else if (opts.stack_size < MIN_STACK_SIZE) {
    fprintf(stderr, "Stack size too small, using %ld\n", MIN_STACK_SIZE);
    opts.stack_size = MIN_STACK_SIZE;
  }

  T0 = createThread(G_cap0, opts.stack_size / sizeof(Word));
  clos0 = startThread(T0, clos0);
  printf("@Result@ "); printClosure_(stdout, clos0, 1);
  printEvents();
  dumpStorageManagerState();
  dumpApClosures();
  fprintf(stderr, "StaticRoots: %p\n", G_cap0->static_objs);

  return 0;
}

void loadWiredInModules()
{
  loadModule("GHC.Bool");
  loadModule("Control.Exception.Base");
}

#define MAX_LE

/**
 * Parses a memory size description.  For example:
 *
 *     "5K" -> 5120
 *     "3M" -> 3145728
 *
 * More formally, a memory size descriptor satisfies the regular
 * expression `[1-9][0-9]*[kKmMGgBb]?`.
 *
 * @param str the string to parse.  Note that there must not be
 *     leading or trailing whitespace.
 * @returns the parsed value if it fits within a long, otherwise
 *     LONG_MAX; or -1 if no parse was found.
 */
long
parseMemorySize(const char *str)
{
  char *end = NULL;
  long ans = -1;
  int shift = 0;

  if (!isdigit(*str)) {
    return -1;
  }

  ans = strtol(str, &end, 10);
  if (end == str) {		/* No parse */
    return -1;
  }

  switch (*end) {
  case 'b': case 'B':
    shift = 0;
    break;
  case 'k': case 'K':
    shift = 10;
    break;
  case 'm': case 'M':
    shift = 20;
    break;
  case 'g': case 'G':
    shift = 30;
    break;
  default:
    return -1;
  }
  ++end;

  if (*end != '\0') {
    return -1;
  }

  if (ans <= (LONG_MAX >> shift)) {
    return ans << shift;
  } else {
    return LONG_MAX;
  }
}
