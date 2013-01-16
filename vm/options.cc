#include "options.hh"
#include <getopt.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>

_START_LAMBDACHINE_NAMESPACE

typedef enum {
  OPT_PRINT_LOADER_STATE = 0x1000,
  OPT_TRACE_INTERPRETER
} OptionFlags;

#define MAX_CLOSURE_NAME_LEN 512
#define MAX_STACK_SIZE (1024*1024)
#define MIN_STACK_SIZE (1024*sizeof(Word))

long parseMemorySize(const char *str);

Options::Options()
  : entry_("test"),
    printLoaderState_(false),
    traceInterpreter_(false),
    enableAsm_(1),
    stackSize_(MIN_STACK_SIZE)
{
}

Options::~Options() { }

OptionParser::~OptionParser() {
  if (opts_) delete opts_;
}

Options *OptionParser::parse(int argc, char *argv[]) {
  Options *res = NULL;
  int c;

  static struct option long_options[] = {
    {"print-loader-state", optional_argument, NULL, OPT_PRINT_LOADER_STATE},
    //    {"no-jit",             no_argument, &opts()->disable_jit, 1},
    {"asm",                no_argument, &opts()->enableAsm_, 1},
    {"no-run",             no_argument, 0, 'l'},
    {"entry",              required_argument, 0, 'e'},
    {"base",               required_argument, 0, 'B'},
    {"help",               no_argument, 0, 'h'},
    {"step",               required_argument, 0, 'S'},
    {"stack",              required_argument, 0, 's'},
    {"trace",              no_argument, NULL, OPT_TRACE_INTERPRETER},
    {0, 0, 0, 0}
  };

  while (1) {
    int option_index = 0;
    c = getopt_long(argc, argv, "he:B:O:", long_options, &option_index);

    if (c == -1)
      break;

    switch (c) {
    case 0:
      /* If this option set a flag, do nothing else now. */
      if (long_options[option_index].flag != 0)
        break;
      break;
    case OPT_PRINT_LOADER_STATE:
      opts()->printLoaderState_ = true;
      if (optarg != NULL) {
        opts()->printLoaderStateFile_ = optarg;
      }
      break;
    case OPT_TRACE_INTERPRETER:
      opts()->traceInterpreter_ = true;
      break;
    case 'e':
      fprintf(stderr, "entry = %s\n", optarg);
      opts()->entry_ = optarg;
      break;
    case 'B':
      fprintf(stderr, "base = %s\n", optarg);
      opts()->basePath_ = optarg;
      break;
    case 's':
      opts()->stackSize_ = parseMemorySize(optarg);
      if (opts()->stackSize_ < 0) {
        fprintf(stderr, "Could not parse stack size.  Using default.\n");
        opts()->stackSize_ = MIN_STACK_SIZE;
      }
      break;
      // case 'S':
      //   opts()->step_opts = optarg;
      //   break;
    case 'l':
      opts()->entry_ = "";
      break;
    case 'O':
      fprintf(stderr, "Ignoring argument -O: JIT flags not yet supported.\n");
      // if (!parseJitOpt(jitParams, &jitFlags, optarg)) {
      //   fprintf(stderr, "Unrecognized value for -O\n");
      // }
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
      res = NULL;
      goto ret;
    default:
      res = NULL;
      goto ret;
    }
  }

  while (optind < argc) {
    opts()->inputs_.push_back(argv[optind]);
    ++optind;
  }

  if (opts()->inputs_.size() < 1) {
    fprintf(stderr, "Error: No input modules specified.\n");
    res = NULL;
    goto ret;
  }

  res = opts_;
ret:
  if (res != opts_) {
    delete opts_;
  }
  opts_ = NULL;
  return res;
}

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
parseMemorySize(const char *str) {
  char *end = NULL;
  long ans = -1;
  int shift = 0;

  if (!isdigit(*str)) {
    return -1;
  }

  ans = strtol(str, &end, 10);
  if (end == str) {   /* No parse */
    return -1;
  }

  switch (*end) {
  case 'b':
  case 'B':
    shift = 0;
    break;
  case 'k':
  case 'K':
    shift = 10;
    break;
  case 'm':
  case 'M':
    shift = 20;
    break;
  case 'g':
  case 'G':
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


_END_LAMBDACHINE_NAMESPACE
