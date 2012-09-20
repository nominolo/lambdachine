#include "options.hh"
#include "memorymanager.hh"
#include "loader.hh"
#include "capability.hh"
#include "thread.hh"
#include "time.hh"


#include <iostream>
#include <memory>

using namespace std;
using namespace lambdachine;

void formatTime(FILE *out, const char *label, Time time);
void formatWithThousands(char *str, uint64_t n);
void printGCStats(FILE *out, MemoryManager *mm, Time mut_time);

inline double percent(double num, double denom) {
  return (num * 100) / denom;
}

int main(int argc, char *argv[]) {
  OptionParser p;
  p.defaultEntry("test");
  p.defaultBasePath("tests");
  auto_ptr<Options> opts(p.parse(argc, argv));
  if (!opts.get())
    return 1;

  initializeTimer();
  Time startup_time = getProcessElapsedTime();
  MemoryManager mm;
  Loader loader(&mm, opts->basePath().c_str());

  if (!loader.loadWiredInModules())
    return 1;

  for (int i = 0; i < opts->inputCount(); ++i) {
    if (!loader.loadModule(opts->inputModule(i).c_str())) {
      //cerr << "Could not load module: " << opts->inputModule(i) << endl;
      return 1;
    }
  }

  if (opts->printLoaderState()) {
    loader.printInfoTables(cout);
    loader.printClosures(cout);
  }

  if (opts->entry().empty()) {
    return 0;
  }

  string entry = opts->inputModule(0) + "." + opts->entry() + "`closure";
  Closure *entryClosure = loader.closure(entry.c_str());

  if (!entryClosure) {
    cerr << "Could not find entry point: " << entry << endl;
    return 1;
  }

  Capability cap(&mm);
  Thread *T = Thread::createThread(&cap, opts->stackSize() / sizeof(Word));

  if (opts->traceInterpreter()) {
    cap.enableBytecodeTracing();
  }

  Time start_time = getProcessElapsedTime();

  if (!cap.eval(T, entryClosure)) {
    cerr << "Error during evaluation." << endl;
    return 1;
  }

  Closure *result = (Closure*)T->slot(0);
  printClosure(cout, result, true);

  Time stop_time = getProcessElapsedTime();
  Time run_time = stop_time - start_time;
  Time total_time = stop_time - startup_time;
  Time mut_time = run_time - jit_time - gc_time;

  delete T;

  printf("\n\n");
  printLoggedNYIs(stdout);

  printf("\n\n");
  printGCStats(stdout, &mm, mut_time);

  formatTime(stdout, "  Startup ", start_time - startup_time);
  formatTime(stdout, "    LOAD  ", loader_time);
  formatTime(stdout, "  Runtime ", run_time);
  formatTime(stdout, "    MUT   ", mut_time);
  formatTime(stdout, "    JIT   ", jit_time);
  formatTime(stdout, "    GC    ", gc_time);
  formatTime(stdout, "  Total   ", total_time);
  printf("\n" "    %%GC      %5.1f%%\n", percent(gc_time, run_time)); 
  printf("    %%JIT     %5.1f%%  (  # traces      %5d  )" "\n\n",
         percent(jit_time, run_time), Jit::numFragments()); 

  //  cerr << mm << endl;

  return 0;
}

void formatWithThousands(char *res, uint64_t n)
{
  uint32_t ns[7];
  int i = 0;
  char *p = res;
  do {
    ns[i] = n % 1000;
    n = n / 1000;
    i++;
  } while (n > 0);
  i--;
  p += sprintf(p, "%3d", ns[i]);
  for (i = i - 1; i >= 0; i--) {
    p += sprintf(p, ",%03d", ns[i]);
  }
}

void formatTime(FILE *out, const char *label, Time time) {
  char buf[30];
  double seconds = (double)time / TIME_RESOLUTION;
  formatWithThousands(buf, time);
  fprintf(out, "%s %7.2fs  (%20s ns)\n", label, seconds, buf);
}

void printGCStats(FILE *out, MemoryManager *mm, Time mut_time) {
  char buf[30];
  uint64_t total_alloc = mm->allocated();
  formatWithThousands(buf, total_alloc);
  fprintf(out, "  %20s bytes allocated in the heap\n", buf);

  double mut_seconds = (double)mut_time / TIME_RESOLUTION;
  uint64_t alloc_rate = (uint64_t)(total_alloc / mut_seconds);
  formatWithThousands(buf, alloc_rate);
  fprintf(out, "   (%18s bytes per MUT second)\n", buf);
  fprintf(out, "    %18d collections\n\n", mm->numGCs());
}
