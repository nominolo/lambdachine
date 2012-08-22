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

void formatWithThousands(char *str, uint64_t n);

int main(int argc, char *argv[]) {
  OptionParser p;
  p.defaultEntry("test");
  p.defaultBasePath("tests");
  auto_ptr<Options> opts(p.parse(argc, argv));
  if (!opts.get())
    return 1;

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

  Time run_time = getProcessElapsedTime() - start_time;

  delete T;

  char buf[30];

  printf("\n" "Elapsed Time:\n");

  formatWithThousands(buf, loader_time);
  printf("  Loader:   %20s ns\n", buf);

  formatWithThousands(buf, jit_time);
  printf("  Compiler: %20s ns\n", buf);

  formatWithThousands(buf, gc_time);
  printf("  GC:       %20s ns\n", buf);

  formatWithThousands(buf, run_time - jit_time - gc_time);
  printf("  Mutator:  %20s ns\n", buf);

  return 0;
}

void formatWithThousands(char *str, uint64_t n)
{
  uint32_t ns[7];
  int i = 0;
  char *p = str;
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

