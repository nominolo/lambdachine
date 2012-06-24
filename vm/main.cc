#include "options.hh"
#include "memorymanager.hh"
#include "loader.hh"
#include "capability.hh"
#include "thread.hh"

#include <iostream>
#include <memory>

using namespace std;
using namespace lambdachine;

static bool loadWiredInModules(Loader &loader) {
  return
    loader.loadModule("GHC.Bool") &&
    loader.loadModule("Control.Exception.Base");
}

int main(int argc, char *argv[]) {
  OptionParser p;
  p.defaultEntry("test");
  p.defaultBasePath("tests");
  auto_ptr<Options> opts(p.parse(argc, argv));
  if (!opts.get())
    return 1;

  MemoryManager mm;
  Loader loader(&mm, opts->basePath().c_str());

  loadWiredInModules(loader);

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

  if (!cap.eval(T, entryClosure)) {
    cerr << "Error during evaluation." << endl;
    return 1;
  }

  Closure *result = (Closure*)T->slot(0);
  printClosure(cout, result, true);

  delete T;

  return 0;
}
