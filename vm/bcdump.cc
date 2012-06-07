#include "loader.hh"
#include <iostream>

using namespace std;
using namespace lambdachine;

int main(int argc, char *argv[]) {
  if (argc != 2) {
    cerr << "Usage: " << argv[0] << " MODULE_NAME" << endl;
    return 1;
  }

  const char *module = argv[1];
  MemoryManager mm;
  Loader l(&mm, "tests");

  if (!l.loadModule(module)) {
    cerr << "Could not load module " << module << endl;
  }

  l.printInfoTables(cout);

  return 0;
}
