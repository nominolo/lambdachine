#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include "common.hh"

#include <vector>
#include <string>

_START_LAMBDACHINE_NAMESPACE

class Options {
public:
  inline int inputCount() const { return inputs_.size(); }
  std::string inputModule(int index) const { return inputs_[index]; }
  inline const std::string entry() const { return entry_; }
  inline const std::string basePath() const { return basePath_; }
  inline long stackSize() const { return stackSize_; }
  inline bool printLoaderState() const { return printLoaderState_; }
  inline bool traceInterpreter() const { return traceInterpreter_; }
  virtual ~Options();

protected:
  Options();

private:
  std::vector<std::string> inputs_;
  std::string entry_;
  std::string basePath_;
  bool printLoaderState_;
  bool traceInterpreter_;
  std::string printLoaderStateFile_;
  int enableAsm_;
  long stackSize_;

  friend class OptionParser;
};

class OptionParser {
public:
  OptionParser() : opts_(NULL) {}
  ~OptionParser();

  // N.B.: We cannot implement method chaining nicely in C++ because
  // the the destructor would get invoked multiple times, which fucks
  // everything up.

  inline void defaultEntry(const char *name) {
    opts()->entry_ = name;
  }
  inline void defaultBasePath(const char *pathspec) {
    opts()->basePath_ = pathspec;
  }
  Options *parse(int argc, char *argv[]);
private:
  inline Options *opts() {
    if (!opts_) opts_ = new Options();
    return opts_;
  }

  Options *opts_;
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _OPTIONS_H_ */
