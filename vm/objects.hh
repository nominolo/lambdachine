#ifndef _OBJECTS_H_
#define _OBJECTS_H_

#include "common.hh"

_START_LAMBDACHINE_NAMESPACE

class InfoTable {};

typedef struct _Closure Closure;

class ClosureHeader {
public:
  inline InfoTable *info() { return info_; }
private:
  InfoTable *info_;
  friend struct _Closure;
};

struct _Closure {
public:
  ClosureHeader header_;
  Word payload_[];

  static inline void initHeader(Closure *c, InfoTable *info) {
    c->header_.info_ = info;
  }
  inline InfoTable *info() { return header_.info(); }
  inline Word payload(int i) { return payload_[i]; }
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _OBJECTS_H_ */
