#include "objects.hh"

_START_LAMBDACHINE_NAMESPACE

using namespace std;

void InfoTable::debugPrint(std::ostream &out) {
  out << name() << endl;
  switch (type()) {
  case FUN:
  case THUNK:
  case CAF:
  case AP_CONT:
    {
      CodeInfoTable *i = (CodeInfoTable*)this;
      i->printCode(out);
    }
    break;
  default:
    break;
  }
}

void CodeInfoTable::printCode(std::ostream &out) {
  code();
  const BcIns *ins = code()->code;
  while (ins < code()->code + code()->sizecode) {
    out << "  ";
    ins = ins->debugPrint(out, ins);
  }
}

_END_LAMBDACHINE_NAMESPACE
