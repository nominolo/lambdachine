#include "objects.hh"

_START_LAMBDACHINE_NAMESPACE

using namespace std;

void InfoTable::printPayload(ostream &out) {
  out << " Payload=";
  if (size_ >= 32) {
    cerr << "ERROR: Unknown encoding" << endl;
    return;
  }
  out << '[';
  u4 bitmap = layout_.bitmap;
  for (u4 size = size_; size > 0; --size) {
    out << (bitmap & 1 ? '*' : '-');
    bitmap >>= 1;
  }
  out << ']' << endl;
}

void InfoTable::debugPrint(ostream &out) {
  out << COL_YELLOW << name() << ": " << this << COL_RESET << endl
      << "  ";
  switch (type()) {
  case FUN:
    out << "Function";
    break;
  case THUNK:
    out << "Thunk";
    break;
  case CAF:
    out << "CAF";
    break;
  case CONSTR:
    out << "Constructor. Tag=" << (int)tagOrBitmap_;
    break;
  default:
    out << "[Other Type]";
    break;
  }
  printPayload(out);
  if (hasCode()) {
      CodeInfoTable *i = (CodeInfoTable*)this;
      i->printCode(out);
  }
  out << endl;
}

void CodeInfoTable::printCode(std::ostream &out) {
  out << "  literals:" << endl;

  for (u4 i = 0; i < (u4)code()->sizelits; ++i) {
    out << "    " << i << ": ";
    Word lit = code()->lits[i];
    switch (code()->littypes[i]) {
    case LIT_INT:
      out << (WordInt)lit << " (i)";
      break;
    case LIT_WORD:
      out << (Word)lit << " (w)";
      break;
    case LIT_FLOAT:
      out << (float)lit << " (w)";
      break;
    case LIT_CHAR:
      if (lit < 256)
        out << "'" << (char)lit << "'";
      else
        out << "u" << hex << (u4)lit << dec;
      break;
    case LIT_STRING:
      out << '"' << (const char*)lit << '"';
      break;
    case LIT_INFO:
      {
        const InfoTable *i = (const InfoTable *)lit;
        out << "info " << i << " (" << i->name() << ")";
        break;
      }
    case LIT_CLOSURE:
      {
        const Closure *cl = (const Closure*)lit;
        out << "clos " << cl << " (" << cl->info()->name() << ")";
        break;
      }
    default:
      out << "???";
    }
    out << endl;
  }

  out << "  code"
      << " (arity=" << (int)code()->arity
      << ", frame=" << (int)code()->framesize
      << ")" << endl;
  const BcIns *ins = code()->code;
  while (ins < code()->code + code()->sizecode) {
    out << "    ";
    ins = ins->debugPrint(out, ins, false, code()->code);
  }
}

_END_LAMBDACHINE_NAMESPACE
