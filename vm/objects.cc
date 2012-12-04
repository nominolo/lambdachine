#include "objects.hh"
#include <string.h>

_START_LAMBDACHINE_NAMESPACE

using namespace std;

const u2 closureFlags[] = {
# define DEFFLAG(_, flags) CF_##flags,
  CTDEF(DEFFLAG)
};

void printClosure(ostream &out, Closure *cl, bool oneline) {
  const InfoTable *info = cl->info();

  if (!info) {
    out << "[UnknownClosure]";
    if (!oneline) out << endl;
    return;
  }

  while (info->type() == IND) {
    cl = (Closure *)cl->payload(0);
    info = cl->info();
    out << "IND -> ";
  }

  out << info->name() << ' ';

  u4 bitmap = info->layout().bitmap;
  for (i4 i = 0; i < (i4)info->size(); ++i, bitmap >>= 1) {
    if (bitmap & 1)
      out << (Word *)cl->payload(i) << ' ';
    else
      out << (Word)cl->payload(i) << ' ';
  }

  if (!oneline) out << endl;
}

void
printClosureShort(ostream &out, Closure *cl)
{
  const InfoTable *info = cl->info();
  
  if (!info) {
    out << "[!NULL!]";
    return;
  }

  out << '[' << COL_BLUE;
  while (info->type() == IND) {
    out << (void *)cl << "->";
    cl = (Closure *)cl->payload(0);
    info = cl->info();
  }
  out << (void *)cl << COL_RESET << '=';

  const char *name = info->name();
  const char *lastdot = strrchr(name, '.');
  if (lastdot == NULL) 
    lastdot = name;
  else
    ++lastdot;
  size_t len = strlen(lastdot);
  const char *backtick = strrchr(lastdot, '`');
  if (backtick != NULL)
    len = (size_t)(backtick - lastdot);
  char buf[41];
  if (len > 40) len = 40;
  memcpy(buf, lastdot, len);
  buf[len] = '\0';

  out << buf << ']';
}

void InfoTable::printPayload(ostream &out, u4 bitmap, u4 size) {
  LC_ASSERT(size <= 32);
  out << '[';
  for ( ; size > 0; --size) {
    out << (bitmap & 1 ? '*' : '-');
    bitmap >>= 1;
  }
  out << ']';
}

void InfoTable::printPayload(ostream &out) const {
  out << " Payload=";
  InfoTable::printPayload(out, layout_.bitmap, size_);
  out << endl;
}

void InfoTable::debugPrint(ostream &out) const {
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
    CodeInfoTable *i = (CodeInfoTable *)this;
    i->printCode(out);
  }
  out << endl;
}

void Code::printLiteral(std::ostream &out, u4 litid) const {
  if (LC_UNLIKELY(litid >= sizelits))
    return;

  Word lit = lits[litid];
  switch (littypes[litid]) {
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
    out << '"' << (const char *)lit << '"';
    break;
  case LIT_INFO: {
    const InfoTable *i = (const InfoTable *)lit;
    if (i != NULL) {
      out << "info " << i << " (" << i->name() << ")";
    } else {
      out << "info (unresolved)";
    }
    break;
  }
  case LIT_CLOSURE: {
    const Closure *cl = (const Closure *)lit;
    if (cl != NULL) {
      out << "clos " << cl << " (" << cl->info()->name() << ")";
    } else {
      out << "clos (unresolved)";
    }
    break;
  }
  default:
    out << "???";
  }
}

void CodeInfoTable::printLiteral(std::ostream &out, u4 litid) const {
  code()->printLiteral(out, litid);
}

static void printArgPointers(ostream &out, const u2 *bitmap, u4 arity) {
  u2 mask;
  u4 args = 0;
  out << '[';
  do {
    mask = *bitmap++;
    for (int i = 0; i < 15 && args < arity; ++i, mask >>= 1, ++args) {
      if (mask & 1) {
        out << '*';
      } else {
        out << '-';
      }
    }
  } while (mask != 0);
  out << ']';
}

bool
isConstructor(Closure *cl)
{
  while (cl->isIndirection()) {
    cl = (Closure *)cl->payload(0);
  }
  return cl->info()->type() == CONSTR;
}

void CodeInfoTable::printCode(std::ostream &out) const {
  if (code()->sizelits > 0) {
    out << "  literals:" << endl;
    for (u4 i = 0; i < (u4)code()->sizelits; ++i) {
      out << "    " << i << ": ";
      printLiteral(out, i);
      out << endl;
    }
  }
  out << "  code"
      << " (arity=" << (int)code()->arity << '/';
  printArgPointers(out, (const u2 *)(code()->code + code()->sizecode),
                   code()->arity);
  out << ", frame=" << (int)code()->framesize
      << ")" << endl;
  const BcIns *ins = code()->code;
  while (ins < code()->code + code()->sizecode) {
    out << "    ";
    ins = ins->debugPrint(out, ins, false, code()->code, code());
  }
}

_END_LAMBDACHINE_NAMESPACE
