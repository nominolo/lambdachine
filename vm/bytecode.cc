#include "bytecode.hh"
#include <iomanip>

_START_LAMBDACHINE_NAMESPACE

using namespace std;

static const char *ins_name[] = {
#define BCFMT(name,fmt)  #name,
  BCDEF(BCFMT)
#undef BCFMT
};

const char *BcIns::name() const {
  Opcode opc = opcode();
  LC_ASSERT(opc <= BcIns::kSTOP);
  return ins_name[opc];
}

static const BcIns::InsFormat ins_format[] = {
#define BCFMT(name,f)  BcIns::IFM_##f,
  BCDEF(BCFMT)
#undef BCFMT
};

BcIns::InsFormat BcIns::format() const {
  Opcode opc = opcode();
  LC_ASSERT(opc <= BcIns::kSTOP);
  return ins_format[opc];
}

static ostream &printAddr(ostream &out,
                          const BcIns *baseaddr, const BcIns *addr) {
  if (!baseaddr) {
    out << (addr);
  } else {
    out << setw(2) << (addr) - baseaddr;
  }
  return out;
}

const BcIns *BcIns::debugPrint(ostream &out, const BcIns *ins,
                               bool oneline, const BcIns *baseaddr) {
  const BcIns *ins0 = ins;
  const BcIns i = *ins;

  printAddr(out, baseaddr, ins0) << ": ";
  ++ins;

  switch (i.format()) {
  case IFM_R:
    out << i.name() << "\tr" << (int)i.a() << endl;
    break;
  case IFM_RR:
    out << i.name() << "\tr" << (int)i.a() << ", r" 
        << (int)i.d() << endl;
    break;
  case IFM_RRR:
    out << i.name() << "\tr" << (int)i.a() << ", r" << (int)i.b()
        << ", r" << (int)i.c()<< endl;
    break;
  case IFM_RN:
    out << i.name() << "\tr" << (int)i.a() << ", " << (int)i.d() << endl;
    break;
  case IFM_RS:
    out << i.name() << "\tr" << (int)i.a() << ", " << (int)i.sd() << endl;
    break;
  case IFM_RRN:
    out << i.name() << "\tr" << (int)i.a() << ", r" << (int)i.b()
        << ", " << (int)i.c()<< endl;
    break;
  case IFM_J:
    out << i.name() << " ->";
    printAddr(out, baseaddr, ins + i.j()) << endl;
    break;
  case IFM_RRJ:
    out << i.name() << "\tr" << (int)i.a() << ", r" << (int)i.d()
        << " ->";
    printAddr(out, baseaddr, (ins + 1 + ins->j())) << endl;
    ++ins;
    break;
  case IFM____:
    switch (i.opcode()) {
    case kEVAL:
      out << "EVAL\tr" << (int)i.a() << endl;
      // TODO: Print bitmap
      ++ins;
      break;
    case kCASE:
      {
        u2 *tgt = (u2*)ins;
        u4 ncases = i.d();
        ins += (ncases + 1) / 2;
        out << "CASE\tr" << (int)i.a() << endl;
        if (!oneline) {
          for (u4 j = 0; j < ncases; j++, tgt++) {
            out << "           " << j + 1
                << ": ->";
            printAddr(out, baseaddr, ins + (int)(*tgt)) << endl;
          }
        }
      }
      break;
    case kCASE_S:
      out << "CASE_S\tr" << (int)i.a() << " ...TODO..." << endl;
      ins += i.d();
      break;
    case kALLOC1:
      out << i.name() << "\tr" << (int)i.a() << ", r" << (int)i.b()
          << ", r" << (int)i.c() << endl;
      ++ins;  // skip bitmap
      break;
    case kALLOC:
      {
        u1 *arg = (u1*)ins;
        ins += 1 + BC_ROUND(i.c());
        out << "ALLOC\tr" << (int)i.a() << ", r" << (int)i.b();
        for (u4 j = 0; j < i.c(); j++, arg++) {
          out << ", r" << (int)*arg;
        }
        out << endl;
      }
      break;
    case kALLOCAP:
      {
        u1 *arg = (u1*)ins;
        ins += 1 + BC_ROUND(i.c() + 1);
        out << "ALLOCAP\tr" << (int)i.a();
        u1 ptrmask = i.b();
        out << ", r" << (int)*arg++;
        for (u4 j = 1; j < (u4)i.c() + 1; j++, arg++) {
          out << ", r" << (int)*arg;
          if (ptrmask & 1) out << '*';
          ptrmask >>= 1;
        }
        out << endl;
      }
      break;
    case kCALL:
      {
        u1 *arg = (u1*)ins;
        ins += BC_ROUND(i.c()) + 1;
        out << "CALL\tr" << (int)i.a();
        u1 ptrmask = i.b();
        char comma = '(';
        for (u4 j = 0; j < i.c(); j++, arg++) {
          out << comma << "r" << (int)*arg;
          if (ptrmask & 1) out << '*';
          comma = ',';
          ptrmask >>= 1;
        }
        out << ")"<< endl;
      }
      break;
    case kCALLT:
      {
        u1 bitmask = i.b();
        out << "CALLT r" << (int)i.a();
        char comma = '(';
        for (u4 j = 0; j < i.c(); j++) {
          out << comma << 'r' << j;
          if (bitmask & 1) out << '*';
          comma = ',';
          bitmask >>= 1;
        }
        out << ')' << endl;
      }
      break;
    case kFUNC:
      out << i.name() << endl;
      break;
    default:
      out << i.name() << " {TODO}" << endl;
      break;
    }
    break;
  default:
    out << i.name() << " {unknown format}" << endl;
    break;
  }
  return ins;
}

_END_LAMBDACHINE_NAMESPACE
