#include "miscclosures.hh"

#include <string.h>
#include <iostream>

_START_LAMBDACHINE_NAMESPACE

using namespace std;
using namespace HASH_NAMESPACE;

Closure *MiscClosures::stg_UPD_closure_addr = NULL;
BcIns *MiscClosures::stg_UPD_return_pc = NULL;
Closure *MiscClosures::stg_STOP_closure_addr = NULL;
InfoTable *MiscClosures::stg_IND_info = NULL;
MiscClosures::ApContInfo *MiscClosures::smallApConts = NULL;
HASH_MAP_CLASS<u4, MiscClosures::ApContInfo> *MiscClosures::otherApConts = NULL;
MemoryManager *MiscClosures::allocMM = NULL;
InfoTable *MiscClosures::stg_PAP_info = NULL;
InfoTable **MiscClosures::smallApInfos = NULL;
APMAP *MiscClosures::otherApInfos = NULL;
Closure *MiscClosures::stg_BLACKHOLE_closure_addr = NULL;
InfoTable *MiscClosures::stg_BYTEARR_info = NULL;

void MiscClosures::initStopClosure(MemoryManager &mm) {
  AllocInfoTableHandle hdl(mm);
  CodeInfoTable *info = static_cast<FuncInfoTable *>
    (mm.allocInfoTable(hdl, wordsof(FuncInfoTable)));
  info->type_ = FUN;
  info->size_ = 1;
  info->tagOrBitmap_ = 0;
  info->layout_.bitmap = 0;
  info->name_ = "stg_STOP";
  info->code_.framesize = 2;
  info->code_.arity = 1;
  info->code_.sizecode = 3;
  info->code_.sizelits = 0;
  info->code_.sizebitmaps = 2;
  info->code_.lits = NULL;
  info->code_.littypes = NULL;
  info->code_.code = static_cast<BcIns *>
                     (mm.allocCode(info->code_.sizecode, info->code_.sizebitmaps));
  BcIns *code = info->code_.code;
  u2 *bitmasks = cast(u2 *, code + info->code_.sizecode);

  code[0] = BcIns::ad(BcIns::kEVAL, 0, 0);  // eval r0;
  code[1] = BcIns::bitmapOffset(byteOffset32(&code[1], bitmasks));
  code[2] = BcIns::ad(BcIns::kSTOP, 0, 0);

  // r0 need not be marked as live because, if GC happens
  // then the evaluation frame will have a copy of r0 which is treated
  // as live.  If r0 is already evaluated, no GC can occur hence we do
  // not need to keep it alive via a bitmask entry.
  bitmasks[0] = 0;
  bitmasks[1] = 0;

  Closure *stg_STOP_closure = mm.allocStaticClosure(0);
  stg_STOP_closure->setInfo((InfoTable *)info);
  MiscClosures::stg_STOP_closure_addr = stg_STOP_closure;
}

void MiscClosures::initBlackholeClosure(MemoryManager &mm) {
  AllocInfoTableHandle hdl(mm);
  ThunkInfoTable *info = static_cast<ThunkInfoTable *>
    (mm.allocInfoTable(hdl, wordsof(ThunkInfoTable)));
  info->type_ = BLACKHOLE;
  info->size_ = 1;
  info->tagOrBitmap_ = 0;
  info->layout_.bitmap = 0;
  info->name_ = "stg_BLACKHOLE";
  info->code_.framesize = 1;
  info->code_.arity = 0;
  info->code_.sizecode = 1;
  info->code_.sizelits = 0;
  info->code_.sizebitmaps = 0;
  info->code_.lits = NULL;
  info->code_.littypes = NULL;
  info->code_.code = static_cast<BcIns *>
                     (mm.allocCode(info->code_.sizecode, info->code_.sizebitmaps));
  BcIns *code = info->code_.code;
  //  u2 *bitmasks = cast(u2 *, code + info->code_.sizecode);
  code[0] = BcIns::ad(BcIns::kSTOP, 1, 0);

  Closure *stg_BLACKHOLE_closure = mm.allocStaticClosure(0);
  stg_BLACKHOLE_closure->setInfo((InfoTable *)info);
  MiscClosures::stg_BLACKHOLE_closure_addr = stg_BLACKHOLE_closure;
}

void MiscClosures::initUpdateClosure(MemoryManager &mm) {
  AllocInfoTableHandle hdl(mm);
  CodeInfoTable *info = static_cast<FuncInfoTable *>
    (mm.allocInfoTable(hdl, wordsof(FuncInfoTable)));
  info->type_ = UPDATE_FRAME;
  info->size_ = 2;
  info->tagOrBitmap_ = 0;
  info->layout_.bitmap = 0;
  info->name_ = "stg_UPD";
  info->code_.framesize = 2;
  info->code_.arity = 1;
  info->code_.sizecode = 5;
  info->code_.sizelits = 0;
  info->code_.sizebitmaps = 2;
  info->code_.lits = NULL;
  info->code_.littypes = NULL;
  info->code_.code = static_cast<BcIns *>
                     (mm.allocCode(info->code_.sizecode, info->code_.sizebitmaps));
  BcIns *code = info->code_.code;
  u2 *bitmasks = cast(u2 *, code + info->code_.sizecode);

  // never executed, only for the bitmasks
  code[0] = BcIns::ad(BcIns::kEVAL, 0, 0);
  code[1] = BcIns::bitmapOffset(byteOffset32(&code[1], bitmasks));
  code[2] = BcIns::ad(BcIns::kMOV_RES, 1, 0);
  code[3] = BcIns::ad(BcIns::kUPDATE, 0, 1);
  // If the update is executed by the interpreter then this return
  // will never be executed (because UPDATE will perform the return on
  // its own).  If the update becomes part of a trace, though, then
  // the guard that gets emitted as part of the return may fail.  We
  // arrange it so that in that case we don't re-execute the UPDATE
  // instruction but instead the failing guard transfers execution to
  // just the RETURN instruction below.
  code[4] = BcIns::ad(BcIns::kIRET, 1, 0);
  bitmasks[0] = 1;  // r0 is live and a pointer
  bitmasks[1] = 1;

  MiscClosures::stg_UPD_return_pc = &code[2];

  Closure *stg_UPD_closure = mm.allocStaticClosure(0);
  stg_UPD_closure->setInfo((InfoTable *)info);
  MiscClosures::stg_UPD_closure_addr = stg_UPD_closure;
}

void MiscClosures::initByteArrInfo(MemoryManager &mm)
{
  AllocInfoTableHandle hdl(mm);
  InfoTable *info = static_cast<InfoTable*>
    (mm.allocInfoTable(hdl, wordsof(InfoTable)));
  info->type_ = LARGE;
  info->size_ = 0;
  info->tagOrBitmap_ = 0;
  info->layout_.bitmap = 0;
  info->name_ = "stg_BYTEARR";

  MiscClosures::stg_BYTEARR_info = info;
}

void MiscClosures::initIndirectionItbl(MemoryManager &mm) {
  AllocInfoTableHandle hdl(mm);
  InfoTable *info = static_cast<InfoTable *>
    (mm.allocInfoTable(hdl, wordsof(InfoTable)));
  info->type_ = IND;
  info->size_ = 1;
  info->tagOrBitmap_ = 1;
  info->layout_.bitmap = 1;
  info->name_ = "stg_IND";

  MiscClosures::stg_IND_info = info;
}

static int formatBitmap(char *p, u4 nargs, u4 pointerMask) {
  if (nargs > 32) nargs = 32;
  for (u4 i = 0; i < nargs; ++i) {
    *p = (pointerMask & 1) ? 'p' : 'n';
    p++;
    pointerMask >>= 1;
  }
  *p = '\0';
  return nargs;
}

static int bitmapSize(u4 bitmap) {
  if (bitmap < (1u << 15))
    return 1;
  if (bitmap < (1u << 30))
    return 2;
  else
    return 3;
}

#define BITMASK_MASK   ((1u << 15) - 1)
#define BITMASK_CONT   (1u << 15)

// Write bitmask to dest.  Returns number of u2's written.
static int encodeBitmask(u2 *dest, u4 bitmap) {
  int i, s;
  u2 m;
  s = bitmapSize(bitmap);
  for (i = 0; i < s; i++) {
    m = (u2)(bitmap & BITMASK_MASK);
    if (i < s - 1)
      m |= BITMASK_CONT;
    *dest = m;
    dest++;
    bitmap >>= 15;
  }
  return s;
}


void MiscClosures::buildApCont(MemoryManager *mm, ApContInfo *out,
                               u4 nargs, u4 pointerMask) {
  LC_ASSERT(nargs < 32);
  LC_ASSERT(pointerMask < (1u << nargs));

  AllocInfoTableHandle hdl(*mm);
  CodeInfoTable *info = static_cast<FuncInfoTable *>
    (mm->allocInfoTable(hdl, wordsof(FuncInfoTable)));
  info->type_ = AP_CONT;
  info->size_ = 0;
  info->tagOrBitmap_ = 0; // pointerMask;
  info->layout_.bitmap = 0; // pointerMask;

  char buf[50];
  char *p = buf;
  p += snprintf(buf, 50, "ApK%d_", nargs);
  p += formatBitmap(p, nargs, pointerMask);
  p += snprintf(p, &buf[50] - p, "`info");
  char *name = mm->allocString(p - buf);
  memcpy(name, buf, p - buf + 1);
  info->name_ = name;

  u4 livemask = (1ul << nargs) - 1;  // everything is live

  info->code_.framesize = nargs + 1;
  info->code_.arity = 1;
  info->code_.sizecode = 5 + 1;
  info->code_.sizelits = 0;
  info->code_.sizebitmaps = bitmapSize(pointerMask) + bitmapSize(livemask);

  info->code_.lits = NULL;
  info->code_.littypes = NULL;
  info->code_.code = static_cast<BcIns *>
                     (mm->allocCode(info->code_.sizecode, info->code_.sizebitmaps));
  BcIns *code = info->code_.code;
  u2 *bitmasks = cast(u2 *, code + info->code_.sizecode);

  LC_ASSERT(nargs <= 8);  // need to change encoding of CALL/CALLT to
  // fix this

  //
  // Code for an APK closure of length N:
  //
  //   IFUNC <N+1>   ; never executed
  //   EVAL <N>      ; never executed
  //   <bitmask>     ; r0..r<N-1> are live, pointers variable
  //   MOV_RES <N>   ;
  //   CALLT <N>, r0..r<N-1>
  //
  code[0] = BcIns::ad(BcIns::kIFUNC, nargs + 1, 0);  // never executed
  code[1] = BcIns::ad(BcIns::kEVAL, nargs, 0); // never executed
  code[2] = BcIns::bitmapOffset(byteOffset32(&code[2], bitmasks));
  code[3] = BcIns::ad(BcIns::kMOV_RES, nargs, 0);
  code[4] = BcIns::abc(BcIns::kCALLT, nargs, 0xff, nargs);
  code[5] = BcIns::pointerInfo(pointerMask);

  bitmasks += encodeBitmask(bitmasks, pointerMask);
  bitmasks += encodeBitmask(bitmasks, livemask);

  Closure *cl = mm->allocStaticClosure(0);
  cl->setInfo(info);

  out->closure = cl;
  out->returnAddr = &code[3];
}

void MiscClosures::initApConts(MemoryManager *mm) {
  smallApConts = new ApContInfo[apContIndex(kMaxSmallArity + 1, 0)];
  for (u4 nargs = 1; nargs <= 4; ++nargs) {
    for (u4 pointerMask = 0; pointerMask < (1u << nargs); ++pointerMask) {
      buildApCont(mm, &smallApConts[apContIndex(nargs, pointerMask)],
                  nargs, pointerMask);
    }
  }
  allocMM = mm;
  otherApConts = new HASH_NAMESPACE::HASH_MAP_CLASS<u4, ApContInfo>(20);
}

void MiscClosures::getApCont(Closure **closure, BcIns **returnAddr,
                             u4 nargs, u4 pointerMask) {
  LC_ASSERT(nargs > 0);
  if (LC_LIKELY(nargs <= kMaxSmallArity)) {
    ApContInfo *inf = &smallApConts[apContIndex(nargs, pointerMask)];
    *closure = inf->closure;
    *returnAddr = inf->returnAddr;
  } else {
    u4 index = apContIndex(nargs, pointerMask);
    APKMAP::iterator i = otherApConts->find(index);
    if (i != otherApConts->end()) {
      // cerr << "Returning existing ApKont" << endl;
      *closure = i->second.closure;
      *returnAddr = i->second.returnAddr;
    } else {
      ApContInfo inf;
      // cerr << "Creating new ApKont" << endl;
      buildApCont(allocMM, &inf, nargs, pointerMask);
      *closure = inf.closure;
      *returnAddr = inf.returnAddr;
      (*otherApConts)[index] = inf;
    }
  }
}

void MiscClosures::initPapItbl(MemoryManager *mm) {
  AllocInfoTableHandle hdl(*mm);
  CodeInfoTable *info = static_cast<FuncInfoTable *>
    (mm->allocInfoTable(hdl, wordsof(FuncInfoTable)));
  info->type_ = PAP;
  info->size_ = 0;  // special layout
  info->tagOrBitmap_ = 0;
  info->layout_.bitmap = 0;
  info->name_ = "stg_PAP";

  info->code_.framesize = 1;
  info->code_.arity = 0;
  info->code_.sizecode = 1;
  info->code_.sizelits = 0;
  info->code_.sizebitmaps = 0;
  info->code_.lits = NULL;
  info->code_.littypes = NULL;
  info->code_.code = static_cast<BcIns *>
                     (mm->allocCode(info->code_.sizecode, info->code_.sizebitmaps));

  BcIns *code = info->code_.code;

  code[0] = BcIns::ad(BcIns::kFUNCPAP, 0, 0);

  MiscClosures::stg_PAP_info = info;
}

InfoTable *MiscClosures::buildApInfo(MemoryManager *mm, u4 nargs, u4 pointerMask) {
  LC_ASSERT(nargs <= 8);
  AllocInfoTableHandle hdl(*mm);
  CodeInfoTable *info = static_cast<CodeInfoTable *>
    (mm->allocInfoTable(hdl, wordsof(CodeInfoTable)));
  info->type_ = THUNK;
  info->size_ = 1 + nargs;
  info->tagOrBitmap_ = (pointerMask << 1) | 1u;
  info->layout_.bitmap = (pointerMask << 1) | 1u;

  char buf[50];
  char *p = buf;
  p += snprintf(buf, 50, "Ap%d_", nargs);
  p += formatBitmap(p, nargs, pointerMask);
  p += snprintf(p, &buf[50] - p, "`info");
  char *name = mm->allocString(p - buf);
  memcpy(name, buf, p - buf + 1);
  info->name_ = name;

  info->code_.framesize = 1 + nargs;
  info->code_.arity = 0;
  info->code_.sizecode = 3 + nargs + 1;
  info->code_.sizelits = 0;
  info->code_.sizebitmaps = 0;
  info->code_.lits = NULL;
  info->code_.littypes = NULL;
  info->code_.code = static_cast<BcIns *>
                     (mm->allocCode(info->code_.sizecode, info->code_.sizebitmaps));

  BcIns *code = info->code_.code;
  // Don't allow AP thunks as a trace entry point.  It's way too
  // polymorphic for that, hence IFUNC instead of FUNC.
  code[0] = BcIns::ad(BcIns::kIFUNC, 1 + nargs, 0);
  code[1] = BcIns::ad(BcIns::kLOADFV, nargs, 1);

  u4 i;
  for (i = 0; i < nargs; ++i) {
    code[2 + i] = BcIns::ad(BcIns::kLOADFV, i, i + 2);
  }

  code[2 + nargs] = BcIns::abc(BcIns::kCALLT, nargs, 0xff, nargs);
  code[3 + nargs] = BcIns::pointerInfo(pointerMask);

  return info;
}

void MiscClosures::initApInfos(MemoryManager *mm) {
  smallApInfos = new InfoTable*[apContIndex(kMaxSmallArity + 1, 0)];
  for (u4 nargs = 1; nargs <= kMaxSmallArity; ++nargs) {
    for (u4 pointerMask = 0; pointerMask < (1U << nargs); ++pointerMask) {
      smallApInfos[apContIndex(nargs, pointerMask)] =
        buildApInfo(allocMM, nargs, pointerMask);
    }
  }
  otherApInfos = new APMAP(10);
}

InfoTable *MiscClosures::getApInfo(u4 nargs, u4 pointerMask) {
  if (LC_LIKELY(nargs <= kMaxSmallArity)) {
    return smallApInfos[apContIndex(nargs, pointerMask)];
  } else {
    u4 index = apContIndex(nargs, pointerMask);
    InfoTable *itbl = (*otherApInfos)[index];
    if (LC_LIKELY(itbl != NULL)) {
      return itbl;
    } else {
      itbl = buildApInfo(allocMM, nargs, pointerMask);
      LC_ASSERT(itbl != NULL);
      (*otherApInfos)[index] = itbl;
      return itbl;
    }
  }
}

void MiscClosures::init(MemoryManager *mm) {
  AllocInfoTableHandle h(*mm); // Prevent lots of mprotect calls
  MiscClosures::initStopClosure(*mm);
  MiscClosures::initBlackholeClosure(*mm);
  MiscClosures::initByteArrInfo(*mm);
  MiscClosures::initUpdateClosure(*mm);
  MiscClosures::initIndirectionItbl(*mm);
  MiscClosures::initApConts(mm);
  MiscClosures::initPapItbl(mm);
  MiscClosures::initApInfos(mm);
}

void MiscClosures::reset() {
  MiscClosures::stg_STOP_closure_addr = NULL;
  MiscClosures::stg_BLACKHOLE_closure_addr = NULL;
  MiscClosures::stg_UPD_return_pc = NULL;
  MiscClosures::stg_UPD_closure_addr = NULL;
  MiscClosures::stg_IND_info = NULL;
  delete[] MiscClosures::smallApConts;
  MiscClosures::smallApConts = NULL;
  delete MiscClosures::otherApConts;
  MiscClosures::otherApConts = NULL;
  MiscClosures::allocMM = NULL;
  MiscClosures::stg_PAP_info = NULL;
  delete[] MiscClosures::smallApInfos;
  delete MiscClosures::otherApInfos;
  MiscClosures::smallApInfos = NULL;
  MiscClosures::otherApInfos = NULL;
}

_END_LAMBDACHINE_NAMESPACE
