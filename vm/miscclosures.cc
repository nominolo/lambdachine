#include "miscclosures.hh"

_START_LAMBDACHINE_NAMESPACE

Closure *MiscClosures::stg_UPD_closure_addr = NULL;
BcIns *MiscClosures::stg_UPD_return_pc = NULL;
Closure *MiscClosures::stg_STOP_closure_addr = NULL;
InfoTable *MiscClosures::stg_IND_info = NULL;

void MiscClosures::initStopClosure(MemoryManager &mm) {
  CodeInfoTable *info = static_cast<FuncInfoTable*>
    (mm.allocInfoTable(wordsof(FuncInfoTable)));
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
  info->code_.code = static_cast<BcIns*>
    (mm.allocCode(info->code_.sizecode, info->code_.sizebitmaps));
  BcIns *code = info->code_.code;
  u2 *bitmasks = cast(u2*, code + info->code_.sizecode);

  code[0] = BcIns::ad(BcIns::kEVAL, 0, 0);  // eval r0;
  code[1] = BcIns::bitmapOffset(byte_offset(&code[1], bitmasks));
  code[2] = BcIns::ad(BcIns::kSTOP, 0, 0);

  // r0 need not be marked as live because, if GC happens
  // then the evaluation frame will have a copy of r0 which is treated
  // as live.  If r0 is already evaluated, no GC can occur hence we do
  // not need to keep it alive via a bitmask entry.
  bitmasks[0] = 0;
  bitmasks[1] = 0;

  Closure *stg_STOP_closure = mm.allocStaticClosure(0);
  stg_STOP_closure->setInfo((InfoTable*)info);
  MiscClosures::stg_STOP_closure_addr = stg_STOP_closure;
}

void MiscClosures::initUpdateClosure(MemoryManager &mm) {
  CodeInfoTable *info = static_cast<FuncInfoTable*>
    (mm.allocInfoTable(wordsof(FuncInfoTable)));
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
  info->code_.code = static_cast<BcIns*>
    (mm.allocCode(info->code_.sizecode, info->code_.sizebitmaps));
  BcIns *code = info->code_.code;
  u2 *bitmasks = cast(u2*, code + info->code_.sizecode);

  // never executed, only for the bitmasks
  code[0] = BcIns::ad(BcIns::kEVAL, 0, 0);
  code[1] = BcIns::bitmapOffset(byte_offset(&code[1], bitmasks));
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
  stg_UPD_closure->setInfo((InfoTable*)info);
  MiscClosures::stg_UPD_closure_addr = stg_UPD_closure;
}

void MiscClosures::initIndirectionItbl(MemoryManager& mm) {
  InfoTable *info = static_cast<InfoTable*>
    (mm.allocInfoTable(wordsof(InfoTable)));
  info->type_ = IND;
  info->size_ = 1;
  info->tagOrBitmap_ = 1;
  info->layout_.bitmap = 1;
  info->name_ = "stg_IND";

  MiscClosures::stg_IND_info = info;
}

void MiscClosures::init(MemoryManager& mm) {
  MiscClosures::initStopClosure(mm);
  MiscClosures::initUpdateClosure(mm);
  MiscClosures::initIndirectionItbl(mm);
}

void MiscClosures::reset() {
  MiscClosures::stg_STOP_closure_addr = NULL;
  MiscClosures::stg_UPD_return_pc = NULL;
  MiscClosures::stg_UPD_closure_addr = NULL;
  MiscClosures::stg_IND_info = NULL;
}

_END_LAMBDACHINE_NAMESPACE
