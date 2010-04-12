#include <stdio.h>
#include <stdlib.h>

// #include "arch.h"
#include "vm.h"

#define DEF_BCI(name)	BCIns* bci_##name(VMState *L, BCIns *pc, StgWord **disp_ref)

#define DECODE_COMMON  StgWord *base = L->base; BCIns ins = *pc++
#define DECODE_A       DECODE_COMMON; unsigned ra = bc_a(ins)
#define DECODE_ABC     DECODE_COMMON; unsigned ra = bc_a(ins); \
  unsigned rb = bc_b(ins); unsigned rc = bc_c(ins)
#define DECODE_AD      DECODE_COMMON; unsigned ra = bc_a(ins); \
  unsigned rd = bc_d(ins)
#define DECODE_AC      DECODE_COMMON; unsigned ra = bc_a(ins); \
  unsigned rc = bc_c(ins)

DEF_BCI(exit)
{
  return 0;
}

DEF_BCI(addi_rr)
{
  DECODE_ABC;
  base[ra] = cast(StgWord, cast(long,base[rb]) + cast(long,base[rc]));
  return pc;
}

DEF_BCI(subi_rr)
{
  DECODE_ABC;
  base[ra] = cast(StgWord, cast(long,base[rb]) - cast(long,base[rc]));
  return pc;
}

DEF_BCI(muli_rr)
{
  DECODE_ABC;
  base[ra] = cast(StgWord, cast(long,base[rb]) * cast(long,base[rc]));
  return pc;
}

DEF_BCI(divi_rr)
{
  DECODE_ABC;
  base[ra] = cast(StgWord, cast(long,base[rb]) / cast(long,base[rc]));
  return pc;
}

DEF_BCI(mov)
{
  DECODE_AC;
  base[ra] = base[rc];
  return pc;
}

DEF_BCI(loadk)
{
  DECODE_AD;
  StgWord *kbase = L->kbase;
  base[ra] = kbase[rd];
  return pc;
}

DEF_BCI(kshort)
{
  DECODE_COMMON;
  unsigned ra = bc_a(ins);
  int k = cast(StgWord,bc_sd(ins));
  base[ra] = k;
  return pc;
}

DEF_BCI(jmp)
{
  DECODE_COMMON;
  return pc + bc_j(ins);
}

/* 
   Conditional branch instructions are always followed by a JMP.
   If the condition is true, then we immediately execute this jump.
 */

DEF_BCI(islt)
{
  DECODE_AC;
  if ((StgInt)base[ra] < (StgInt)base[rc])
    return pc + bc_j(*pc) + 1;
  else
    return pc + 1;
}

DEF_BCI(isge)
{
  DECODE_AC;
  if ((StgInt)base[ra] >= (StgInt)base[rc])
    return pc + bc_j(*pc) + 1;
  else
    return pc + 1;
}

DEF_BCI(isgt)
{
  DECODE_AC;
  if ((StgInt)base[ra] > (StgInt)base[rc])
    return pc + bc_j(*pc) + 1;
  else
    return pc + 1;
}

DEF_BCI(isle)
{
  DECODE_AC;
  if ((StgInt)base[ra] <= (StgInt)base[rc])
    return pc + bc_j(*pc) + 1;
  else
    return pc + 1;
}

DEF_BCI(iseq)
{
  DECODE_AC;
  if ((StgInt)base[ra] == (StgInt)base[rc])
    return pc + bc_j(*pc) + 1;
  else
    return pc + 1;
}

DEF_BCI(isne)
{
  DECODE_AC;
  if ((StgInt)base[ra] != (StgInt)base[rc])
    return pc + bc_j(*pc) + 1;
  else
    return pc + 1;
}

DEF_BCI(call)
{
  DECODE_AC;
  FunClosure *node = (FunClosure*)base[ra];
  Prototype *proto = pc_to_proto(node->proto);
  StgWord *new_base = &base[ra+1];
  L->base = new_base;
  *(L->pc_top--) = (StgWord)pc;
  return proto->code;
}

DEF_BCI(ifunc)
{
  DECODE_A;
  StgWord *new_top = base + ra;
  Prototype *proto = pc_to_proto(pc-1);
  //    cast(Prototype*, (char*)(pc-1) - offsetof(Prototype, code));
  L->top = new_top;
  if (new_top >= L->pc_top)
    barf("Stack overflow");
  L->kbase = proto->constants;
  return pc;
}

DEF_BCI(ret1)
{
  DECODE_A;
  unsigned args;
  FunClosure *node;
  Prototype *proto;
  base[0] = base[ra]; // copy down result
  pc = L->pc_top++;   // fetch return address
  // reconstruct stack frame
  ins = *(pc-1);      // the calling instruction
  args = bc_a(ins);
  base = base - (args + 1);
  L->base = base;

  node = (FunClosure*)base[-1];
  proto = pc_to_proto(node->proto);
  L->kbase = proto->constants;
  L->top = base + proto->framesize;
  return pc;
}

void vm_init_dispatch(BciFunction *dispatch) {
  dispatch[OP_EXIT] = &bci_exit;
  dispatch[OP_ADDI_RR] = &bci_addi_rr;
  dispatch[OP_SUBI_RR] = &bci_subi_rr;
  dispatch[OP_MULI_RR] = &bci_muli_rr;
  dispatch[OP_DIVI_RR] = &bci_divi_rr;
  dispatch[OP_MOV] = &bci_mov;
  dispatch[OP_LOADK] = &bci_loadk;
  dispatch[OP_KSHORT] = &bci_kshort;
  dispatch[OP_JMP] = &bci_jmp;
  dispatch[OP_ISLT] = &bci_islt;
  dispatch[OP_ISGE] = &bci_isge;
  dispatch[OP_ISGT] = &bci_isgt;
  dispatch[OP_ISLE] = &bci_isle;
  dispatch[OP_ISEQ] = &bci_iseq;
  dispatch[OP_ISNE] = &bci_isne;
  dispatch[OP_CALL] = &bci_call;
  dispatch[OP_IFUNC] = &bci_ifunc;
  dispatch[OP_RET1] = &bci_ret1;
}

StgWord vm_call(VMState *L, int nargs)
{
  BciFunction *dispatch = L->gbl_state->dispatch_tbl;
  FunClosure *node = cast(FunClosure*,L->base[0]);
  Prototype *proto = pc_to_proto(node->proto);
  BCIns *pc = proto->code;
  L->pc = pc;
  L->top = L->base + nargs;
  L->base = L->base + 1;
  L->kbase = proto->constants;

  while (pc > 0) {
    L->pc = pc;
    pc = dispatch[bc_op(*pc)](L, pc, cast(StgWord**,&dispatch));
  }
  return 1;
}

int test_interp(BciFunction *dispatch, VMState *L, BCIns *code) {
  BCIns *pc = code;
  while (pc > 0) {
    L->pc = pc;
    pc = dispatch[bc_op(*pc)](L, pc, cast(StgWord**,&dispatch));
  }
  return 1;
}

void test3()
{
  GblState *vm = init_vm();
  VMState *L = create_thread(vm);
  BCIns testcode[32];
  //  BciFunction dispatch[256];

  printf("sizeof(StgWord) = %ld\n", sizeof(StgWord));

  /* dispatch[OP_EXIT] = &bci_exit; */
  /* dispatch[OP_ADDI_RR] = &bci_addi_rr; */
  
  /* testcode[0] = BCINS_ABC(OP_MULI_RR, 0, 1, 2); */
  /* testcode[1] = BCINS_AD(OP_KSHORT, 1, -1); */
  testcode[0] = BCINS_AD(OP_ISLT, 2, 1);
  testcode[1] = BCINS_AJ(OP_JMP, 0, 1);
  testcode[2] = BCINS_AD(OP_KSHORT, 0, -1);
  testcode[3] = BCINS_AD(OP_EXIT, 0, 0);

  lc_pushint(L, 2); // R(0)
  lc_pushint(L, 3); // R(1)
  lc_pushint(L, 4); // R(2)
  lc_printstate(L);
  
  test_interp(L->gbl_state->dispatch_tbl, L, testcode);

  lc_printstate(L);
}

void test2()
{
  GblState *gbl = init_vm();
  VMState *L;
  Prototype* proto = (Prototype*)malloc(sizeof(proto) + 32 * sizeof(BCIns));
  FunClosure clos;
  StgPtr constants[1];

  L = create_thread(gbl);

  proto->numparams = 2;
  proto->framesize = 4;
  proto->nresults = 1;
  proto->nconstants = 1;
  constants[0] = cast(StgPtr, &clos);
  proto->constants = cast(StgPtr, constants);
  proto->code[0] = BCINS_AD(OP_IFUNC, 4, 0);
  proto->code[1] = BCINS_AD(OP_KSHORT, 0, 0);
  proto->code[2] = BCINS_AD(OP_KSHORT, 1, 1);
  proto->code[3] = BCINS_AD(OP_KSHORT, 2, 100);
  proto->code[4] = BCINS_AD(OP_ISLE, 2, 0);
  proto->code[5] = BCINS_AJ(OP_JMP, 0, 6);  // goto 8
  proto->code[6] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  proto->code[7] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  proto->code[8] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  proto->code[9] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  proto->code[10] = BCINS_ABC(OP_SUBI_RR, 2, 2, 1);  // decrement counter
  proto->code[11] = BCINS_AJ(OP_JMP, 0, -8); // goto 0
  proto->code[12] = BCINS_AD(OP_RET1, 1, 0);
  proto->code[12] = 0;
  
  printf("Gbl: %p,  TSO: %p\n", gbl, L);
  printf("code: %p,  clos: %p\n", proto->code, &clos);
  print_bytecode((BCIns*)proto->code);
  print_bytecode((BCIns*)gbl->entry_closure->proto);
  
  clos.proto = cast(Prototype*,proto->code);
  // no free variables

  lc_pushptr(L, &clos);
  lc_printstate(L);
  vm_call(L, 1);
  lc_printstate(L);
  
  free(proto);
  destroy_thread(L);
}


int main(int argc, char* argv[])
{
  //StgWord w = 42;
  //printf("word %ld", sizeof(long));
  test2();
  
  return 0;
}


