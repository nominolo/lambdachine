#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "vm.h"

#define DEFAULT_STACK_SIZE      1024

GblState* init_vm()
{
  GblState *gbl = (GblState*)malloc(sizeof(GblState));
  Prototype *dummy = (Prototype*)malloc(sizeof(Prototype) +
                                        3 * sizeof(BCIns));
  FunClosure *cl = (FunClosure*)malloc(sizeof(FunClosure));
  vm_init_dispatch(gbl->dispatch_tbl);
  dummy->framesize = 1;
  dummy->constants = 0;
  dummy->code[0] = BCINS_AD(OP_IFUNC, 1, 0);
  dummy->code[1] = BCINS_ABC(OP_CALL, 0, 0, 1);
  dummy->code[2] = BCINS_AD(OP_EXIT, 0, 0);
  cl->proto = proto_to_pcproto(dummy);
  gbl->entry_closure = cl;
  return gbl;
}

void stop_vm(GblState *gbl)
{
  Prototype *dummy = cast(Prototype*, gbl->entry_closure->proto - sizeof(Prototype));
  free(dummy);
  free(gbl->entry_closure);
  free(gbl);
}

VMState *create_thread(GblState *gbl)
{
  // TODO: allocate everything on the GC'd heap
  VMState *vms = malloc(sizeof(VMState));
  StgWord *stack = malloc(DEFAULT_STACK_SIZE * sizeof(StgWord));
  
  vms->gbl_state = gbl;
  vms->saved_cstack = 0; // set by interpreter
  vms->stack = stack;
  vms->stack_limit = stack + DEFAULT_STACK_SIZE;
  vms->pc_top = stack + DEFAULT_STACK_SIZE - 2;
  vms->pc_top[1] = cast(StgWord, &cast(BCIns*,gbl->entry_closure->proto)[2]);
  vms->base = stack + 1;
  vms->top = stack + 1;
  stack[0] = cast(StgWord, gbl->entry_closure);
  
  return vms;
}

void destroy_thread(VMState *L)
{
  free(L->stack);
  free(L);
}

void barf(char* last_words)
{
  printf("FATAL: %s\n", last_words);
  exit(1);
}

void lc_stackoverflow()
{
  // TODO:
  barf("Stack overflow");
}

void lc_pushint(VMState *L, int n)
{
  if (L->top >= L->stack_limit)
    lc_stackoverflow();

  *(L->top)++ = cast(StgWord, n);
}

void lc_pushptr(VMState *L, void *p)
{
  if (L->top >= L->stack_limit)
    lc_stackoverflow();
  
  *(L->top)++ = cast(StgWord, p);  
}

void lc_printstate(VMState *L)
{
  StgWord *s;
  int n;

  printf("Global state: %p\n", L->gbl_state);

  if (!(L->top >= L->stack && L->top < L->stack_limit))
    barf("Stack invariant");

  printf("Base: %p\n", L->base);
  printf("Stack (%ld words) %p-%p, @%p:\n",
         L->top - L->stack, L->stack, L->stack_limit, L->top);
  for (s = L->top - 1, n = 0;
       s >= L->stack && n < 16;
       s--, n++) {
    printf("   %2d: %08" FMT_WordX " (%" FMT_Int ")\n", n, *s, *s);
  }

  printf("Return stack: ");
  for (s = L->pc_top + 1, n = 4;
       s < L->stack_limit && n > 0;
       s++, n--) {
    printf("  %0" FMT_WordX, *s);
  }
  printf("\n\n");
}

void test1() {
  GblState gbl;
  VMState *L;
  BCIns testcode[32];
  uint32_t count = 200; // * 1000 * 1000;
  vm_init_dispatch(gbl.dispatch_tbl);
  //gbl.fall = 0;
  L = create_thread(&gbl);
  L->savedpc = testcode;

  lc_pushint(L, 0); // R(0) = 0
  lc_pushint(L, 1); // R(1) = 1
  lc_pushint(L, count); // R(2) = iteration count
  lc_pushint(L, 0); // R(3) = step countre
  lc_printstate(L);
  printf("1st instr: %p\n", testcode);

  // Calculating the jump offset: target instr id - next instr id
/*   testcode[0] = BCINS_AD(OP_KSHORT, 0, -1); */
/*   testcode[1] = BCINS_AD(OP_EXIT, 0, 0); */

  testcode[0] = BCINS_AD(OP_ISLE, 2, 0);
  testcode[1] = BCINS_AJ(OP_JMP, 0, 6);  // goto 8
  testcode[2] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[3] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[4] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[5] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[6] = BCINS_ABC(OP_SUBI_RR, 2, 2, 1);  // decrement counter
  testcode[7] = BCINS_AJ(OP_JMP, 0, -8); // goto 0
  testcode[8] = BCINS_AD(OP_EXIT, 0, 0);

  print_bytecode(testcode);
  
/*   printf("%p\n", vm_leave); */
  printf("Dispatch Table: %p\n", gbl.dispatch_tbl);
  //  c = getc(stdin);

  //  printf("vm: %" FMT_WordX "\n", vm_enter(gbl.dispatch_tbl, 0, L->base, testcode));
  //printf("acc: %x\n", vm_bench(count));
  
  lc_printstate(L);

  destroy_thread(L);
}


/*
int main(int argc, char* argv[])
{
  test2();
  return 0;
}
*/

