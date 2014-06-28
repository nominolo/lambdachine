#include <stdio.h>
#include <stdlib.h>

#include "vm.h"
#include "bc.h"

extern Word bci_ret();
extern Word vm_resume(VMState *L);
extern int vm_init_dispatch(AsmFunction *disp_tbl);
extern AsmFunction vm_leave;

/**
 *
 */
//extern Word vm_call(VMState *L, int nargs, int nresults);

#define DEFAULT_STACK_SIZE      1024

VMState *create_thread(GblState *gbl)
{
  // TODO: allocate everything on the GC'd heap
  VMState* vms = malloc(sizeof(VMState));
  Word* stack = malloc(DEFAULT_STACK_SIZE * sizeof(Word));
  
  vms->gbl_state = gbl;
  vms->saved_cstack = 0; // set by interpreter
  vms->stack = stack;
  vms->stack_limit = stack + DEFAULT_STACK_SIZE;
  vms->base = stack;
  vms->top = stack;
  
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

  *(L->top)++ = (Word)n;
}

void lc_pushptr(VMState *L, void *p)
{
  if (L->top >= L->stack_limit)
    lc_stackoverflow();
  
  *(L->top)++ = (Word)p;  
}

void lc_printstate(VMState *L)
{
  Word *s;
  int n;

  if (!(L->top >= L->stack && L->top < L->stack_limit))
    barf("Stack invariant");

  printf("Stack (top 16 slots) %p-%p:\n", L->stack, L->stack_limit);
  for (s = L->top - 1, n = 16; s >= L->stack && n > 0; s--, n--) {
    printf("    %p: %08x (%d)\n", s, *s, *s);
  }
}

extern uint32_t vm_bench(uint32_t count);

/*
 * Lua C calling interface:
 *
 *  1. Push function
 *  2. Push arguments left to right
 *  3. Use `lua_call(L, nargs, nresults)`.  L is the thread state
 *     nargs is the number of arguments, nresults is the number of
 *     results.
 *  4. After the call, the last result is on top of the stack.
 * 
 */

int main(int argc, char* argv[])
{
  GblState gbl;
  VMState *L;
  BCIns testcode[32];
  uint32_t count = 200 * 1000 * 1000;
  char c;
  vm_init_dispatch(gbl.dispatch_tbl);
  L = create_thread(&gbl);
  L->savedpc = testcode;
  /*
  lc_pushint(L, 0);  // make R(0) = 0
  lc_pushint(L, 1);  // make R(1) = 1
  lc_pushint(L, 23); // R(2)
  lc_pushint(L, 42); // R(3)
  lc_pushint(L, 5);  // R(4)
  lc_printstate(L);
  testcode[0] = BCINS_ABC(OP_ADDI_RR, 4, 0, 0); // R(4) = 0
  testcode[1] = BCINS_AD(OP_ISLT, 2, 3);
  testcode[2] = BCINS_AJ(OP_JMP, 0, 1);
  testcode[3] = BCINS_ABC(OP_ADDI_RR, 4, 0, 1);
  testcode[4] = BCINS_AD(OP_EXIT, 0, 0); // stop VM
  */
  lc_pushint(L, 0); // R(0) = 0
  lc_pushint(L, 1); // R(1) = 1
  lc_pushint(L, count); // R(2) = iteration count
  lc_pushint(L, 0); // R(3) = step countre
  lc_printstate(L);
  printf("1st instr: %p\n", testcode);

  // Calculating the jump offset: target instr id - next instr id
  testcode[0] = BCINS_AD(OP_ISLE, 2, 0);
  testcode[1] = BCINS_AJ(OP_JMP, 0, 6);  // goto 8
  testcode[2] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[3] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[4] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[5] = BCINS_ABC(OP_ADDI_RR, 3, 3, 1);  // Do some work
  testcode[6] = BCINS_ABC(OP_SUBI_RR, 2, 2, 1);  // decrement counter
  testcode[7] = BCINS_AJ(OP_JMP, 0, -8); // goto 0
  testcode[8] = BCINS_AD(OP_EXIT, 0, 0);

  
/*   printf("%p\n", vm_leave); */
  printf("Dispatch Table: %p\n", gbl.dispatch_tbl);
  c = getc(stdin);

  printf("vm: %x\n", vm_resume(L));
  //printf("acc: %x\n", vm_bench(count));
  
  lc_printstate(L);

  destroy_thread(L);
  
  return 0;
}
