#ifndef _LAMBDACHINE_VM_H
#define _LAMBDACHINE_VM_H

#include "def.h"
#include "bc.h"

typedef uint32_t BCInst;
typedef struct _VMState VMState;

#ifdef LC_ASM_INTERP
typedef void (*BciFunction)();
#else
typedef BCIns* (*BciFunction)(VMState*,BCIns*,StgWord**);
#endif

typedef struct _Prototype {
  uint8_t       numparams; ///< function arity
  uint8_t       framesize; ///< local variables (including args)
  uint8_t       nresults;  ///< number of results
  uint8_t       unused1;
  uint16_t      nconstants;
  uint16_t      ninstructions;
  StgPtr        constants;
  BCIns         code[];
} Prototype;

typedef struct _FunClosure {
  Prototype     *proto;
  StgWord       free_vars[];
} FunClosure;

typedef struct _GblState {
  BciFunction   dispatch_tbl[256]; // not sure if this should be global
  FunClosure*   entry_closure;
} GblState;

struct _VMState {
  GblState *gbl_state;  // must be first
  StgWord *saved_cstack;
  StgWord *base;        ///< base of current function
  StgWord *kbase;
  StgWord *top;         ///< first free slot in the stack
  StgWord *pc_top;      ///< first return save address
  StgWord *stack;       ///< stack base
  StgWord *stack_limit; ///< end of stack
  StgWord *dispatch;
  BCIns *pc;
  BCIns *savedpc;
};

extern BciFunction vm_exit;

typedef struct _StgHeader {
  
} StgHeader;

GblState* init_vm();
void stop_vm(GblState *gbl);

VMState *create_thread(GblState *gbl);
void destroy_thread(VMState *L);

#ifdef LC_ASM_INTERP
#define LC_ASM_FUN extern
#else
#define LC_ASM_FUN
#endif

LC_ASM_FUN void vm_init_dispatch(BciFunction *dispatch);
LC_ASM_FUN StgWord vm_call(VMState *L, int nargs);

#define pc_to_proto(pc) cast(Prototype*, (char*)(pc) - offsetof(Prototype, code))
#define proto_to_pcproto(clos) cast(Prototype*, clos->code)

void barf(char* last_words);
void lc_stackoverflow();
void lc_pushint(VMState *L, int n);
void lc_pushptr(VMState *L, void *p);
void lc_printstate(VMState *L);

#endif
