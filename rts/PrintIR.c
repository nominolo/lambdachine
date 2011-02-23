#include "PrintIR.h"
#include "InfoTables.h"
#include "PrintClosure.h"

#include <stdio.h>

// -- Convenience macros.  Undefined at end of file. -----------------

// Pointer to referenced IR.
#define IR(ref)     (&J->cur.ir[(ref)])

// -------------------------------------------------------------------

const char *ir_type_names[] = {
  [IRT_UNK]  = "?  ",
  [IRT_VOID]  = "-  ",
  [IRT_I32]  = "i32",
  [IRT_U32]  = "u32",
  [IRT_CHAR] = "chr",
  [IRT_F32]  = "flt",

  [IRT_CLOS] = "cls",
  [IRT_INFO] = "itb",
  [IRT_PC]   = "pc ",
  [IRT_PTR]  = "ptr",
};

// foo -> "foo"
#define STR(x) #x

const char *ir_name[] = {
#define IRNAME(name,flags,mode1,mode2)  [IR_##name] = STR(name),
  IRDEF(IRNAME)
#undef IRNAME
};

const u1 ir_mode[IR__MAX + 1] = {
  IRDEF(IRMODE)
  0
};

INLINE_HEADER const char *irt_str(IRType irt)
{
  return ir_type_names[irt & IRT_TYPE];
}

void
printIRRef_(JitState *J, IRRef1 ref, char *comment, int *lencomment, int maxlen)
{
  if (ref < REF_BIAS) {
    printf("K%03d ", REF_BIAS - ref);
    // Add a comment describing the value of the literal
    IRIns *ir = IR(ref);
    if (ir->o == IR_KWORD && comment != NULL) {
      int n;
      switch (ir->t) {
      case IRT_CLOS:
        {
          Closure *cl = (Closure*)J->kwords[ir->u];
          n = snprintf(comment + *lencomment, maxlen - *lencomment,
                       "%s  ", getFInfo(cl)->name);
          *lencomment += n;
        }
        break;

      case IRT_INFO:
        {
          //printInfoTable((InfoTable*)J->kwords[ir->u]);
          FuncInfoTable *info = (FuncInfoTable*)J->kwords[ir->u];
          LC_ASSERT(info != NULL && info->name != NULL);
          n = snprintf(comment + *lencomment, maxlen - *lencomment,
                       "%s  ", info->name);
          *lencomment += n;
        }
        break;

      case IRT_I32:
        n = snprintf(comment + *lencomment, maxlen - *lencomment,
                     "%" FMT_Int "  ", (WordInt)J->kwords[ir->u]);
        *lencomment += n;
        break;

      default:
        break;
      }
    }
  } else
    printf("%04d ", ref - REF_BIAS);
}

void
printIRRef(JitState *J, IRRef1 ref)
{
  if (ref == 0)
    printf("---- ");
  else if (ref < REF_BIAS)
    printf("K%03d ", (int)(REF_BIAS - ref));
  else
    printf("%04d ", (int)(ref - REF_BIAS));
}

#define MAX_COMMENT 100

void
printIR(JitState *J, IRIns ir)
{
  char comment[MAX_COMMENT];
  int  lencomment = 0;

  if (ir.o == IR_LOOP) {
    printf("=== LOOP =============\n");
    return;
  }

  printf("%3s %s%s%-8s ", irt_str(ir.t),
         irt_getphi(ir.t) ? "%" : " ",
         irt_getmark(ir.t) ? "*" : " ",
         ir_name[ir.o]);
  switch (irm_op1(ir_mode[ir.o])) {
  case IRMref:
    printIRRef_(J, ir.op1, comment, &lencomment, MAX_COMMENT);
    break;
  case IRMlit:
    if (ir.o == IR_SLOAD)
      printf("%4d ", (IRRef1)ir.op1 - J->baseslot);
    else
      printf("%4d ", (IRRef1)ir.op1);
    break;
  case IRMcst:
    if (ir.o == IR_KWORD) {
      printf("  0x%08" FMT_WordX, J->kwords[ir.u]);
    } else
      printf(" %11d", ir.i);
    break;
  case IRMnone:  printf("     "); break;
  }

  switch (irm_op2(ir_mode[ir.o])) {
  case IRMref:
    printIRRef_(J, ir.op2, comment, &lencomment, MAX_COMMENT);
    break;
  case IRMlit: printf("%4d ", (IRRef1)ir.op2); break;
  case IRMcst: printf("%11d ", ir.i); break;
  case IRMnone:
    if (irm_op1(ir_mode[ir.o]) != IRMcst) printf("     ");
    break;
  }

  if (lencomment > 0)
    printf("  ; %s", comment);
  //  printf("  prev: %d", ir.prev ? ir.prev - REF_BIAS : 0);

  printf("\n");
}

#undef IR
