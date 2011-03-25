#include "PrintIR.h"
#include "InfoTables.h"
#include "PrintClosure.h"
#include "HeapInfo.h"

#include <stdio.h>

// -- Convenience macros.  Undefined at end of file. -----------------

// Pointer to referenced IR.
#define IR(ref)     (&F->ir[(ref)])

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
printIRRef_(Fragment *F, IRRef1 ref, char *comment, int *lencomment, int maxlen)
{
  if (ref < REF_BIAS) {
    printf("K%03d ", REF_BIAS - ref);
    // Add a comment describing the value of the literal
    IRIns *ir = &F->ir[ref];
    if (ir->o == IR_KWORD && comment != NULL) {
      int n;
      switch (ir->t) {
      case IRT_CLOS:
        {
          Closure *cl = (Closure*)F->kwords[ir->u];
          n = snprintf(comment + *lencomment, maxlen - *lencomment,
                       "%s  ", getFInfo(cl)->name);
          *lencomment += n;
        }
        break;

      case IRT_INFO:
        {
          //printInfoTable((InfoTable*)J->kwords[ir->u]);
          FuncInfoTable *info = (FuncInfoTable*)F->kwords[ir->u];
          LC_ASSERT(info != NULL && info->name != NULL);
          n = snprintf(comment + *lencomment, maxlen - *lencomment,
                       "%s  ", info->name);
          *lencomment += n;
        }
        break;

      case IRT_I32:
        n = snprintf(comment + *lencomment, maxlen - *lencomment,
                     "%" FMT_Int "  ", (WordInt)F->kwords[ir->u]);
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
printIRRef(Fragment *F, IRRef1 ref)
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
printIR(Fragment *F, IRIns ir)
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
  fflush(stdout);
  switch (irm_op1(ir_mode[ir.o])) {
  case IRMref:
    printIRRef_(F, ir.op1, comment, &lencomment, MAX_COMMENT);
    break;
  case IRMlit:
    if (ir.o == IR_SLOAD)
      printf("%4d ", (IRRef1)ir.op1 - 1);
    else
      printf("%4d ", (IRRef1)ir.op1);
    break;
  case IRMcst:
    if (ir.o == IR_KWORD) {
      printf("  0x%08" FMT_WordX, F->kwords[ir.u]);
    } else
      printf(" %11d", ir.i);
    break;
  case IRMnone:  printf("     "); break;
  }

  switch (irm_op2(ir_mode[ir.o])) {
  case IRMref:
    printIRRef_(F, ir.op2, comment, &lencomment, MAX_COMMENT);
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

// -------------------------------------------------------------------
// Printing IR in a C-like syntax.
//
// Example:
//
//    c031 = new I# i022
//    guard hasItbl(t044)
//

const char ir_type_char[] = {
  [IRT_UNK]  = 'U',
  [IRT_VOID] = ' ',
  [IRT_I32]  = 'i',
  [IRT_U32]  = 'u',
  [IRT_CHAR] = 'c',
  [IRT_F32]  = 'f',

  [IRT_CLOS] = 'p',
  [IRT_INFO] = 't',
  [IRT_PTR]  = 'r',
  [IRT_PC]   = 'y'
};

// NO "l" and "O" due to possible confusion with "1" and "0"
const char ir_baseX[60] =
  "0123456789abcdefghijkmnopqrstuvwxyzABCDEFGHIJKLMNPQRSTUVWXYZ";

INLINE_HEADER char irt_chr(IRType irt)
{
  return ir_type_char[irt & IRT_TYPE];
}

void
printPrettyIRRef_(Fragment *F, IRRef ref, int follow)
{
  LC_ASSERT(F->nk < ref && ref < F->nins);

  IRIns *ir = IR(ref);

  if (ref >= REF_BIAS) {
    int n = ref - REF_BIAS;
    if (follow && ir->o == IR_ILOAD) {
      printf("getInfo(");
      printPrettyIRRef_(F, ir->op1, 1);
      putchar(')');
      return;
    }
    printf(COLOURED(COL_GREEN, "%c%03d"), irt_chr(ir->t), n);
  } else {
    int n = REF_BIAS - ref;
    if (ir->o == IR_KWORD) {
      switch (irt_type(ir->t)) {
      case IRT_I32:
        printf(COLOURED(COL_BLUE, "%" FMT_Int),
               (WordInt)F->kwords[ir->u]);
        break;
      case IRT_INFO:
        {
          FuncInfoTable *info = (FuncInfoTable*)F->kwords[ir->u];
          LC_ASSERT(info != NULL && info->name != NULL);
          printf(COLOURED(COL_BLUE, "%s"), info->name);
        }
        break;
      default:
        printf(COLOURED(COL_BLUE, "%cK%02d"), irt_chr(ir->t), n);
      }
    } else {
      printf(COLOURED(COL_BLUE, "%cK%02d"), irt_chr(ir->t), n);
    }
  }
}

INLINE_HEADER void
printPrettyIRRef(Fragment *F, IRRef ref)
{
  printPrettyIRRef_(F, ref, 1);
}

const char *ir_cmp_name[IR_NE - IR_LT + 1] = {
  [IR_LT - IR_LT] = "< ",
  [IR_GE - IR_LT] = ">=",
  [IR_LE - IR_LT] = "<=",
  [IR_GT - IR_LT] = "> ",
  [IR_EQ - IR_LT] = "==",
  [IR_NE - IR_LT] = "!="
};

INLINE_HEADER int
printedByPretty(Fragment *F, IRRef ref)
{
  LC_ASSERT(F->nk < ref && ref < F->nins);

  IRIns *ir = IR(ref);

  if (ir->o == IR_FREF || ir->o == IR_ILOAD || ir->o == IR_PHI ||
      ir->o == IR_NOP || ir->o == IR_FRAME || ir->o == IR_RET)
    // These instructions are no-ops or will be merged with their use
    // sites, so we don't print them.
    return 0;

  if (ir->o == IR_NEW) {
    LC_ASSERT(ir->op2 < F->nheap);

    if (!(F->heap[ir->op2].loop & 1))
      return 0;
  }

  return 1;
}

const char *ir_op_name[IR__MAX] = {
  [IR_ADD] = "+",
  [IR_SUB] = "-",
  [IR_MUL] = "*",
  [IR_DIV] = "/",
  [IR_REM] = "rem"
};

void
printPrettyIRIns(Fragment *F, IRRef ref)
{
  LC_ASSERT(F->nk < ref && ref < F->nins);

  IRIns *ir = IR(ref);

  if (!printedByPretty(F, ref))
    return;

  printf("| ");
  if ((ir->t & IRT_TYPE) != IRT_VOID && ir->o != IR_PHI) {
    printPrettyIRRef_(F, ref, 0);
    printf(" = ");
  } else
    printf("       ");

  switch (ir->o) {
  case IR_SLOAD:
    printf("base[%d]", (int)(ir->op1 - 1));
    break;
  case IR_ADD: case IR_SUB: case IR_MUL: case IR_DIV: case IR_REM:
    printPrettyIRRef(F, ir->op1);
    printf(" %s ", ir_op_name[ir->o]);
    printPrettyIRRef(F, ir->op2);
    break;
  case IR_LT: case IR_GT: case IR_LE:
  case IR_GE: case IR_EQ: case IR_NE:
    printf("guard (");
    printPrettyIRRef(F, ir->op1);
    printf(" %s ", ir_cmp_name[ir->o - IR_LT]);
    printPrettyIRRef(F, ir->op2);
    putchar(')');
    // TODO: Print snapshot / live-outs
    break;
  case IR_FLOAD:
    ir = IR(ir->op1);
    printPrettyIRRef(F, ir->op1);
    printf("[%d]", (int)ir->op2);
    break;
  case IR_LOOP:
    // TODO: Print PHIs here
    printf("--- LOOP --------");
    {
      IRRef firstphi = REF_DROP;
      IRRef phiref;
      // 1. Find first PHI instruction
      for (phiref = F->nins - 1;
           IR(phiref)->o == IR_PHI || IR(phiref)->o == IR_NOP;
           phiref--) {
        if (IR(phiref)->o == IR_PHI)
          firstphi = phiref;
      }

      for (phiref = firstphi; phiref < F->nins; phiref++) {
        IRIns *phi = IR(phiref);
        if (phi->o == IR_PHI && printedByPretty(F, phi->op1)) {
          printf("\n| ");
          printPrettyIRRef(F, phi->op1);
          printf(" = phi(");
          printPrettyIRRef(F, phi->op1);
          printf(", ");
          printPrettyIRRef(F, phi->op2);
          putchar(')');
        }
      }
    }
    break;
  case IR_UPDATE:
    printf("update(");
    printPrettyIRRef(F, ir->op1);
    putchar(',');
    printPrettyIRRef(F, ir->op2);
    putchar(')');
    break;
  case IR_NEW:
    printf("new ");
    printPrettyIRRef(F, ir->op1);
    {
      LC_ASSERT(ir->op2 < F->nheap);
      HeapInfo *hp = &F->heap[ir->op2];
      int i;
      putchar('(');
      for (i = 0; i < hp->nfields; i++) {
        if (i != 0) printf(", ");
        printPrettyIRRef(F, getHeapInfoField(F, hp, i));
      }
      putchar(')');
    }
    break;
  default:
    printf("{TODO: %s}", ir_name[ir->o]);
  }
  putchar('\n');
}

void
printPrettyIR(Fragment *F, int fragment_id)
{
  IRRef ref;
  printf("+==== Fragment: %04d =============================\n",
         fragment_id);
  for (ref = REF_FIRST; ref < F->nins; ref++)
    printPrettyIRIns(F, ref);
  printf("+=================================================\n");
}

#undef IR
