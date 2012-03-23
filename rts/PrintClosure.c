#include <stdio.h>
#include "InfoTables.h"
#include "PrintClosure.h"
#include "Bytecode.h"
#include "StorageManager.h"

void printInlineBitmap(FILE *stream, const BCIns *p0);

void
printClosure_(FILE *f, Closure* cl, int nl)
{
  const InfoTable *info = getInfo(cl);

  if (info == NULL) {
    fprintf(f, "???\n");
    return;
  }

  if (info->type == IND) {
    cl = (Closure*)cl->payload[0];
    info = getInfo(cl);
    fprintf(f, "IND -> ");
  }

  switch (info->type) {
  case CONSTR:
    fprintf(f, "%s ", cast(ConInfoTable*,info)->name);
    break;
  case FUN:
    fprintf(f, "%s ", cast(FuncInfoTable*,info)->name);
    break;
  case THUNK:
  case CAF:
    fprintf(f, "%s ", cast(ThunkInfoTable*,info)->name);
    break;
  }

  int n, p = 0;
  u4 bitmap = info->layout.bitmap;
  for (n = info->size; n > 0; p++, n--, bitmap = bitmap >> 1) {
    if (bitmap & 1)
      fprintf(f, "%p ", (Word*)cl->payload[p]);
    else
      fprintf(f, "%" FMT_Int " ", cl->payload[p]);
  }

  /* For when are we going to use ptr/nptr layout
  for (n = info->layout.payload.ptrs; n > 0; p++, n--)
    printf("0x%0" FMT_WordLen FMT_WordX " ", cl->payload[p]);
  for (n = info->layout.payload.nptrs; n > 0; p++, n--)
    printf("%" FMT_Int " ", cl->payload[p]);
  */
  if (nl) 
    fputc('\n', f);
}

u4
printInstruction_aux(FILE *stream, const BCIns *ins /*in*/, int oneline)
{
  const BCIns *ins0 = ins;
  u4 j;
  const BCIns i = *ins;
  const char *name = ins_name[bc_op(i)];

  fprintf(stream, "%p: ", ins);
  ++ ins;

  switch(ins_format[bc_op(i)]) {
  case IFM_R:
    fprintf(stream, "%s\tr%d\n", name, bc_a(i)); break;
  case IFM_RR:
    fprintf(stream, "%s\tr%d, r%d\n", name, bc_a(i), bc_d(i)); break;
  case IFM_RRR:
    fprintf(stream, "%s\tr%d, r%d, r%d\n", name, bc_a(i), bc_b(i), bc_c(i));
    break;
  case IFM_RN:
    fprintf(stream, "%s\tr%d, %d\n", name, bc_a(i), bc_d(i)); break;
  case IFM_RRN:
    fprintf(stream, "%s\tr%d, r%d, %d\n", name, bc_a(i), bc_b(i), bc_c(i));
    break;
  case IFM_RS:
    fprintf(stream, "%s\tr%d, %d\n", name, bc_a(i), bc_sd(i)); break;
  case IFM_J:
    fprintf(stream, "%s\t%p\n", name, ins + bc_j(i)); break;
  case IFM_RRJ:
    fprintf(stream, "%s\tr%d, r%d, %p\n", name, bc_a(i), bc_d(i),
           ins + 1 + bc_j(*ins));
    ins++;
    break;
  case IFM____:
    switch (bc_op(i)) {
    case BC_EVAL:
      { fprintf(stream, "EVAL\tr%d", bc_a(i));
        printInlineBitmap(stream, ins);
        ins++;
      }
      break;
    case BC_CASE:
      { u2 *tgt = (u2*)ins;  u4 ncases = bc_d(i);
        ins += (ncases + 1) / 2;
        fprintf(stream, "CASE\tr%d\n", bc_a(i));
        if (!oneline) {
          for (j = 0; j < ncases; j++, tgt++) {
            fprintf(stream, "         %d: %p\n", j + 1, ins + bc_j_from_d(*tgt));
          }
        }
      }
      break;
    case BC_CASE_S:
      fprintf(stream, "CASE_S\tr%d ...TODO...\n", bc_a(i));
      ins += bc_d(i);
      break;
    case BC_ALLOC1:
      fprintf(stream, "ALLOC1\tr%d, r%d, r%d", bc_a(i), bc_b(i), bc_c(i));
      printInlineBitmap(stream, ins);
      ins += 1;
      break;

    case BC_ALLOC:
      {
        u1 *arg = (u1*)ins; ins += 1 + BC_ROUND(bc_c(i));
        fprintf(stream, "ALLOC\tr%d, r%d", bc_a(i), bc_b(i));
        for (j = 0; j < bc_c(i); j++, arg++)
          fprintf(stream, ", r%d", *arg);
        printInlineBitmap(stream, ins - 1);
      }
      break;
    case BC_ALLOCAP:
      {
        u1 *arg = (u1*)ins; ins += 1 + BC_ROUND(bc_c(i) + 1);
        fprintf(stream, "ALLOCAP\tr%d", bc_a(i));
        u1 ptrmask = bc_b(i);
        fprintf(stream, ", r%d", *arg++);
        for (j = 1; j < bc_c(i) + 1; j++, arg++) {
          fprintf(stream, ", r%d%c", *arg, ptrmask & 1 ? '*' : ' ');
          ptrmask >>= 1;
        }
        printInlineBitmap(stream, ins - 1);
      }
      break;
    case BC_CALL:
      { u1 *arg = (u1*)ins; ins += BC_ROUND(bc_c(i)) + 1;
        fprintf(stream, "%s\tr%d", name, bc_a(i));
        char comma = '(';
        for (j = 0; j < bc_c(i); j++, arg++) {
          fprintf(stream, "%cr%d", comma, *arg);
          comma = ',';
        }
        fprintf(stream, ") [%x]", bc_b(i));
        printInlineBitmap(stream, ins - 1);
      }
      break;
    case BC_CALLT:
      { 
        int j;
        u1 bitmask = bc_b(i);
        fprintf(stream, "CALLT r%d", bc_a(i));
        for (j = 0; j < bc_c(i); j++) {
          fprintf(stream, "%cr%d%c", j == 0 ? '(' : ',', j, bitmask & 1 ? '*' : ' ');
          bitmask >>= 1;
        }
        fprintf(stream, ")\n");
      }
      break;
    default:
      fprintf(stream, "%s ...TODO...\n", name);
    }
    break;
  default:
    fprintf(stderr, "FATAL: Unknown intruction format: %d\n",
            ins_format[bc_op(i)]);
    exit(1);
  }

  return (u4)(ins - ins0);
}

const u2 *
printInlineBitmap_(FILE *stream, const u2 *p)
{
  u2 bitmap;
  int min = 0;
  int i;
  // Live pointers
  fprintf(stream, "(%p) { ", p);
  do {
    bitmap = *p;
    for (i = 0; i < 15 && bitmap != 0; i++) {
      if (bitmap & 1)
        fprintf(stream, "r%d ", min + i);
      bitmap = bitmap >> 1;
    }
    ++p;
  } while (bitmap != 0);
  fprintf(stream, "}");
  return p;
}

void
printInlineBitmap(FILE *stream, const BCIns *p0)
{
  u4 offset = (u4)(*p0);
  if (offset != 0) {
    const u2 *p = (const u2*)((u1*)p0 + offset);
    fputc('\t', stream);
    p = printInlineBitmap_(stream, p); // pointers
    fprintf(stream, " / ");
    printInlineBitmap_(stream, p);   // Live-out variables
    fputc('\n', stream);
  }
}

void
printPointerBitmap(FILE *stream, InfoTable *info)
{
  int i; u4 bitmap;
  i = info->size;
  if (i <= 32) {
    bitmap = info->layout.bitmap;
    fprintf(stream, "  pointers: (%d) ", (int)i);
    while (i > 0) {
      if (bitmap & 1) fputc('*', stream); else fputc('-', stream);
      i--;
      bitmap = bitmap >> 1;
    }
  } else {
    fprintf(stream, "  pointers: ?");
  }
  fputc('\n', stream);
}

void
printPtrNPtr(InfoTable *info)
{
  printf("  ptrs/nptrs: %d/%d\n",
         info->layout.payload.ptrs, info->layout.payload.nptrs);
}

void
printInfoTable(FILE *stream, InfoTable* info0)
{
  switch (info0->type) {
  case CONSTR:
    {
      ConInfoTable* info = (ConInfoTable*)info0;
      fprintf(stream, "Constructor: %s, (%p)\n", info->name, info);
      fprintf(stream, "  tag: %d\n", info->i.tagOrBitmap);
      printPointerBitmap(stream, info0);
    }
    break;
  case FUN:
    {
      FuncInfoTable *info = (FuncInfoTable*)info0;
      fprintf(stream, "Function: %s (%p)\n", info->name, info);
      printPointerBitmap(stream, info0);
      printCode(stream, &info->code);
    }
    break;
  case CAF:
  case THUNK:
  case AP_CONT:
    {
      ThunkInfoTable *info = (ThunkInfoTable*)info0;
      fprintf(stream, "%s: %s (%p)\n",
             info0->type == THUNK ? "Thunk" : "CAF",
             info->name, info);
      printPointerBitmap(stream, info0);
      printCode(stream, &info->code);
    }
    break;
  default:
    fprintf(stream, "Unknown info table\n");
  }
  fprintf(stream, "\n");
}

void
printCode(FILE *stream, LcCode *code)
{
  u4 i; u4 nc = 0; BCIns *c = code->code;
  if (code->arity > 0) {
    fprintf(stream, "  arity: %d, ptrs: ", code->arity);
    // First bitmap is the function pointer map
    printInlineBitmap_(stream, (const u2 *)&code->code[code->sizecode]);
    fputc('\n', stream);
  }
  fprintf(stream, "  frame: %d\n", code->framesize);
  fprintf(stream, "  literals:\n");
  for (i = 0; i < code->sizelits; i++) {
    fprintf(stream, "   %3d: ", i);
    switch (code->littypes[i]) {
    case LIT_INT:
      fprintf(stream, "%" FMT_Int " (i)", (WordInt)code->lits[i]);
      break;
    case LIT_WORD:
      fprintf(stream, "%" FMT_Word " (w)", (Word)code->lits[i]);
      break;
    case LIT_FLOAT:
      fprintf(stream, "%f / %" FMT_WordX, *((float*)&code->lits[i]), code->lits[i]);
      break;
    case LIT_CHAR:
      { Word c = code->lits[i];
	if (c < 256)
	  fprintf(stream, "'%c'", (char)c);
	else
	  fprintf(stream, "u%xd", (u4)c);
      }
      break;
    case LIT_STRING:
      fprintf(stream, "\"%s\"", (char*)code->lits[i]);
      break;
    case LIT_CLOSURE:
      fprintf(stream, "clos %" FMT_WordX " (%s)", code->lits[i],
             getFInfo(code->lits[i])->name);
      break;
    case LIT_INFO:
      fprintf(stream, "info %" FMT_WordX " (%s)", code->lits[i],
             cast(FuncInfoTable*,code->lits[i])->name);
      break;
    default:
      fprintf(stream, "???");
    }
    fprintf(stream, "\n");
  }
  fprintf(stream, "  code:\n");
  while (nc < code->sizecode && bc_op(*c) != BC_STOP) {
    i = printInstruction(stream, c);
    c += i;
    nc += i;
  }
}
