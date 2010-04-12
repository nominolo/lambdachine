#include <stdio.h>

#include "bc.h"

char op_char(BCIns);
const char *op_rel(BCIns);

int BCIns_print(BCIns *ip, int ic)
{
  BCIns i = *ip;
  printf(" %4d:  ", ic);
  switch (bc_op(i)) {
  case OP_EXIT:
    printf("exit\n"); break;
  case OP_ADDI_RR:
  case OP_SUBI_RR:
  case OP_MULI_RR:
  case OP_DIVI_RR:
    printf("r%d <- r%d %c r%d\n", bc_a(i), bc_b(i), op_char(i), bc_c(i));
    break;
  case OP_MOV:
    printf("r%d <- r%d\n", bc_a(i), bc_d(i));
    break;
  case OP_KSHORT:
    printf("r%d <- %d  ; 0x%x\n", bc_a(i), bc_sd(i), bc_sd(i));
    break;
  case OP_JMP:
    printf("goto %d\n", ic + 1 + cast(int,bc_j(i)));
    break;
  case OP_ISLT: case OP_ISGT: case OP_ISLE: case OP_ISGE:
  case OP_ISEQ: case OP_ISNE:
    printf("if r%d %s r%d ", bc_a(i), op_rel(i), bc_d(i));
    i = *(ip+1); ++ic;
    printf("goto %d\n", ic + 1 + cast(int,bc_j(i)));
    return 2;
  case OP_IFUNC:
    printf("ifunc %d\n", bc_a(i));
    break;
  case OP_CALL:
    printf("call r%d(", bc_a(i));
    if (bc_c(i) > 1) {
      printf("r%d", bc_a(i) + 1);
      if (bc_c(i) > 2) {
        printf(",..,r%d", bc_a(i) + bc_c(i) - 1);
      }
    }
    printf(")\n");
    break;
  case OP_RET1:
    printf("ret %d\n", bc_a(i));
    break;
  default:
    printf("(unknown)\n");
  }
  return 1;
}

void print_bytecode(BCIns *ip)
{
  int local_addr = 0;
  BCIns i;
  int isize;
  do {
    i = *ip;
    isize = BCIns_print(ip, local_addr);
    local_addr += isize;
    ip += isize;
  } while (bc_op(i) != OP_EXIT);
}

char op_char(BCIns i)
{
  switch (bc_op(i)) {
  case OP_ADDI_RR: return '+';
  case OP_SUBI_RR: return '-';
  case OP_MULI_RR: return '*';
  case OP_DIVI_RR: return '/';
  }
  return ' ';
}

const char* op_rel(BCIns i)
{
  switch (bc_op(i)) {
  case OP_ISLT: return "<";
  case OP_ISGE: return ">=";
  case OP_ISLE: return "<=";
  case OP_ISGT: return ">";
  case OP_ISEQ: return "==";
  case OP_ISNE: return "/=";
  default:
    return "??";
  }
}
