#import "IR.h"
#import "Arch.h"

#import <stdio.h>
#import <limits.h>

int main(int argc, char *argv[]) {
  IRIns ir;
  ir.op1 = 0x0102;
  ir.op2 = 0x0304;
  ir.ot = 0x0506;
  ir.prev = 0x0708;

  printf("%x\n", ir.op12);
}
