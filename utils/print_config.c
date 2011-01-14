#include "Arch.h"
#include "Common.h"

#include <stdio.h>

int main(int argc, char* argv[]) {
  printf("Detected:\n"
	 "  architecture:  %s\n" 
	 "  bits per word: %d\n"
	 "  endianness:    %s\n",
	 LC_ARCH_NAME,
	 LC_ARCH_BITS,
	 (LAMBDACHINE_BE) ? "big endian" : "little endian");

  printf("sizeof(Word)     = %ld\n"
	 "sizeof(HalfWord) = %ld\n",
	 sizeof(Word), sizeof(HalfWord));
  printf("sizeof(u1) = %ld\n"
	 "sizeof(u2) = %ld\n"
	 "sizeof(u4) = %ld\n"
	 "sizeof(u8) = %ld\n",
	 sizeof(u1), sizeof(u2), sizeof(u4), sizeof(u8));

  return 0;
}
