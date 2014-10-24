#include <stddef.h>

#include <gmp.h>

#define OFFSET(s_type, field) ((size_t)&(((s_type*)0)->field))

int
main(int argc, char *argv[])
{
  mpz_t t;

  printf("sizeof(mpz_t) = %ld\n"
         "offsetof(MP_INT, _mp_alloc) = %ld (%ld)\n"
         "offsetof(MP_INT, _mp_size) = %ld (%ld)\n"
         "offsetof(MP_INT, _mp_d) = %ld (%ld)\n",
         sizeof(t),
         offsetof(MP_INT, _mp_alloc), sizeof(((MP_INT*)0)->_mp_alloc),
         offsetof(MP_INT, _mp_size), sizeof(((MP_INT*)0)->_mp_size),
         offsetof(MP_INT, _mp_d), sizeof(((MP_INT*)0)->_mp_d));

  return 0; 
}
