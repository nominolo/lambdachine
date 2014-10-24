#include <ffi.h>
#include <stdio.h>

int foo(int x) {
  return x + 3;
}

int
main(int argc, char *argv[])
{
  printf("foo(4)=%d\n", foo(4));

  ffi_cif cif;
  ffi_status status;
  
  ffi_type *arg_types[1];
  arg_types[0] = &ffi_type_sint;
  
  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI,
                        1, &ffi_type_sint, arg_types);
  if (status != FFI_OK) {
    fprintf(stderr, "ffi_prep_cif failed\n");
    return 1;
  }

  int r;
  void* args[1];
  int arg1 = 4;
  args[0] = &arg1;

  ffi_call(&cif, (void(*)())&foo, &r, args);
  
  printf("FFI foo(4)=%d\n", r);

  return 0;
}
