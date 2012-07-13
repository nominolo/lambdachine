Missing Optimisations
---------------------

### Heap check hoisting

### Unnecessary evals

Consider the function:

     infList :: Int -> List Int
     infList n =
       let l1 = Cons n l2; l2 = Cons n l1 in l1

The generated code currently looks like this:
         
        alloc_frame 3
    0   r1 = <blackhole>
    1   r2 = &Cons_con_info
    2   r1 = alloc(r2, r0, r1)
    3   r0 = alloc(r2, r0, r1)
    4   Mem[r1 + 2] = r0
    5   eval r0 {r0}
    6   return r0
    
Note the `eval` at instruction 5.  We know that `r0` has constructor
`Cons` because we just allocated it, so the call to `eval` will return
immediately and can be removed.


Trace Selection
---------------

  - How much polymorphism is introduced by EVAL?

  - Should we specialise per *call*-site rather than per function?

  - Use traces at all, or use some hybrid scheme?


Benchmarks
----------

  - Are micro-benchmarks enough?

  - A compiler (or type checker) could be interesting -- lots of
    nested case expressions.  This is a good test whether trace sizes
    can become large.

Configuration
-------------

The following configure.in lines should allow detecting
the LEADING_UNDERSCORE feature.

    #######################
    ## test for underscores. The vpi module loader in vvm needs to know this
    ## in order to know the name of the start symbol for the .vpi module.
    #######################

    AC_MSG_CHECKING("for leading and/or trailing underscores")
    cat << EOF > underscore.c
           void underscore(void){}
    EOF
    $CC -c underscore.c > /dev/null 2>&1

    CC_LEADING_UNDERSCORE=no
    CC_TRAILING_UNDERSCORE=no

    output=`nm underscore.o|grep _underscore 2>&1`
    if test ! -z "$output"; then
      CC_LEADING_UNDERSCORE=yes
      AC_DEFINE(NEED_LU)
    fi

    output=`nm underscore.o|grep underscore_ 2>&1`
    if test ! -z "$output"; then
      CC_TRAILING_UNDERSCORE=yes
      AC_DEFINE(NEED_TU)
    fi

    if test "$CC_LEADING_UNDERSCORE" = yes; then
      AC_DEFINE(WLU)
    fi
    if test "$CC_TRAILING_UNDERSCORE" = yes; then
      AC_DEFINE(WTU)
    fi

    rm underscore.c underscore.o

    AC_MSG_RESULT("$CC_LEADING_UNDERSCORE $CC_TRAILING_UNDERSCORE")

    #######################
    ## end of test for underscores
    #######################

From https://github.com/freequaos/sourcenavigator/blob/cb99d9a17/snavigator/parsers/verilog/configure.in

