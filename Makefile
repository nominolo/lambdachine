.SUFFIXES:  # delete default rules

# Put local customisations into `mk/build.mk`.
-include mk/build.mk

# DIST must be an absolute directory
ifeq ($(DIST),)
DIST := $(shell pwd)/dist
endif

HC ?= ghc
CC ?= gcc

ifeq "$(strip $(PerformanceBuild))" "Yes"
EXTRA_CFLAGS := $(EXTRA_CFLAGS) -DNDEBUG
endif

ifeq "$(strip $(DisableJit))" "Yes"
EXTRA_CFLAGS := $(EXTRA_CFLAGS) -DLC_HAS_JIT=0
endif

ifneq ($(DebugLevel),)
EXTRA_CFLAGS := $(EXTRA_CFLAGS) -DLC_DEBUG_LEVEL=$(DebugLevel)
endif

HSBUILDDIR = $(DIST)/build
LCC = $(HSBUILDDIR)/lcc
CABAL ?= cabal

DEPDIR = $(DIST)/.deps
DEPDIRS = $(DEPDIR) $(DEPDIR)/rts

.PHONY: all
all: interp compiler/Opcodes.h $(LCC)

.PHONY: boot
boot:
	mkdir -p $(HSBUILDDIR)
	mkdir -p $(DEPDIR)/rts
	mkdir -p $(DEPDIR)/utils

INCLUDES = -Iincludes -Irts
CFLAGS = -Wall -g $(EXTRA_CFLAGS)

df = $(DEPDIR)/$(*D)/$(*F)

#SRCS := $(wildcard rts/*.c)
SRCS = rts/Bytecode.c rts/Capability.c rts/ClosureFlags.c \
       rts/FileUtils.c rts/HashTable.c rts/InterpThreaded.c \
       rts/Loader.c rts/MiscClosures.c rts/PrintClosure.c \
       rts/Thread.c rts/StorageManager.c \
       rts/Main.c \
       rts/Record.c rts/PrintIR.c rts/OptimiseIR.c \
       rts/Snapshot.c rts/HeapInfo.c rts/Bitset.c \
       rts/InterpIR.c rts/Stats.c

UTILSRCS = utils/genopcodes.c

echo:
	@echo "SRCS = $(SRCS)"
#SRCS = rts/Loader.c rts/HashTable.c

interp: $(SRCS:.c=.o)
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

# Building a C file automatically generates dependencies as a side
# effect.  This only works with `gcc'.
#
# The dependency file for `rts/Foo.c' lives at `.deps/rts/Foo.c'.
#
%.o: %.c mk/build.mk
	@echo "CC $(CFLAGS) $< => $@"
	@$(CC) -c $(INCLUDES) -MD -MF $(patsubst %.c,$(DEPDIR)/%.d,$<) $(CFLAGS) -o $@ $<
	@cp $(df).d $(df).P; \
	    sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
	        -e '/^$$/ d' -e 's/$$/ :/' < $(df).d >> $(df).P; \
	rm -f $(df).d


utils/genopcodes: utils/genopcodes.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

utils/print_config: utils/print_config.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

compiler/Opcodes.h: utils/genopcodes
	./$< > $@

HSDEPFILE = compiler/.depend

HSFLAGS = -hide-all-packages \
          -package ghc -package base -package filepath -package process -package directory -package containers \
          -package ghc-paths -package cmdargs -package mtl -package blaze-builder -package vector \
          -package utf8-string -package bytestring -package array -package ansi-wl-pprint -package binary \
          -package uniplate -package hoopl -package value-supply \
          -package graph-serialize -package temporary \
          -icompiler \
          -odir $(HSBUILDDIR) -hidir $(HSBUILDDIR)

$(HSDEPFILE):
	$(HC) -M $(HSFLAGS) compiler/Main.hs -dep-makefile $(HSDEPFILE)

# include $(HSDEPFILE)

%.hi: %.o
	@:

%.o: %.hs
	$(HC) -c $< $(HSFLAGS)

HSSRCS := $(shell find compiler -name '*.hs')

# FIXME: We let the compiler depend on the source files not the .o
# files.  This actually doesn't always work.  Fortunately,
#
#    make clean && make boot && make
#
# is pretty quick.

# .PHONY:
$(LCC): $(HSSRCS) compiler/Opcodes.h
	@mkdir -p $(HSBUILDDIR)
	$(HC) --make $(HSFLAGS)  compiler/Main.hs -o $@

.PHONY: clean-interp
clean-interp:
	rm -f $(SRCS:%.c=%.o) utils/*.o interp

.PHONY: clean
clean:
	rm -f $(SRCS:%.c=%.o) utils/*.o interp compiler/.depend \
		compiler/lcc
	rm -rf $(HSBUILDDIR)

.PHONY: install-deps
install-deps:
	$(CABAL) install 'ghc-paths ==0.1.*' 'cmdargs == 0.6.*' \
	 'mtl == 2.*' 'blaze-builder ==0.3.*' 'vector == 0.7.*' \
	 'utf8-string == 0.3.*' 'ansi-wl-pprint == 0.6.*' \
         'binary == 0.5.*' 'uniplate == 1.6.*' 'hoopl >= 3.8.6.1' \
	 'value-supply == 0.6.*' 'graph-serialize >= 0.1.5' \
	 'temporary == 1.1.*'

# find compiler -name "*.hi" -delete

# Rules for building built-in packages

LCCFLAGS = --dump-bytecode --dump-core-binds

tests/ghc-prim/%.lcbc: tests/ghc-prim/%.hs
	cd tests/ghc-prim && \
	$(LCC) $(LCCFLAGS) --package-name=ghc-prim $(patsubst tests/ghc-prim/%, %, $<)

tests/integer-gmp/%.lcbc: tests/integer-gmp/%.hs
	cd tests/integer-gmp && \
	$(LCC) $(LCCFLAGS) --package-name=integer-gmp $(patsubst tests/integer-gmp/%, %, $<)

tests/base/%.lcbc: tests/base/%.hs
	cd tests/base && \
	$(LCC) $(LCCFLAGS) --package-name=base $(patsubst tests/base/%, %, $<)
#	@echo "@ = $@, < = $<"

tests/%.lcbc: tests/%.hs
	cd tests && $(LCC) $(LCCFLAGS) $(patsubst tests/%, %, $<)

PRIM_MODULES_ghc-prim = GHC/Bool GHC/Types GHC/Ordering GHC/Tuple
PRIM_MODULES_integer-gmp = GHC/Integer/Type GHC/Integer
PRIM_MODULES_base = GHC/Base GHC/Classes GHC/Num GHC/List \
	Control/Exception/Base

PRIM_MODULES = \
	$(patsubst %,tests/ghc-prim/%.lcbc,$(PRIM_MODULES_ghc-prim)) \
	$(patsubst %,tests/integer-gmp/%.lcbc,$(PRIM_MODULES_integer-gmp)) \
	$(patsubst %,tests/base/%.lcbc,$(PRIM_MODULES_base))


test1: tests/Bc0005.lcbc $(PRIM_MODULES)
	./interp --print-loader-state Bc0005

test2: tests/Bc0006.lcbc $(PRIM_MODULES)
	./interp Bc0006

test3: tests/Bc0007.lcbc $(PRIM_MODULES)
	./interp Bc0007

test4: tests/PreludeTests.lcbc $(PRIM_MODULES)
	./interp PreludeTests

test5: tests/Bc0008.lcbc $(PRIM_MODULES)
	./interp Bc0008

test6: tests/Toys/Ackermann.lcbc $(PRIM_MODULES)
	./interp Toys.Ackermann

test7: tests/Bc0009.lcbc  $(PRIM_MODULES)
	./interp --print-loader-state Bc0009

test10: tests/Bc0010.lcbc  $(PRIM_MODULES)
	./interp Bc0010

test11: tests/Bc0011.lcbc  $(PRIM_MODULES)
	./interp --print-loader-state Bc0011

test12: tests/Bc0012.lcbc  $(PRIM_MODULES)
	./interp --print-loader-state Bc0012

test13: tests/Bc0013.lcbc $(PRIM_MODULES)
	./interp Bc0013

#
# To run benchmark without the JIT enabled:
#
# $ make bench<n> NOJIT=1
#

ifeq ($(NOJIT),)
BENCH_FLAGS ?=
else
BENCH_FLAGS = --no-jit
endif

bench1: tests/Bench/Append.lcbc $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.Append

bench2: tests/Bench/SumFromTo1.lcbc $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.SumFromTo1

bench2a: tests/Bench/SumFromTo2.lcbc $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.SumFromTo2

bench2b: tests/Bench/SumFromTo3.lcbc $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.SumFromTo3

bench2c: tests/Bench/SumFromTo4.lcbc $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.SumFromTo4

bench3: tests/Bench/Tak.lcbc  $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.Tak

bench4: tests/Bench/Primes.lcbc $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.Primes

bench5: tests/Bench/SumSquare1.lcbc $(PRIM_MODULES)
	./interp $(BENCH_FLAGS) Bench.SumSquare1

pr:
	@echo $(PRIM_MODULES)

clean-bytecode:
	rm -f $(PRIM_MODULES) tests/*.lcbc tests/Toys/*.lcbc \
           tests/Bench/*.lcbc

-include $(SRCS:%.c=$(DEPDIR)/%.P)
-include $(UTILSRCS:%.c=$(DEPDIR)/%.P)

