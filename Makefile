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

ifeq "$(strip $(DisableAsm))" "Yes"
EXTRA_CFLAGS := $(EXTRA_CFLAGS) -DLC_HAS_ASM_BACKEND=0
endif

HSBUILDDIR = $(DIST)/build
LCC = $(HSBUILDDIR)/lcc
CABAL ?= cabal

DEPDIR = $(DIST)/.deps
DEPDIRS = $(DEPDIR) $(DEPDIR)/rts

.PHONY: all
all: interp compiler/Opcodes.h $(LCC) lcc

.PHONY: boot
boot:
	mkdir -p $(HSBUILDDIR)
	mkdir -p $(DEPDIR)/rts
	mkdir -p $(DEPDIR)/rts/codegen
	mkdir -p $(DEPDIR)/utils
	touch mk/build.mk

INCLUDES = -Iincludes -Irts -Irts/codegen
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
       rts/InterpIR.c rts/Stats.c \
       rts/codegen/MCode.c rts/codegen/InterpAsm.c \
       rts/codegen/AsmCodeGen.c \
       rts/GC.c

UTILSRCS = utils/genopcodes.c

echo:
	@echo "SRCS = $(SRCS)"
#SRCS = rts/Loader.c rts/HashTable.c

interp: $(SRCS:.c=.o)
	@echo "LINK $^ => $@"
	@$(CC) -Wl,-no_pie -o $@ $^

lcc: $(LCC)
	ln -s $(LCC) $@

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
		compiler/lcc lcc
	rm -rf $(HSBUILDDIR)
	$(MAKE) -C tests clean

.PHONY: install-deps
install-deps:
	$(CABAL) install --only-dependencies
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

PRIM_MODULES_ghc-prim = GHC/Bool GHC/Types GHC/Ordering GHC/Tuple
PRIM_MODULES_integer-gmp = GHC/Integer/Type GHC/Integer
PRIM_MODULES_base = GHC/Base GHC/Classes GHC/Num GHC/List \
	Control/Exception/Base

PRIM_MODULES = \
	$(patsubst %,tests/ghc-prim/%.lcbc,$(PRIM_MODULES_ghc-prim)) \
	$(patsubst %,tests/integer-gmp/%.lcbc,$(PRIM_MODULES_integer-gmp)) \
	$(patsubst %,tests/base/%.lcbc,$(PRIM_MODULES_base))

.PHONY: check
TESTS ?= .
check: $(PRIM_MODULES)
	@ $(MAKE) -C tests check TESTS=$(TESTS) LITARGS=$(LITARGS)

.PHONY: bench
bench: $(PRIM_MODULES)
	$(MAKE) -C tests check TESTS=Bench LITARGS=$(LITARGS)

pr:
	@echo $(PRIM_MODULES)

clean-bytecode:
	rm -f $(PRIM_MODULES)
	$(MAKE) -C tests clean

-include $(SRCS:%.c=$(DEPDIR)/%.P)
-include $(UTILSRCS:%.c=$(DEPDIR)/%.P)

