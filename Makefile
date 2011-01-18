.SUFFIXES:  # delete default rules

-include mk/build.mk

DIST ?= dist

HSBUILDDIR = $(DIST)/build
LCC = $(HSBUILDDIR)/lcc

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
CFLAGS = -g

df = $(DEPDIR)/$(*D)/$(*F)

#SRCS := $(wildcard rts/*.c)
SRCS = rts/Bytecode.c rts/Capability.c rts/ClosureFlags.c \
       rts/FileUtils.c rts/HashTable.c rts/InterpThreaded.c \
       rts/Loader.c rts/MiscClosures.c rts/PrintClosure.c \
       rts/Thread.c rts/Main.c

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
%.o: %.c
	@echo "CC $(CFLAGS) $< => $@"
	@gcc -c $(INCLUDES) -MD -MF $(patsubst %.c,$(DEPDIR)/%.d,$<) $(CFLAGS) -o $@ $<
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

HSFLAGS = -package ghc -icompiler -hide-package mtl \
          -odir $(HSBUILDDIR) -hidir $(HSBUILDDIR)

$(HSDEPFILE):
	ghc -M $(HSFLAGS) compiler/Main.hs -dep-makefile $(HSDEPFILE)

# include $(HSDEPFILE)

%.hi: %.o
	@:

%.o: %.hs
	ghc -c $< $(HSFLAGS)

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
	ghc --make $(HSFLAGS)  compiler/Main.hs -o $@

.PHONY: clean
clean:
	rm -f $(SRCS:%.c=%.o) utils/*.o interp compiler/.depend \
		compiler/lcc
	rm -rf $(HSBUILDDIR)
# find compiler -name "*.hi" -delete

# Rules for building built-in packages

LCCFLAGS = --dump-bytecode

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
	cd tests && $(LCC) --dump-bytecode $(patsubst tests/%, %, $<)

PRIM_MODULES_ghc-prim = GHC/Bool GHC/Types
PRIM_MODULES_integer-gmp = GHC/Integer/Type GHC/Integer
PRIM_MODULES_base = GHC/Base GHC/Classes

PRIM_MODULES = \
	$(patsubst %,tests/ghc-prim/%.lcbc,$(PRIM_MODULES_ghc-prim)) \
	$(patsubst %,tests/integer-gmp/%.lcbc,$(PRIM_MODULES_integer-gmp)) \
	$(patsubst %,tests/base/%.lcbc,$(PRIM_MODULES_base))


test1: tests/Bc0005.lcbc $(PRIM_MODULES)
	./interp Bc0005 'Bc0005.test!closure'

test2: tests/Bc0006.lcbc $(PRIM_MODULES)
	./interp Bc0006

test3: tests/Bc0007.lcbc $(PRIM_MODULES)
	./interp Bc0007

pr:
	@echo $(PRIM_MODULES)

clean-bytecode:
	rm -f $(PRIM_MODULES)

-include $(SRCS:%.c=$(DEPDIR)/%.P)
-include $(UTILSRCS:%.c=$(DEPDIR)/%.P)

