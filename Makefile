.SUFFIXES:  # delete default rules

.PHONY: all
all: interp compiler/Opcodes.h

DEPDIR = .deps
DEPDIRS = $(DEPDIR) $(DEPDIR)/rts

INCLUDES = -Iincludes -Irts
CFLAGS = -g

df = $(DEPDIR)/$(*D)/$(*F)

#SRCS := $(wildcard rts/*.c)
SRCS = rts/Bytecode.c rts/Capability.c rts/ClosureFlags.c \
       rts/FileUtils.c rts/HashTable.c rts/InterpThreaded.c \
       rts/Loader.c rts/MiscClosures.c rts/PrintClosure.c \
       rts/Thread.c

UTILSRCS = utils/genopcodes.c

echo:
	@echo "SRCS = $(SRCS)"
#SRCS = rts/Loader.c rts/HashTable.c

interp: $(SRCS:.c=.o)
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

$(DEPDIR):
	mkdir $@
$(DEPDIR)/rts:
	mkdir $@

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

HSFLAGS = -package ghc -icompiler -hide-package mtl -odir build

$(HSDEPFILE):
	ghc -M $(HSFLAGS) compiler/Main.hs -dep-makefile $(HSDEPFILE)

include $(HSDEPFILE)

%.hi: %.o
	@:

%.o: %.hs
	ghc -c $< $(HSFLAGS)

.PHONY: compiler/lc
compiler/lc:
	ghc --make $(HSFLAGS)  compiler/Main.hs -o $@

.PHONY: clean
clean:
	rm -f $(SRCS:%.c=%.o) utils/*.o interp compiler/.depend
	rm -rf build

test:
	./compiler/lc --dump-bytecode tests/Bc0005.hs

-include $(SRCS:%.c=$(DEPDIR)/%.P)
-include $(UTILSRCS:%.c=$(DEPDIR)/%.P)

