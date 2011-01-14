.SUFFIXES:  # delete default rules

.PHONY: all
all: interp

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

# Building a C file automatically generates dependencies as a side effect
# The dependency file for `rts/Foo.c' lives at `.deps/rts/Foo.c'.
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


.PHONY: clean
clean:
	rm -f $(SRCS:%.c=%.o) utils/*.o

-include $(SRCS:%.c=$(DEPDIR)/%.P)
