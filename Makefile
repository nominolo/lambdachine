-include mk/build.mk

# DIST ?= ./dist

CFLAGS_ALL ?= -Wall 
LFLAGS_ALL ?=
LL_ALL ?= 

CC ?= gcc
COMP = $(CC) $(CFLAGS_ALL) $(CFLAGS_TARGET) -o $@ -c $<
LINK = $(CC) $(LFLAGS_ALL) $(LFLAGS_TARGET) -o $@ $^ $(LL_TARGET) $(LL_ALL)

ifeq	"$(AsmInterpreter)" "Yes"
CFLAGS_ALL += -DLC_ASM_INTERP
endif

default: all
	@:

all: test
	@:

# $(DIST):
# 	mkdir -p $@

# VPATH = .:$(DIST)


%.o: %.c $(DIST)
	@echo Compiling
	$(COMP)

test_OBJS = reference-bci.o vm.o bc.o mm.o
test: $(test_OBJS)
	@echo Linking
	$(LINK)

run: test
	test

.PHONY: clean
clean:
	rm -rf $(test_OBJS) test
