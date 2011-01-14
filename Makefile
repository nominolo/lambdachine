all: print_config

INCLUDES = -Iincludes -Irts
CFLAGS = $(INCLUDES) -O3 -g

DIST = $(HOME)/tmp/dist-devel/lambdachine

print_config: print_config.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

interp: rts/InterpThreaded.o rts/ClosureFlags.o rts/MiscClosures.o rts/Capability.o rts/Thread.o rts/PrintClosure.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

testloader: rts/Loader.o rts/HashTable.o rts/FileUtils.o rts/PrintClosure.o rts/Bytecode.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

utils/genopcodes.o: includes/Bytecode.h
utils/genopcodes: utils/genopcodes.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^
compiler/Opcodes.h: utils/genopcodes
	./$< > $@

rts/Bytecode.o: includes/Bytecode.h

# def.h: arch.h
# vm.h: def.h bc.h
gen_offs.o: vm.h
# dispatch.o: asm_offsets.incl
bci.o: opcodes.h asm_offsets.incl
vm.o: vm.h bc.h def.h
print_config.o: includes/Arch.h includes/Common.h
rts/InterpThreaded.o: includes/Arch.h includes/Common.h includes/Bytecode.h includes/Opcodes.h \
	includes/Capability.h
rts/Thread.o: includes/Thread.h includes/Bytecode.h includes/Common.h
rts/MiscClosures.o: includes/InfoTables.h includes/Common.h includes/Bytecode.h includes/MiscClosures.h
rts/Loader.o: includes/Loader.h rts/HashTable.h includes/InfoTables.h includes/FileUtils.h

rts/FileUtils.o: includes/FileUtils.h

# %.o: %.c
# 	@echo "CC $< => $@"
# 	@$(CC) -m32 -Wall -c $< -o $@

# %.o: %.s
# 	@echo "ASM $< => $@"
# 	@$(CC) -m32 -c $< -o $@

# bci.o: bci.S
# 	@echo "ASM $< => $@"
# 	@$(CC) -m64 -c $< -o $@

# test: main.o dispatch.o
# 	@echo "LINK $^ => $@"
# 	@$(CC) -m32 -o $@ $^

gen_offs: gen_offs.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^

asm_offsets.incl: gen_offs
	@echo "GEN $@"
	@./gen_offs

%.o: %.S
	@echo "ASM $< => $@"
	@$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	@echo "CC $< => $@"
	@$(CC) $(CFLAGS) -Wall -c $< -o $@

test: vm.o bci.o bc.o
	@echo "LINK $^ => $@"
	@$(CC) -o $@ $^


.PHONY: clean
clean:
	rm -f *.o rts/*.o test gen_offs asm_offsets.incl
