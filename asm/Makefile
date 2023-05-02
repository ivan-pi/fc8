
ASM_FLAGS=

all: rocket.rom	\
	 jumping_X_and_O.rom \
	 Fishie.rom \
	 random_number_test.rom

rocket.rom: rocket.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

jumping_X_and_O.rom: jumping_X_and_O.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

Fishie.rom: Fishie.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

mytest.rom: mytest.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

random_number_test.rom: random_number_test.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

.phony: clean
clean:
	rm *.rom