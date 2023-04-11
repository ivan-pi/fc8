
ASM_FLAGS=

all: rocket.rom	\
	 jumping_X_and_O.rom \
	 Fishie.rom

rocket.rom: rocket.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

jumping_X_and_O.rom: jumping_X_and_O.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

Fishie.rom: Fishie.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

.phony: clean
clean:
	rm *.rom