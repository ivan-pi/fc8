
ASM_FLAGS=

.PHONY: all clean

all: rocket.rom	\
	 jumping_X_and_O.rom \
	 Fishie.rom \
	 random_number_test.rom \
	 delay_timer_test.rom \
	 show_bcd.rom \
	 counter.rom

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

delay_timer_test.rom: delay_timer_test.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

show_bcd.rom: show_bcd.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

counter.rom: counter.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

clean:
	rm -vf *.rom