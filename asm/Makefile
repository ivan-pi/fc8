
ASM_FLAGS=

.PHONY: all clean

%.rom : %.asm chip8.asm
	customasm -o $@ $(ASM_FLAGS) $<

all: rocket.rom	\
	 jumping_X_and_O.rom \
	 Fishie.rom \
	 random_number_test.rom \
	 delay_timer_test.rom \
	 show_bcd.rom \
	 counter.rom \
	 factorial.asm

clean:
	rm -vf *.rom
