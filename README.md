# FC8

Fortran CHIP-8 interpreter

## Graphics and keyboard input

* EggX/PROCALL: https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/

* SDL 1.2
* SDL 2.0
* SDL 3.0
* OpenGL

One of my aims in this project was to investigate design patterns. One of the 
typical design problems in game development (or graphical programs in general)
is how to support different graphical engines on different platforms.

The easy solution is to pick a graphics framework with widespread platform 
support. This way all of the platform specific issues are pushed down into the
graphical layer. 

Obviously, the requirements of a black and white, 2D game view are low compared
to more realistic programs, however the designs patterns still apply.

## Creating games

For a true experience you can write the games by hand in pseudo-assembly,
and compile them to hex manually. You can use a hex editor such as [hexyll]() or [hexedit]() to save and edit your CHIP-8 programs.

The `hexdump` tool can be used to quickly inspect a cartridge (watch out as different tools might display the byte-order differently).

## A few games

* Fishie: http://www.emulator101.com/introduction-to-chip-8.html
* IBM Logo:
* Jumping X and O:

## Other resources

* [Emulator 101: Introduction to CHIP-8](http://www.emulator101.com/introduction-to-chip-8.html)
* []()