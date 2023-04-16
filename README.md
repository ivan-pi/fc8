# FC8

Fortran CHIP-8 interpreter

## Build instructions

Clone the repository and build using CMake:

```txt
$ git clone
$ mkdir build && cd build
$ cmake ..
```

### Installing EGGX/ProCALL

Currently, FC8 depends on the [EGGX/ProCALL](https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/) library. This can be obtained using the following steps:

```txt
$ wget https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/eggx-0.95.tar.gz
$ # or, alternatively
$ curl -o eggx-0.95.tar.gz https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/eggx-0.95.tar.gz
$ tar -xf eggx-0.95.tar.gz && cd eggx-0.95
$ make
$ make install
```
(By default, the EGGX Makefile will try to install the library to `/usr/local/lib`, where super user rights might be needed.)

To install EGGX to a custom directory use:
```
$ make install PREFIX=<custom directory>
```

If EGGX was installed at a custom location, you should communicate this to CMake at config time:
```
$ cmake .. -DEGGX_DIR=<custom directory>
```

## Examples

A few example ROMs can be bound in the `cartridges/` directory. ROMS/cartridges can be specified at the command line:
```
$ ./fc8 <cartridge>
```

## Tests

To test the interpreter, I have used the ROM images found in the following third-party repositories (in this order of helpfulness):
- https://github.com/Timendus/chip8-test-suite
- https://github.com/corax89/chip8-test-rom
- https://github.com/mattmikolay/chip-8
- https://github.com/Skosulor/c8int

It's often insightful to compare results with interpreters of others. Here are just a few:
- https://bluishcoder.co.nz/js8080/
- https://chip8.iorel.nl/

(Note that many interpreters found on the internet are incomplete or misinterpret certain opcodes. In some cases, also the ROMs are broken or rely on implementation quirks.)

Larger collection of games and other demos can be found in:
- https://github.com/dmatlack/chip8
- https://johnearnest.github.io/chip8Archive/
- http://pong-story.com/chip8/

## Graphics and keyboard input

* EggX/PROCALL: https://www.ir.isas.jaxa.jp/~cyamauch/eggx_procall/

* SDL 1.2
* SDL 2.0
* SDL 3.0
* OpenGL/GLUT: https://www.opengl.org/resources/libraries/glut/spec3/spec3.html

One of my aims in this project was to investigate design patterns. One of the 
typical design problems in game development (or graphical programs in general)
is how to support different graphical engines on different platforms.

The easy solution is to pick a graphics framework with widespread platform 
support. This way all of the platform specific issues are pushed down into the
graphical layer. 

Obviously, the requirements of a black and white, 2D game view are low compared
to more realistic programs, however the designs patterns still apply.

## Writing and editing ROMs manually

For a true experience you can write the games by hand in pseudo-assembly,
and compile them to hex manually. You can use a hex editor such as [hexyll]() or [hexedit]() to save and edit your CHIP-8 programs. 

The `hexdump` tool can be used to quickly inspect a cartridge (watch out as different tools might display the byte-order differently). Alternatively, [HexEd.it](https://hexed.it/) client-side JavaScript based hex editor is a more powerful tool which runs in the browser.

## CHIP8 Assemblers

A far better developer experience is to use a high-level assembler. The most famous one today is [Octo](http://johnearnest.github.io/Octo/), which also includes a disassembler and emulator, as well as a sprite editor. A second alternative is [Dorito](https://github.com/lesharris/dorito).

## Other resources

* [Emulator 101: Introduction to CHIP-8](http://www.emulator101.com/introduction-to-chip-8.html)