
FC=gfortran-10
CC=gcc-10

PREFIX = /usr/local

EGGX_PATH=$(PREFIX)/lib
EGGX_LIB=$(EGGX_PATH)/libeggx.a

EGGX2003_PATH=../eggx-procall-2003
EGGX2003_LIB=$(EGGX2003_PATH)/libeggx2003.a

FFLAGS=-Wall -fcheck=all
CFLAGS=-Wall

LDFLAGS=-I$(PREFIX)/include -L$(PREFIX)/lib
LDFLAGS+=-I$(EGGX2003_PATH)

LDLIBS=$(EGGX2003_LIB) $(EGGX_LIB) -lX11

.PHONY: all

all: chip8

chip8: chip8.F90 randint8.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

randint8.o: randint8.c
	$(CC) $(CFLAGS) -c -o $@ $<

op8xy5.o: op8xy5.c
	$(CC) $(CFLAGS) -c -o $@ $<


test_ggetch: test_ggetch.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)


xprog: xprog-2.cc
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< -lX11

xcb_main: xcb_main.c
	$(CC) -I$(PREFIX)/include -L$(PREFIX)/lib $(CFLAGS) -o $@ $< -lxcb

fcw_main: fcw_main.f90 fc8_win.o
	$(FC) $(FFLAGS) -L$(PREFIX)/lib -o $@ $^ -lxcb

fc8_win.o: fc8_win.c
	$(CC) -I$(PREFIX)/include $(CFLAGS) -c $<

.PHONY: clean
clean:
	rm -f *.mod *.o
	rm -f chip8

