
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

all: FC8

FC8: src/FC8_vm.F90 src/FC8.F90 src/randint8.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

src/randint8.o: src/randint8.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean
clean:
	rm -f *.mod *.o
	rm -f FC8

