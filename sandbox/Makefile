
FC=gfortran-10
CC=gcc-10

FFLAGS=
CFLAGS=

LDFLAGS=
LDLIBS=

X11_INC = /usr/X11/include
XCB_INC = /usr/X11/include

X11_LIB = /usr/X11/lib

all: fcw_main_Xlib

test_ggetch: test_ggetch.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

xprog: xprog-2.cc
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< -lX11

xcb_main: xcb_main.c
	$(CC) -I$(PREFIX)/include -L$(PREFIX)/lib $(CFLAGS) -o $@ $< -lxcb

xlib_example: xlib_example.c
	$(CC) -I$(X11_INC) -L$(X11_LIB) $(CFLAGS) -o $@ $< -lX11

fcw_main_Xlib: fcw_main.f90 fcw_Xlib.o
	$(FC) $(FFLAGS) -L$(X11_LIB) -o $@ $^ -lX11

fcw_main_xcb: fcw_main.f90 fc8_win.o
	$(FC) $(FFLAGS) -L$(X11_LIB) -o $@ $^ -lxcb

fcw_Xlib.o: fcw_Xlib.c
	$(CC) -I$(X11_INC) $(CFLAGS) -c $<

fc8_win.o: fc8_win.c
	$(CC) -I$(PREFIX) $(CFLAGS) -c $<


glut_example: glut_example.c blocks_vert.c
	$(CC) $(CFLAGS) -DGL_SILENCE_DEPRECATION -framework OpenGL -framework GLUT -o $@ $^

.phony: clean
clean:
	rm -f *.o *.mod glut_example xlib_example fcw_main_Xlib fcw_main_xcb