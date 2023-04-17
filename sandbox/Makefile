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