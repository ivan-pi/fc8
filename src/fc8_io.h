#ifndef FC8_IO_H
#define FC8_IO_H

#ifdef __cplusplus
#include <cstdint>
#else
#include <stdint.h>
#include <stdbool.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

void fc8_display_open(const char *title, int n, const int *mf);

void fc8_display_close(void);

void fc8_display_draw(const int32_t [64] /* screen */);

void fc8_display_clear(void);

void fc8_event_get(
	int * /* irep */, 
	int * /* xkey */);


struct fc8_keypad { 
	bool key[16];
};

// Defined in the Fortran `io` module
extern struct fc8_keypad keypad;

#ifdef __cplusplus
}
#endif

#endif // FC8_IO_H
