// Implementation based upon this StackOverflow thread:
//   https://stackoverflow.com/questions/2100654/ignore-auto-repeat-in-x11-applications

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#include <ISO_Fortran_binding.h>

static Display *dis;
static Window win;

static GC gc; /* graphics context */

static int MF = 8; // Magnification Factor

struct keypad {
  bool key[16];
};

extern struct keypad pad;

struct dxyn {
  int x, y;   // location
  int n;      // number of bytes (rows)
};

// Clear the CHIP-8 display
void fcw_clear_display(void) 
{
  XClearWindow(dis,win);
  XFlush(dis);
}

#define BTEST(var,pos) ((var) & (1<<(pos)))

// Draw the CHIP-8 display stored in the form of a pixel-buffer
void fcw_draw_display(const int32_t screen[64]) 
{
  XClearWindow(dis,win); // alternatively, XClearArea(..)
  for (int i = 0; i < 64; i++) {
    for (int j = 0; j < 32; j++) {
      const int k = j + (i < 32 ? 0 : 32);
      if (BTEST(screen[k], i % 32)) {
        XFillRectangle(dis, win, gc, i * MF, j * MF, MF, MF);
      }
    }
  }
  XFlush(dis);
}
#undef BTEST

// Draw the CHIP-8 display region described by the last draw command d

/*
void fcw_draw_sprite(const int32_t screen[64], const struct dxyn *d) 
{

  const int x = d->x * MF;
  const int y = d->y * MF;

  const unsigned int w = 8 * MF
  const unsigned int h = d->n * MF;

  XClearArea(dis,win,x,w,h,False); // alternatively, XClearArea(..)
  for (int i = 0; i < 8; i++) {
    for (int j = 0; j < d->n; j++) {
      const int zi = d->x + i;
      const int zj = d->y + j;
      const int k = zj + (zi < 32 ? 0 : 32);
      if (BTEST(screen[k], zi % 32)) {
        XFillRectangle(dis, win, gc, zi * MF, zj * MF, MF, MF);
      }
    }
  }
  XFlush(dis);
}
*/

// Open the CHIP-8 display
void fcw_open_display(const int *mf, const CFI_cdesc_t *title) {

  dis = XOpenDisplay (NULL);
  //XAutoRepeatOff(dis);

  if (dis == NULL) {
    printf("Cannot connect to X server.\n");
    exit(1);
  }

  // Set magnification factor
  if (mf) MF = *mf;

  win = XCreateSimpleWindow(dis, 
                            RootWindow(dis, 0), 
                            1, 1, 
                            64 * MF, 32 * MF,
                            0, 
                            BlackPixel(dis, 0), 
                            BlackPixel(dis, 0));
  
  XSetStandardProperties(dis,win,"CHIP-8 Interpeter","CHIP-8",None,NULL,0,NULL);

  // Perhaps the ExposureMask is also needed?
  XSelectInput (dis, win, KeyPressMask | KeyReleaseMask);

  unsigned long valuemask = GCForeground;
  XGCValues values;
  values.foreground = WhitePixel(dis,0);
  
  gc = XCreateGC(dis,win,valuemask,&values);

  //XSetBackground(dis,gc,BlackPixel(dis,0));

  /* change the foreground color of the GC to white */
  XSetForeground(dis, gc, WhitePixel(dis,0));

  /* change the fill style of this GC to 'solid'. */
  //XSetFillStyle(dis, gc, FillSolid);

  /* Display the window by mapping it to the display */
  XMapWindow(dis, win);
  XFlush(dis);

  printf("Open display done.\n");
}

// Close the CHIP-8 display
void fcw_close_display(void) {
  XFreeGC(dis,gc);
  XDestroyWindow(dis,win);
  XCloseDisplay(dis);
}

// Helper function to map from Xlib key codes to CHIP-8 key code.
// TODO: replace this with a switch
/*static inline int findkey(unsigned int xkey) {

  const unsigned int keymap[16] = 
    {15,26,27,28,20,21,22,8,9,10,14,16,29,23,11,17};

  for (int i = 0; i < 16; ++i) {
    if (xkey == keymap[i])
      return i;
  }
  return -1;
} */

#if defined(AZERTY)
// Version compatible with AZERTY keyboard
static inline int findkey(unsigned int xkey) {
    switch(xkey) {
    case 53: return 0x0; // X
    case 10: return 0x1; // 1, ampersand
    case 11: return 0x2; // 2, eacute
    case 12: return 0x3; // 3, quotedbl
    case 24: return 0x4; // A
    case 25: return 0x5; // Z
    case 26: return 0x6; // E
    case 38: return 0x7; // Q
    case 39: return 0x8; // S
    case 40: return 0x9; // D
    case 52: return 0xA; // W
    case 54: return 0xB; // C
    case 13: return 0xC; // 4, apostrophe
    case 27: return 0xD; // R
    case 41: return 0xE; // F
    case 55: return 0xF; // V
    default: 
        return -1;
    }
}
static const unsigned int ESC_KEY = 9;
static const unsigned int RELOAD  = 19;
#else
// Mapping the Xlib xkey keycodes of the QWERTY keyboard to CHIP-8 keys
static inline int findkey(unsigned int xkey) {
    switch(xkey) {
    case 15: return 0x0; // X
    case 26: return 0x1; // 1
    case 27: return 0x2; // 2
    case 28: return 0x3; // 3
    case 20: return 0x4; // Q
    case 21: return 0x5; // W
    case 22: return 0x6; // E
    case  8: return 0x7; // A
    case  9: return 0x8; // S
    case 10: return 0x9; // D
    case 14: return 0xA; // Y
    case 16: return 0xB; // C
    case 29: return 0xC; // 4
    case 23: return 0xD; // R
    case 11: return 0xE; // F
    case 17: return 0xF; // V
    default: 
        return -1;
    }
}
static const unsigned int ESC_KEY = 61;
static const unsigned int RELOAD  = 37;
#endif

// Check for valid CHIP-8 events
//
// TODO: Add expose event, in case of manual resize/maximize
void fcw_get_event(int *irep, int *xkey) {

  XEvent report;
  unsigned int keycode;
  int key;

//  XNextEvent (dis, &report); // blocking
  Bool e = XCheckWindowEvent(dis, win, 
                    KeyPressMask | KeyReleaseMask, 
                    &report);

  if (!e) {
    *irep = -1;
    return;
  }

  *irep = report.type;

  switch (report.type)
  {
  case KeyPress:

    fprintf (stdout, "key #%d (%s) was pressed.\n", report.xkey.keycode, 
        XKeysymToString(XLookupKeysym(&(report.xkey), 0)));
    keycode = report.xkey.keycode;

    // Check if ESCAPE was pressed
    if (keycode == ESC_KEY) {
      *irep = -2;
      return;
    } 
    if (keycode == RELOAD) {
      *irep = -3;
      return;
    } 

    // Check CHIP-8 Keypad
    key = findkey(keycode);
    if (key >= 0) {
      pad.key[key] = true;
    }
    break;
  case KeyRelease:
    fprintf (stdout, "key #%d (%s) was released.\n", report.xkey.keycode, 
        XKeysymToString(XLookupKeysym(&(report.xkey), 0)));
    keycode = report.xkey.keycode;

    // Check CHIP-8 Keypad
    key = findkey(keycode);
    if (key >= 0) {
      pad.key[key] = false;
    }
    break;
  default:
    key = -1;
  }

  *xkey = key;
}
