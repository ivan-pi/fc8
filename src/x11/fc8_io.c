
#include "fc8_io.h"

#include <stdio.h>
#include <stdlib.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

// Implementation based upon this StackOverflow thread:
//   https://stackoverflow.com/questions/2100654/ignore-auto-repeat-in-x11-applications

static Display *dis;
static Window win;

static GC gc; /* graphics context */

static int MF = 10; // Magnification Factor

void fc8_display_open(const char *title, int n, const float *zoom) {

  dis = XOpenDisplay (NULL);
  //XAutoRepeatOff(dis);

  if (dis == NULL) {
    printf("Cannot connect to X server.\n");
    exit(1);
  }

  // Set magnification factor
  if (zoom) MF *= (int) *zoom/100.;

  win = XCreateSimpleWindow(dis, 
                            RootWindow(dis, 0), 
                            0, 0, 
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


void fc8_display_close(void) {
  XFreeGC(dis,gc);
  XDestroyWindow(dis,win);
  XCloseDisplay(dis);
}

void fc8_display_clear(void) 
{
  XClearWindow(dis,win);
  XFlush(dis);
}

#define BTEST(var,pos) ((var) & (1<<(pos)))

// Draw the CHIP-8 display stored in the form of a pixel-buffer
void fc8_display_draw(const int32_t screen[64]) 
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


//
// CHIP-8 KeyPad binding 
//
#define kp_0 XK_x
#define kp_1 XK_1
#define kp_2 XK_2
#if defined KEYBOARD_AZERTY
#define kp_3 XK_3
#define kp_4 XK_a
#define kp_5 XK_z
#define kp_6 XK_e
#define kp_7 XK_q
#define kp_8 XK_s
#define kp_9 XK_d
#define kp_A XK_w
#else
// Default layout is QWERTY
#define kp_0 XK_x
#define kp_1 XK_1
#define kp_2 XK_2
#define kp_3 XK_3
#define kp_4 XK_q
#define kp_5 XK_w
#define kp_6 XK_e
#define kp_7 XK_a
#define kp_8 XK_s
#define kp_9 XK_d
#if defined KEYBOARD_QWERTZ
#define kp_A XK_y
#else
#define kp_A XK_z
#endif
#endif
#define kp_B XK_c
#define kp_C XK_4
#define kp_D XK_r
#define kp_E XK_f
#define kp_F XK_v

//
// Other buttons
//
#define ESC_KEY XK_Escape
#define RELOAD  XK_0

static int findkey(KeySym xkey) {
    switch(xkey) {
      case kp_0: return 0x0; // X
      case kp_1: return 0x1; // 1
      case kp_2: return 0x2; // 2
      case kp_3: return 0x3; // 3
      case kp_4: return 0x4; // Q
      case kp_5: return 0x5; // W
      case kp_6: return 0x6; // E
      case kp_7: return 0x7; // A
      case kp_8: return 0x8; // S
      case kp_9: return 0x9; // D
      case kp_A: return 0xA; // Y
      case kp_B: return 0xB; // C
      case kp_C: return 0xC; // 4
      case kp_D: return 0xD; // R
      case kp_E: return 0xE; // F
      case kp_F: return 0xF; // V
      default: 
        return -1;
    }
}



// Check for valid CHIP-8 events
//
// TODO: Add expose event, in case of manual resize/maximize
void fc8_event_get(int *irep, int *xkey) {

  XEvent report;
  unsigned int keycode;
  int key;
  KeySym ks;

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

    ks = XLookupKeysym(&(report.xkey), 0);

    // Check if ESCAPE was pressed
    if (ks == ESC_KEY) {
      *irep = -2;
      return;
    } 
    if (ks == RELOAD) {
      *irep = -3;
      return;
    } 

    // Check CHIP-8 Keypad
    key = findkey(ks);
    if (key >= 0) {
      keypad.key[key] = true;
    }
    break;
  case KeyRelease:
    fprintf (stdout, "key #%d (%s) was released.\n", report.xkey.keycode, 
        XKeysymToString(XLookupKeysym(&(report.xkey), 0)));
    keycode = report.xkey.keycode;
    ks = XLookupKeysym(&(report.xkey), 0);

    // Check CHIP-8 Keypad
    key = findkey(ks);
    if (key >= 0) {
      keypad.key[key] = false;
    }
    break;
  default:
    key = -1;
  }

  *xkey = key;
}
