// Implementation based upon this StackOverflow thread:
//   https://stackoverflow.com/questions/2100654/ignore-auto-repeat-in-x11-applications

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

static Display *dis;
static Window win;
static XEvent report;

static GC gc; /* graphics context */

int main ()
{
  dis = XOpenDisplay (NULL);
  XAutoRepeatOn(dis);
  win = XCreateSimpleWindow (dis, RootWindow (dis, 0), 1, 1, 500, 500,
        0, BlackPixel (dis, 0), BlackPixel (dis, 0));
  XSelectInput (dis, win, KeyPressMask | KeyReleaseMask);
  XMapWindow (dis, win);
  XFlush (dis);

  while (1)
    {
      XNextEvent (dis, &report);
      switch (report.type)
      {
      case KeyPress:
        fprintf (stdout, "key #%d was pressed.\n", report.xkey.keycode);
       break;
      case KeyRelease:
        fprintf (stdout, "key #%d was released.\n", report.xkey.keycode);
        break;
      }
    }

  return (0);
}