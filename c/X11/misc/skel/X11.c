#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#ifndef SIMPLEDEFINES

#define SIMPLEDEFINES
#define WIN_TITLE "New Program"
#define PROGGYCLASS "NewProg"
#define PROGGYNAME "NewProg"
#define WIN_WIDTH 320
#define WIN_HEIGHT 200
#define FONT "variable"
#define BACKFONT "fixed"
#endif


extern Display *display;
extern Window window, rootwindow;
extern GC gc;
extern int screen, verbose;

extern XFontStruct *font;
extern int font_height, have_font;

void
init_window(void)
{
  XSetWindowAttributes attributes;
  unsigned long attr_mask, event_mask;
  XSizeHints sizehints;
  XClassHint class_hints;
  XWMHints wmhints;
  XGCValues xgcvalues;

  if (NULL == (display = XOpenDisplay(NULL)))
    {
      fprintf(stderr, "Cannot connect to X server: %s\n",
	      XDisplayName(NULL));

      exit(1);
    }

  screen = DefaultScreen(display);
  rootwindow = RootWindow(display, screen);

  event_mask = ButtonPressMask | ExposureMask | KeyPressMask |
    PointerMotionMask;

  attributes.event_mask = event_mask;
  attributes.border_pixel = BlackPixel(display, screen);
  attributes.background_pixel = WhitePixel(display, screen);

  attr_mask = CWEventMask | CWBackPixel | CWBorderPixel;

  window = XCreateWindow(display, rootwindow, 0, 0, WIN_WIDTH, WIN_HEIGHT, 2,
       CopyFromParent, InputOutput, CopyFromParent, attr_mask, &attributes);

  sizehints.height = WIN_HEIGHT;
  sizehints.width = WIN_WIDTH;
  sizehints.flags = USSize;

  XSetWMNormalHints(display, window, &sizehints);

  XStoreName(display, window, WIN_TITLE);

  class_hints.res_class = PROGGYCLASS;
  class_hints.res_name = PROGGYNAME;
  XSetClassHint(display, window, &class_hints);

  wmhints.flags = InputHint | StateHint;
  wmhints.initial_state = NormalState;
  wmhints.input = True;
  XSetWMHints(display, window, &wmhints);

  xgcvalues.foreground = BlackPixel(display, screen);
  xgcvalues.background = WhitePixel(display, screen);
  gc = XCreateGC(display, (Drawable) window, (GCForeground | GCBackground),
		 &xgcvalues);

  font = XLoadQueryFont(display, FONT);

  have_font = True;

  if (font == NULL)
    font = XLoadQueryFont(display, BACKFONT);
  if (font == NULL)
    have_font = False;

  if (have_font)
    {
      font_height = font->ascent + font->descent;
#ifdef DEBUG
      printf("font height: %d\n", font_height);
#endif
      XSetFont(display, gc, font->fid);
    }

  XMapRaised(display, window);

  XFlush(display);

}

void
keyevent(XKeyEvent * event)
{
  KeySym keysym;
  XComposeStatus cs;
  int x, y;
  char whatkey[70];

  keysym = 0x0;
  whatkey[0] = '\0';
  x = event->x, y = event->y;
  XLookupString(event, whatkey, 1, &keysym, &cs);
#ifdef DEBUG
  printf("%d (%c) x:%d y:%d\n", whatkey[0], whatkey[0], x, y);
#endif
  switch (whatkey[0])
    {
    case 'q':
      XCloseDisplay(display);
      exit(0);
      break;
    }
}

void
buttonevent(int x, int y, int button)
{
  switch (button)
    {

    case 1:
      break;

    case 2:
      exit(0);

    case 3:
      break;
    }
}
