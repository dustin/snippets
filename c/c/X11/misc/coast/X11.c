#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "plot.h"

#define _setcolor(x) XSetForeground(display, gc, x)
#define _setpixel(x, y) XDrawPoint(display, window, gc, x, y)

extern Display *display;
extern Window window, rootwindow;
extern GC gc;
extern int screen;
extern int verbose;

extern char *filename;

extern float max_lat, min_lat, max_lng, min_lng;

XFontStruct *font;
int font_height, have_font;

void
init_window(void)
{
  XSetWindowAttributes attributes;
  unsigned long attr_mask, event_mask;
  XSizeHints sizehints;
  XClassHint class_hints;
  XWMHints wmhints;
  XGCValues xgcvalues;

/*
 * I went ahead and put the XOpenDisplay call in here so
 * the whole thing would be in one c file.
 */

  if (NULL == (display = XOpenDisplay(NULL)))
    {
      fprintf(stderr, "Cannot connect to X server: %s\n",
	      XDisplayName(NULL));

      exit(1);
    }

  screen = DefaultScreen(display);
  rootwindow = RootWindow(display, screen);

/*
 * This is the event mask, a flag that determines which events will be
 * processed by XNextEvent();
 *
 * Here I have told it I only want to process the button being pressed down
 * (there is a seperate one for released, but that won't be needed), and
 * exposure.  An exposure event occurs when a partially covered window is
 * uncovered, a window changes size, a window is created, the user switches
 * to a virtual desktop that has the window on it, and probably other things
 * I don't know about.  It's important to always process exposure events
 * unless you don't care.
 *
 * I'm using BlackPixel() and WhitePixel() to find the values for black and
 * white.  These don't necessarily return black and white in the colormap,
 * but they are supposed to return different colors.
 */

  event_mask = ButtonPressMask | ExposureMask;

  attributes.event_mask = event_mask;
  attributes.border_pixel = BlackPixel(display, screen);
  attributes.background_pixel = WhitePixel(display, screen);

/*
 * This is a mask for attributes that have been set.  This tells
 * XCreateWindow that I have set the event mask, the background color and
 * the foreground color.  You can also tell it things such as the user has
 * already picked the spot for the window and tell it where the window will
 * land.  This can be hard coded in if you want to lie.
 */

  attr_mask = CWEventMask | CWBackPixel | CWBorderPixel;

  window = XCreateWindow(display, rootwindow, 0, 0, MAX_X, MAX_Y, 2,
       CopyFromParent, InputOutput, CopyFromParent, attr_mask, &attributes);

/*
 * These are size hints.  I'm telling the window manager that I have a size
 * I would like my window to be.  The window manager can override these,
 * that's why they call the hints.
 */

  sizehints.height = MAX_Y;
  sizehints.width = MAX_X;
  sizehints.flags = USSize;

  XSetWMNormalHints(display, window, &sizehints);

/*
 * This puts that cool title at the top of the window.
 */

  XStoreName(display, window, "James Lemley's Cool Coast Plotter Thing.");

/*
 * Class hints are kind of neat.  It tells other programs what kind of
 * program this is.  I could have my programs check to see if any other
 * programs of the "CoolX11Ports" class are running.  res_name is usually
 * argv[0], but that's close enough.
 */

  class_hints.res_class = "CoolX11Ports";
  class_hints.res_name = "xPlot";
  XSetClassHint(display, window, &class_hints);

/*
 * These are window manager hints.  It too has flags to tell it which ones
 * I'm using.  In this case, I'm telling it that I do want it to take input
 * and I have set the initial state to be the normal state, which means the
 * window is visible.  Other states are WithdrawnState which means the
 * window is not visible and IconicState which means it starts iconic (if
 * supported).
 */

  wmhints.flags = InputHint | StateHint;
  wmhints.initial_state = NormalState;
  wmhints.input = True;
  XSetWMHints(display, window, &wmhints);

/*
 * This sets up the graphic context for the window.  I'm just telling it
 * that I want it to use white for the background and black for the
 * foreground, you can leave that out at first, but if you want to draw
 * anything into the window, you have to do it.
 */

  xgcvalues.foreground = BlackPixel(display, screen);
  xgcvalues.background = WhitePixel(display, screen);
  gc = XCreateGC(display, (Drawable) window, (GCForeground | GCBackground),
		 &xgcvalues);

/*
 * I'm also going to try to load a font while I'm here.  I'll try to load
 * the default font, but if that doesn't work, I'll try another one.  If
 * that doesn't work either, I'll just tell it that I don't have any fonts
 * loaded.
 */

  font = XLoadQueryFont(display, FONT);

  if (font == NULL)
    font = XLoadQueryFont(display, BACKFONT);
  if (font == NULL)
    have_font = False;

  if (have_font)
    font_height = font->ascent + font->descent;

/*
 * This makes the window visible on the screen.
 */

  XMapRaised(display, window);

/*
 * Well, actually, this probably does it, XMapRaised makes it visible, but
 * this flushes all of the X11 protocol data so everything can happen.
 */

  XFlush(display);

}

void
xcircle(int xc, int yc, int r)	/*
				 * x, y, and radius
				 */
{
  if (r < 1)
    r = 1;

/*
 * The last argument in XDrawArc is the number of 1/64 of a degree units to
 * draw.  So 360 * 64 * 1/64 = 360, a circle since x and y are the same r.
 */

  XDrawArc(display, window, gc, xc, yc, r, r, 0, (360 * 64));
}

void
xplot()
{
  long x, y;
  long oldx = 0;
  long oldy = 0;
  float lat, lng;
  char line[80];
  float lat_diff;
  float lng_diff;
  float temp;
  long island;

  FILE *infile;

  if (NULL == (infile = fopen(filename, "r")))
    exit(12);

  lat_diff = max_lat - min_lat;
  lng_diff = max_lng - min_lng;

  island = 1;

  _setcolor(BlackPixel(display, screen));
  XClearWindow(display, window);
  _setcolor(BlackPixel(display, screen));

  while (!feof(infile))
    {
      fgets(line, sizeof(line), infile);

      if (feof(infile))
	continue;

      lat = atof(line);
      lng = atof(line + 12);

      if (lat > -70.0)
	{
	  island = 1;
	  continue;
	}

      temp = max_lat - lat;
      temp = lat_diff - temp;
      x = (long) ((float) MAX_X * (temp / lat_diff));

      temp = max_lng - lng;
      y = (long) ((float) MAX_Y * (temp / lng_diff));

/*
 * _setpixel(x, y);
 */

      if (!island)
	{
	  if ((x >= 0 && x < MAX_X && y >= 0 && y < MAX_Y) ||
	      (oldx >= 0 && oldx < MAX_X && oldy >= 0 && oldy < MAX_Y))
	    XDrawLine(display, window, gc, oldx, oldy, x, y);
	}
      else
	island = 0;

      oldx = x;
      oldy = y;
    }
  fclose(infile);

/*
 * After it's all drawn, flush it to make sure it all gets displayed.
 * Looks faster on mine to have already drawn a bunch and then display it.
 */

  XFlush(display);
}

/*
 * Here is where the button event is processed.
 */

void
buttonevent(int x, int y, int button)
{
  switch (button)
    {

/*
 * Button one is for zoom in.
 */
    case 1:
      factor_bounds(x, y, 0.5);
      xplot();
      x = MAX_X / 2;
      y = MAX_Y / 2;
      break;

/*
 * Middle button quits, I think I'll make it ask first though.
 */
    case 2:
      XCloseDisplay(display);
      exit(0);

/*
 * Other button zooms out.
 */
    case 3:
      factor_bounds(x, y, 2.0);
      xplot();
      x = MAX_X / 2;
      y = MAX_Y / 2;
      break;
    }
}
