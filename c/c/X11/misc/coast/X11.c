#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "plot.h"
#include "data.h"

#define _setcolor(x) XSetForeground(display, gc, x)
#define _setpixel(x, y) XDrawPoint(display, window, gc, x, y)

extern Display *display;
extern Window window, rootwindow;
extern GC gc;
extern int screen, verbose, max_x, max_y;

extern char *filename;

extern float max_lat, min_lat, max_lng, min_lng;

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

  event_mask = ButtonPressMask | ExposureMask | KeyPressMask |
    PointerMotionMask;

  attributes.event_mask = event_mask;
  attributes.border_pixel = BlackPixel(display, screen);
  attributes.background_pixel = WhitePixel(display, screen);
  attributes.backing_store = WhenMapped;

/*
 * This is a mask for attributes that have been set.  This tells
 * XCreateWindow that I have set the event mask, the background color and
 * the foreground color.  You can also tell it things such as the user has
 * already picked the spot for the window and tell it where the window will
 * land.  This can be hard coded in if you want to lie.
 */

  attr_mask = CWEventMask | CWBackPixel | CWBorderPixel | CWBackingStore;

  window = XCreateWindow(display, rootwindow, 0, 0, max_x, max_y, 2,
       CopyFromParent, InputOutput, CopyFromParent, attr_mask, &attributes);

/*
 * These are size hints.  I'm telling the window manager that I have a size
 * I would like my window to be.  The window manager can override these,
 * that's why they call the hints.
 */

  sizehints.height = max_y;
  sizehints.width = max_x;
  sizehints.flags = USSize;

  XSetWMNormalHints(display, window, &sizehints);

/*
 * This puts that cool title at the top of the window.
 */

  XStoreName(display, window, "Coast Plotter Thing.");

/*
 * Class hints are kind of neat.  It tells other programs what kind of
 * program this is.  I could have my programs check to see if any other
 * programs of the "CoolX11Ports" class are running.  res_name is usually
 * argv[0], but that's close enough.
 */

  class_hints.res_class = "CoolX11Stuff";
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
reportpos(int x, int y)
{
  float lng, lat;
  char string[80];
  float lat_diff, lng_diff;

  lat_diff = max_lat - min_lat;
  lng_diff = max_lng - min_lng;

  lat = ((((float) x / max_x) * lat_diff) + max_lat - lat_diff);
  lng = (-(((float) y / max_y) * lng_diff) + max_lng);

  sprintf(string, "Latitude: %f", lat);
  if (have_font)
    XDrawImageString(display, window, gc, 7, font_height + 7,
		     string, strlen(string));
  else
    puts(string);

  sprintf(string, "Longitude: %f", lng);
  if (have_font)
    XDrawImageString(display, window, gc, 7, 2 * font_height + 7,
		     string, strlen(string));
  else
    puts(string);
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
  Head header;
  Point point;
  float temp;
  float lat_diff, lng_diff;
  long island;
  XWindowAttributes wattr;

  FILE *infile;

  if (NULL == (infile = fopen(filename, "rb")))
    exit(12);

/*
 * I'm going to read the header, may need it for the proper diffs,
 * at least need it out of the way.
 */

  fread(&header, sizeof(header), 1, infile);

/*
 * This finds the actual max_x and max_y values of the window before it
 * plots.
 */

  XGetWindowAttributes(display, window, &wattr);
  max_x = wattr.width;
  max_y = wattr.height;

/*
 * It doesn't seem to work the first time if I don't calculate the
 * differences right here, so I guess I'll do that, although it'll slow it
 * down ever so slightly.
 */

  lat_diff = max_lat - min_lat;
  lng_diff = max_lng - min_lng;

  island = 1;

  _setcolor(BlackPixel(display, screen));
  XClearWindow(display, window);
  _setcolor(BlackPixel(display, screen));

  while (!feof(infile))
    {

      fread(&point, sizeof(point), 1, infile);

      if (point.lat > -70.0)
	{
	  island = 1;
	  continue;
	}

      temp = max_lat - point.lat;
      temp = lat_diff - temp;
      x = (long) ((float) max_x * (temp / lat_diff));

      temp = max_lng - point.lng;
      y = (long) ((float) max_y * (temp / lng_diff));

/*
 * _setpixel(x, y);
 */

      if (!island)
	{
	  if ((x >= 0 && x < max_x && y >= 0 && y < max_y) ||
	      (oldx >= 0 && oldx < max_x && oldy >= 0 && oldy < max_y))
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
 * This is where I process button presses.
 */

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
    case '\r':
    case ' ':
      factor_bounds(x, y, 1.0);
      xplot();
      break;
    case 'q':
      XCloseDisplay(display);
      exit(0);
      break;
    }
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
      x = max_x / 2;
      y = max_y / 2;
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
      x = max_x / 2;
      y = max_y / 2;
      break;
    }
}
