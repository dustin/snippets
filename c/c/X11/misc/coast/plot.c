#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <math.h>

/*
 * I defined X11COMPILE to compile the X11 version so I could keep the old
 * svgalib version working while having an X11 interface optional at compile
 * time.
 */

#ifdef X11COMPILE
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#else
#include <vga.h>
#endif

#define MAX_X 640
#define MAX_Y 480
#define VGAMODE G640x480x256

#ifdef X11COMPILE
#define _setcolor(x) xsetcolor(x)
#define _setpixel(x, y) XDrawPoint(display, window, gc, x, y)
#define safe_vga_drawpixel(x, y) safe_x_drawpixel(x, y)
#else
#define _setcolor(x) vga_setcolor(x)
#define _setpixel(x, y) vga_drawpixel(x, y)
#define safe_vga_drawpixel(x, y) \
	if (x >= 0 && y >= 0 && x < MAX_X && y < MAX_Y) vga_drawpixel(x,y)
#endif

#ifdef X11COMPILE
Display *display;
Window rootwindow, window;
GC gc;
int screen;

#endif

void circle(int xc, int yc, int r);	/*

					 *
					 * *
					 * * * x, y, and radius
					 */

#ifdef X11COMPILE

/*
 * This is all of the X11 specific code. the first one is just kind of a
 * hack for the safe_draw_pixel thing.
 */

void
safe_x_drawpixel(int x, int y)
{
  XDrawPoint(display, window, gc, x, y);
}

/*
 * This is how you set a foreground color in X11, except you have to have
 * allocated the color you want to set first and call the function with the
 * color handle or something like that.  I've never done that, although
 * I've typed a program that did.  They said it was hard in the book, but it
 * didn't look to hard.
 */

void
xsetcolor(unsigned long color)
{
  XSetForeground(display, gc, color);
}

void
init_window(void)
{
  XSetWindowAttributes attributes;
  unsigned long attr_mask, event_mask;
  XSizeHints sizehints;
  XClassHint class_hints;
  XWMHints wmhints;
  XGCValues xgcvalues;

/*  I went ahead and put the XOpenDisplay call in here so
the whole thing would be in one c file.
*/

  if(NULL == (display = XOpenDisplay(NULL)) )
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
 * This makes the window visible on the screen.
 */

  XMapRaised(display, window);

/*
 * Well, actually, this probably does it, XMapRaised makes it visible, but
 * this flushes all of the X11 protocol data so everything can happen.
 */

  XFlush(display);

}

#endif

#ifndef X11COMPILE
void
setrange(int start, int end, int startred, int startgreen,
	 int startblue, int endred, int endgreen, int endblue)
{
  int i;			/*

				 *
				 * *
				 * * * set colors from start to end
				 */
  float ri, gi, bi;
  int range = end - start + 2;
  int j;

  ri = (float) (endred - startred);
  ri /= (float) range;
  gi = (float) (endgreen - startgreen);
  gi /= (float) range;
  bi = (float) (endblue - startblue);
  bi /= (float) range;
  j = 0;

  for (i = start; i <= end; i++)
    {
      vga_setpalette(i, startred + (int) (ri * (float) j),
		     startgreen + (int) (gi * (float) j),
		     startblue + (int) (bi * (float) j));
      j++;
    }
}

void
setup_palette()
{
  int i;

  setrange(1, 43, 63, 0, 0, 63, 31, 0);
  setrange(44, 86, 63, 31, 0, 63, 63, 0);
  setrange(87, 129, 63, 63, 0, 0, 63, 0);
  setrange(130, 172, 0, 63, 0, 0, 0, 63);
  setrange(173, 255, 0, 0, 63, 31, 0, 31);

}
#endif

void
circle(int xc, int yc, int r)	/*
				 * x, y, and radius
				 */
{
#ifdef X11COMPILE
  if (r < 1)
    r = 1;

/*
 * The last argument in XDrawArc is the number of 1/64 of a degree units to
 * draw.  So 360 * 64 * 1/64 = 360, a circle since x and y are the same r.
 */

  XDrawArc(display, window, gc, xc, yc, r, r, 0, (360 * 64));
#else

  register int x, y, p;

  x = 0;
  y = r;
  p = 3 - (r << 2);

  while (x < y)
    {
      safe_vga_drawpixel(xc + x, yc + y);
      safe_vga_drawpixel(xc - x, yc + y);
      safe_vga_drawpixel(xc + x, yc - y);
      safe_vga_drawpixel(xc - x, yc - y);
      safe_vga_drawpixel(xc + y, yc + x);
      safe_vga_drawpixel(xc - y, yc + x);
      safe_vga_drawpixel(xc + y, yc - x);
      safe_vga_drawpixel(xc - y, yc - x);

      if (p < 0)
	p += (x * 4 + 6);
      else
	p += (4 * (x - y--) + 10);
      x++;
    }

  if (x == y)
    {
      safe_vga_drawpixel(xc + x, yc + y);
      safe_vga_drawpixel(xc - x, yc + y);
      safe_vga_drawpixel(xc + x, yc - y);
      safe_vga_drawpixel(xc - x, yc - y);
    }
#endif
}

#ifndef X11COMPILE
void
rotate_palette(int howfar)	/*
				 * this will work in any 256-color app
				 */
{
  static char palvec[6120];
  static char *p;

  howfar %= 255;

  /*
   * get the current palette
   */
  vga_getpalvec(1, 255, (int *) palvec);

  /*
   * 3060 is the magic number
   */
  memcpy(palvec + 3060, palvec, 3060);

  if (howfar > 0)
    p = palvec + (12 * howfar);
  else
    p = palvec + 3060 + (12 * howfar);

  /*
   * set the new rotated palette
   */
  vga_setpalvec(1, 255, (int *) p);
}
#endif

#ifdef C700
void
init_vga()
{
  struct videoconfig vc;

  _getvideoconfig(&vc);

  switch (vc.adapter)
    {
    case _VGA:
    case _SVGA:
      _setvideomode(_VRES16COLOR);
      break;
    default:
      puts("Nope.\n");
      exit(0);
    }

  _getvideoconfig(&vc);
}
#endif

#ifdef LINUX
#ifndef X11COMPILE
void
init_vga()
{
  vga_init();			/*
				 * init vgalib graphics
				 */
  vga_setmode(VGAMODE);
  setup_palette();
}
#endif
#endif

void
bresline(int x1, int y1, int x2, int y2)
{
  static int dx, dy, x, y, xend, p, c1, c2, yfact;
  int swap = 0;

  dx = x1 - x2;
  dy = y1 - y2;

  if (dx < 0)
    dx *= -1;
  if (dy < 0)
    dy *= -1;

  if (dy > dx)
    {
      swap = dx;
      dx = dy;
      dy = swap;

      swap = x1;
      x1 = y1;
      y1 = swap;

      swap = x2;
      x2 = y2;
      y2 = swap;

      swap = 1;
    }

  p = (dy << 1) - dx;
  c1 = dy << 1;
  c2 = (dy - dx) << 1;

  if (x1 > x2)
    {
      x = x2;
      y = y2;
      xend = x1;
      if (y1 > y2)
	yfact = 1;
      else
	yfact = -1;

    }
  else
    {
      x = x1;
      y = y1;
      xend = x2;
      if (y1 > y2)
	yfact = -1;
      else
	yfact = 1;
    }

  if (swap)
    {
      safe_vga_drawpixel(y, x);
    }
  else
    {
      safe_vga_drawpixel(x, y);
    }

  while (x < xend)
    {
      x++;
      if (p < 0)
	p += c1;
      else
	{
	  y += yfact;
	  p += c2;
	}

      if (swap)
	{
	  safe_vga_drawpixel(y, x);
	}
      else
	{
	  safe_vga_drawpixel(x, y);
	}

    }
}

float min_lat;
float max_lat;

float min_lng;
float max_lng;
int color = 101;

void
define_bounds()
{
  FILE *infile;
  char line[80];

  float lat, lng;

  if (NULL == (infile = fopen("coast.dat", "r")))
    exit(12);

  min_lat = 9999999.0;
  max_lat = -999999.0;
  min_lng = 9999999.0;
  max_lng = -999999.0;

  while (!feof(infile))
    {
      fgets(line, sizeof(line), infile);

      if (feof(infile))
	continue;

      lat = atof(line);
      lng = atof(line + 12);

      if (lat > -70.0)
	continue;

      if (lat < min_lat)
	min_lat = lat;
      if (lat > max_lat)
	max_lat = lat;

      if (lng < min_lng)
	min_lng = lng;
      if (lng > max_lng)
	max_lng = lng;
    }
  fclose(infile);

  /*
   * fix aspect ratio
   */
  lat = max_lat - min_lat;
  /*
   * lat /= 2.0;
   */
  max_lat += lat;
  min_lat -= lat;

  printf("min lat: %f   max lat: %f \n", min_lat, max_lat);
  printf("min lng: %f  max lng: %f \n", min_lng, max_lng);
}

void
plot()
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

  if (NULL == (infile = fopen("coast.dat", "r")))
    exit(12);

  lat_diff = max_lat - min_lat;
  lng_diff = max_lng - min_lng;

  island = 1;

#ifdef X11COMPILE
  _setcolor(BlackPixel(display, screen));
  XClearWindow(display, window);
  _setcolor(BlackPixel(display, screen));
#else
  _setcolor(0);
  vga_clear();
  _setcolor(color);
#endif

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
#ifdef LINUX
#ifdef X11COMPILE
	  if ((x >= 0 && x < MAX_X && y >= 0 && y < MAX_Y) &&
	      (oldx >= 0 && oldx < MAX_X && oldy >= 0 && oldy < MAX_Y))
	    XDrawLine(display, window, gc, oldx, oldy, x, y);
#else
	  if ((x >= 0 && x < MAX_X && y >= 0 && y < MAX_Y) &&
	      (oldx >= 0 && oldx < MAX_X && oldy >= 0 && oldy < MAX_Y))
	    vga_drawline(oldx, oldy, x, y);
#endif
	  else if ((x >= 0 && x < MAX_X && y >= 0 && y < MAX_Y) ||
		   (oldx >= 0 && oldx < MAX_X && oldy >= 0 && oldy < MAX_Y))
	    bresline(x, y, oldx, oldy);
#else
	  _moveto(oldx, oldy);
	  _lineto(x, y);
#endif
	}
      else
	island = 0;

      oldx = x;
      oldy = y;
    }
  fclose(infile);
#ifdef X11COMPILE

/*
 * After it's all drawn, flush it to make sure it all gets displayed.
 * Looks faster on mine to have already drawn a bunch and then display it.
 */

  XFlush(display);
#endif
}

void
factor_bounds(long x, long y, float factor)
{
  float temp;
  float diff;

  /*
   * first,  move the viewport a little.
   */

  temp = ((float) x / (float) MAX_X);
  temp -= 0.5;
  diff = max_lat - min_lat;
  diff *= temp;
  max_lat += diff;
  min_lat += diff;

  temp = ((float) y / (float) MAX_Y);
  temp -= 0.5;
  diff = max_lng - min_lng;
  diff *= temp;
  max_lng -= diff;
  min_lng -= diff;

  /*
   * don't scale yet.
   */

  /*
   * now scale the viewport
   */
  diff = max_lat - min_lat;
  diff /= 2;
  temp = (min_lat + max_lat) / 2;
  diff *= factor;
  max_lat = temp + diff;
  min_lat = temp - diff;

  diff = max_lng - min_lng;
  diff /= 2;
  temp = (min_lng + max_lng) / 2;
  diff *= factor;
  max_lng = temp + diff;
  min_lng = temp - diff;
}

#ifdef X11COMPILE

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
      plot();
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
      plot();
      x = MAX_X / 2;
      y = MAX_Y / 2;
      break;
    }
}
#endif

int
main()
{
#ifndef X11COMPILE
  long c;

#endif
  long x = MAX_X / 2;
  long y = MAX_Y / 2;

#ifdef X11COMPILE
  XEvent event;

/*
 * It looks faster if I open the window before I define_bounds()
 */

  init_window();
  puts("X11 conversion by Dustin Sallings.");
#endif

  define_bounds();

#ifndef X11COMPILE
  init_vga();
  plot();
#endif

#ifdef X11COMPILE
/*
 * I endlessly run a while loop grabbing XEvents.
 */
  while (1)
    {
      XNextEvent(display, &event);
      switch (event.type)
	{
	  /*
	   * For an expose event, I just replot the whole thing.
	   */
	case Expose:
	  plot();
	  break;
	case ButtonPress:
	  buttonevent(event.xbutton.x, event.xbutton.y, event.xbutton.button);
	  break;
	}
    }
#else
  while ('q' != (c = vga_getch()))
    {
      if (c == 0)
	c = vga_getch();

      _setcolor(0);
      _setpixel(x, y);
      switch (c)
	{
	case 'c':
	  color++;
	  if (color == 256)
	    color = 1;
	  break;

	case 152:		/*
				 * up
				 */

	  if (y > 0)
	    y -= 30;
	  break;
	case 160:		/*
				 * down
				 */

	  if (y < MAX_Y - 1)
	    y += 30;
	  break;

	case 155:		/*
				 * left
				 */

	  if (x > 0)
	    x -= 30;
	  break;

	case 157:		/*
				 * right
				 */

	  if (x < MAX_X - 1)
	    x += 30;
	  break;

	case 'H':		/*
				 * up
				 */
	case 'A':

	  if (y > 0)
	    y -= 3;
	  break;
	case 'P':		/*
				 * down
				 */
	case 'B':

	  if (y < MAX_Y - 1)
	    y += 3;
	  break;

	case 'K':		/*
				 * left
				 */
	case 'D':

	  if (x > 0)
	    x -= 3;
	  break;

	case 'M':		/*
				 * right
				 */
	case 'C':

	  if (x < MAX_X - 1)
	    x += 3;
	  break;

	case 'o':		/*
				 * zoom out
				 */
	  factor_bounds(x, y, 2.0);
	  plot();
	  x = MAX_X / 2;
	  y = MAX_Y / 2;
	  break;

	case 'z':		/*
				 * zoom in
				 */
	  factor_bounds(x, y, 0.5);
	  plot();
	  x = MAX_X / 2;
	  y = MAX_Y / 2;
	  break;

	case 10:
	case 13:		/*
				 * move
				 */
	  factor_bounds(x, y, 1.0);
	  plot();
	  x = MAX_X / 2;
	  y = MAX_Y / 2;
	  break;

	}
    }
#endif

/*
 * What does this stuff do?  The X code never even goes here.
 */

#ifdef X11COMPILE
  _setcolor(BlackPixel(display, screen));
  XDrawPoint(display, window, gc, x, y);
  XCloseDisplay(display);
#else
  _setcolor(color);
  vga_drawpixel(x, y);
  vga_setmode(3);
  printf("Ok, here I am.  Color is %d.\n", color);
#endif

#ifndef LINUX
  _setvideomode(_DEFAULTMODE);
#endif
  exit(0);
}
