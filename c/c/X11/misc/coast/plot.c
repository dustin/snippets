#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <math.h>

#ifdef X11COMPILE
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif

#ifdef VGACOMPILE
#include "vga.h"
#endif

#include "plot.h"

/*
 * I defined X11COMPILE to compile the X11 version so I could keep the old
 * svgalib version working while having an X11 interface optional at compile
 * time.
 */

#ifdef X11COMPILE

#define _setcolor(x) XSetForeground(display, gc, x)
Display *display;
Window rootwindow, window;
GC gc;
int screen;

#endif

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

int
main(int argc, char *argv[])
{
#ifndef X11COMPILE
  long c;

#else
  long x = MAX_X / 2;
  long y = MAX_Y / 2;

#endif

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
  vga_plot();
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
	  xplot();
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
	  vga_plot();
	  x = MAX_X / 2;
	  y = MAX_Y / 2;
	  break;

	case 'z':		/*
				 * zoom in
				 */
	  factor_bounds(x, y, 0.5);
	  vga_plot();
	  x = MAX_X / 2;
	  y = MAX_Y / 2;
	  break;

	case 10:
	case 13:		/*
				 * move
				 */
	  factor_bounds(x, y, 1.0);
	  vga_plot();
	  x = MAX_X / 2;
	  y = MAX_Y / 2;
	  break;

	}

      _setcolor(color);
      vga_drawpixel(x, y);
    }
#endif

#ifdef VGACOMPILE
  vga_setmode(3);
  printf("Ok, here I am.  Color is %d.\n", color);
#endif

#ifndef LINUX
  _setvideomode(_DEFAULTMODE);
#endif
  exit(0);
}
