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
#include "data.h"

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
int screen, font_height, have_font;
XFontStruct *font;

#endif

char *filename;
char *progname;

float min_lat, max_lat, min_lng, max_lng;

int color = 101, verbose = 0, max_x = MAX_X, max_y = MAX_Y;

/*
 * This really doesn't define anything anymore, it just gets necessary stuff
 * from the header of the data file.
 */

void
define_bounds(void)
{
  FILE *infile;
  Head header;

  if (NULL == (infile = fopen(filename, "rb")))
    {
      perror(filename);
      exit(1);
    }

  fread(&header, sizeof(header), 1, infile);

  max_lat = header.max_lat;
  min_lat = header.min_lat;
  max_lng = header.max_lng;
  min_lng = header.min_lng;

#ifdef DEBUG
  printf("%i points\nmin_lat=%f max_lat=%f\nmin_lng=%f max_lng=%f\n",
	 header.num_points, min_lat, max_lat, min_lng, max_lng);
#endif
  fclose(infile);
}

void
factor_bounds(long x, long y, float factor)
{
  float temp;
  float diff;

  /*
   * first,  move the viewport a little.
   */

  temp = ((float) x / (float) max_x);
  temp -= 0.5;
  diff = max_lat - min_lat;
  diff *= temp;
  max_lat += diff;
  min_lat += diff;

  temp = ((float) y / (float) max_y);
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
#ifdef VGACOMPILE
  long c;
  long x = max_x / 2;
  long y = max_y / 2;

#endif

#ifdef X11COMPILE
  XEvent event;

#endif

/*
 * It looks faster if I open the window before I define_bounds()
 */

  if (argc < 2)
    filename = DEFAULTBINFILENAME;
  else
    filename = argv[1];

  if ((access(filename, R_OK)) != 0)
    {
      fprintf(stderr, "Can't open file %s for reading.\n", filename);
      exit(1);
    }

#ifdef X11COMPILE
  init_window();
  puts("X11 conversion by Dustin Sallings.");
#endif

  define_bounds();

#ifdef VGACOMPILE
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
	case ResizeRequest:
	  max_x = event.xresizerequest.width;
	  max_y = event.xresizerequest.height;
	  XResizeWindow(display, window, max_x, max_y);
	  break;
	case MotionNotify:
          reportpos(event.xmotion.x, event.xmotion.y);
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

	  if (y < max_y - 1)
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

	  if (x < max_x - 1)
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

	  if (y < max_y - 1)
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

	  if (x < max_y - 1)
	    x += 3;
	  break;

	case 'o':		/*
				 * zoom out
				 */
	  factor_bounds(x, y, 2.0);
	  vga_plot();
	  x = max_x / 2;
	  y = max_y / 2;
	  break;

	case 'z':		/*
				 * zoom in
				 */
	  factor_bounds(x, y, 0.5);
	  vga_plot();
	  x = max_y / 2;
	  y = max_y / 2;
	  break;

	case 10:
	case 13:		/*
				 * move
				 */
	  factor_bounds(x, y, 1.0);
	  vga_plot();
	  x = max_x / 2;
	  y = max_y / 2;
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
