#include <stdio.h>
#include <stdlib.h>

#include <vga.h>

#ifndef VGACOMPILE
#define VGACOMPILE
#endif

#include "plot.h"

#define safe_vga_drawpixel(x, y) \
	if (x >= 0 && y >= 0 && x < MAX_X && y < MAX_Y) vga_drawpixel(x,y)

extern float max_lat, min_lat, max_lng, min_lng;
extern int color;
extern char *filename;

void
setrange(int start, int end, int startred, int startgreen,
	 int startblue, int endred, int endgreen, int endblue)
{
  int i;			/*

				 *
				 * *
				 * * *
				 * * * * set colors from start to end
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
  setrange(1, 43, 63, 0, 0, 63, 31, 0);
  setrange(44, 86, 63, 31, 0, 63, 63, 0);
  setrange(87, 129, 63, 63, 0, 0, 63, 0);
  setrange(130, 172, 0, 63, 0, 0, 0, 63);
  setrange(173, 255, 0, 0, 63, 31, 0, 31);

}

void
circle(int xc, int yc, int r)	/*
				 * x, y, and radius
				 */
{
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
}

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

void
vga_plot()
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

  _setcolor(0);
  vga_clear();
  _setcolor(color);

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
	  if ((x >= 0 && x < MAX_X && y >= 0 && y < MAX_Y) &&
	      (oldx >= 0 && oldx < MAX_X && oldy >= 0 && oldy < MAX_Y))
	    vga_drawline(oldx, oldy, x, y);
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
}
