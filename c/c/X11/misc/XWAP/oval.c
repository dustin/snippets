#include <stdio.h>
#include <X11/Xlib.h>

#define START 0
#define FULL (360*64)

DrawOval(Display * display, Drawable drawable, GC gc, int x, int y, int
	 width, int height)
{
  if (width < 1)
    width = 1;
  if (height < 1)
    height = 1;

  XDrawArc(display, drawable, gc, x, y, width, height, START,
	   FULL);
}

FillOval(Display * display, Drawable drawable, GC gc, int x, int y, int
	 width, int height)
{
  if (width < 1)
    width = 1;
  if (height < 1)
    height = 1;

  XFillArc(display, drawable, gc, x, y, width, height, START,
	   FULL);
}
