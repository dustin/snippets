#include <X11/Xlib.h>
#include <X11/Xutil.h>

FillSizeHints(int x, int y, int width, int height, XSizeHints * sizehints)
{
  sizehints->x = x;
  sizehints->y = y;
  sizehints->height = height;
  sizehints->width = width;
  sizehints->min_height = height;
  sizehints->min_width = width;
  sizehints->flags = USPosition | USSize | PMinSize;

  sizehints->base_width = width;
  sizehints->base_height = height;
  sizehints->flags |= PBaseSize;
}

SetSizeHints(Display * display, Window window, int x, int y, int width,
	     int height)
{
  XSizeHints sizehints;

  FillSizeHints(x, y, width, height, &sizehints);

  XSetWMNormalHints(display, window, &sizehints);
}
