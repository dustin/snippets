#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

unsigned long black, white, red, green, blue, magenta;

main(int argc, char *argv[])
{
  Display *display;
  Window rootwindow, window;
  int screen, x, y, width, height, count, depth, status;
  Visual *visual = CopyFromParent;
  GC gc;
  XEvent event;
  Colormap colormap;

  display = ConnectToServer(NULL, &screen, &rootwindow);

  status = SetUpVisual(display, screen, &visual, &depth);

  if (status != True)
    {
      fprintf(stderr, "Error in finding a PseudoColor \
visual.\n");
      XCloseDisplay(display);
      exit(1);
    }

  x = y = 10;
  width = height = 300;

  window = OpenWindow(display, rootwindow, x, y, width, height,
		   BlackPixel(display, screen), WhitePixel(display, screen),
		      ExposureMask, visual);

  SetStandardHints(display, window, argv[0], argv[0], x, y, width,
		   height);

  status = SetUpColormap(display, screen, window, visual, &colormap);

  if (status != True)
    {
      fprintf(stderr, "Error: Could not create a colormap.\n");
      XCloseDisplay(display);
      exit(1);
    }

  black = AllocNamedColor(display, colormap, "black",
			  BlackPixel(display, screen));
  white = AllocNamedColor(display, colormap, "white",
			  WhitePixel(display, screen));
  red = AllocNamedColor(display, colormap, "red", black);
  blue = AllocNamedColor(display, colormap, "blue", black);
  green = AllocNamedColor(display, colormap, "green", black);
  magenta = AllocNamedColor(display, colormap, "magenta", black);

  gc = CreateGC(display, window, black, white);

  XMapRaised(display, window);
  XFlush(display);

  count = 0;
  while (count < 10)
    {
      XNextEvent(display, &event);

      if (event.type == Expose)
	{
	  Redraw(display, window, gc);
	  count++;
	}
    }

  XCloseDisplay(display);
}

Redraw(Display * display, Window window, GC gc)
{
  XSetForeground(display, gc, blue);
  XDrawLine(display, window, gc, 1, 1, 100, 100);

  XSetForeground(display, gc, red);
  XDrawRectangle(display, window, gc, 100, 100, 100, 100);

  XSetForeground(display, gc, green);
  XFillRectangle(display, window, gc, 200, 200, 100, 100);

  XSetForeground(display, gc, black);
  DrawOval(display, window, gc, 100, 100, 100, 100);

  XSetForeground(display, gc, magenta);
  FillOval(display, window, gc, 110, 110, 64, 64);

  XFlush(display);
}
