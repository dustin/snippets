
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

Redraw(Display * display, Window window, GC gc)
{
  XDrawLine(display, window, gc, 1, 1, 100, 100);

  XDrawRectangle(display, window, gc, 100, 100, 100, 100);
  XFillRectangle(display, window, gc, 200, 200, 100, 100);

  DrawOval(display, window, gc, 100, 100, 100, 100);
  FillOval(display, window, gc, 110, 110, 64, 64);

  XFlush(display);
}

main(int argc, char *argv[])
{
  Display *display;
  Window rootwindow, window;
  int screen, x, y, width, height, count;
  Visual *visual = CopyFromParent;
  GC gc;
  XEvent event;

  display = ConnectToServer(NULL, &screen, &rootwindow);

  x = y = 10;
  width = height = 300;

  window = OpenWindow(display, rootwindow, x, y, width, height,
     BlackPixel(display, screen), WhitePixel(display, screen), ExposureMask,
		      visual);

  SetStandardHints(display, window, argv[0], argv[0], x, y, width,
		   height);

  gc = CreateGC(display, window, BlackPixel(display, screen),
		WhitePixel(display, screen));

  XMapRaised(display, window);
  XFlush(display);

  count = 0;
  while (count < 20)
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
