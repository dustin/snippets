#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

main(int argc, char *argv[])
{
  Display *display;
  Window rootwindow, window;
  int screen, x, y, width, height, count;
  Visual *visual = CopyFromParent;
  XEvent event;

  display = (Display *) ConnectToServer((char *) NULL, &screen, &rootwindow);

  x = y = 10;
  width = height = 300;

  window = OpenWindow(display, rootwindow, x, y, width, height,
		   BlackPixel(display, screen), WhitePixel(display, screen),
		      ExposureMask, visual);

  SetStandardHints(display, window, argv[0], argv[0], x, y,
		   width, height);

  XMapRaised(display, window);
  XFlush(display);

  count = 0;

  while (count < 20)
    {
      XNextEvent(display, &event);

      if (event.type == Expose)
	{
	  count++;
	  printf("For Expose event %d,", count);
	  printf("the area is:\n");
	  printf("\tAt %d,%d,", event.xexpose.x,
		 event.xexpose.y);

	  printf(" %d pixels wide, %d high\n",
		 event.xexpose.width, event.xexpose.height);
	}
    }

  XCloseDisplay(display);
}
