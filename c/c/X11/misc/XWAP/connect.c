#include <stdio.h>
#include <X11/Xlib.h>

Display *
ConnectToServer(char display_name[], int *screen, Window *rootwindow)
{
  Display *display;

  display = XOpenDisplay(display_name);

  if (display == (Display *) NULL)
    {
      fprintf(stderr, "Cannot connect to X server: %s\n",
	      XDisplayName(display_name));

      exit(1);
    }

  *screen = DefaultScreen(display);
  *rootwindow = RootWindow(display, *screen);
  return (display);
}
