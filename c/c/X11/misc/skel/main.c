#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

Display *display;
Window rootwindow, window;
GC gc;
int screen, font_height, have_font;
XFontStruct *font;

int verbose = 0;

int
main(int argc, char *argv[])
{
  XEvent event;

  init_window();
  while (1)
    {
      XNextEvent(display, &event);
      switch (event.type)
	{
	case MappingNotify:
	  XRefreshKeyboardMapping(&event.xmapping);
	  break;
	case Expose:
	  break;
	case KeyPress:
	  keyevent(&event.xkey);
	  break;
	case ButtonPress:
	  buttonevent(event.xbutton.x, event.xbutton.y, event.xbutton.button);
	  break;
	}
    }
  exit(0);
}
