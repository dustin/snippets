#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define BORDERWIDTH 2
#define APP_CLASS "Examples"

Window 
OpenWindow(Display * display, Window parent, int x, int y, int
      width, int height, unsigned long bordercolor, unsigned long backcolor,
	   unsigned long event_mask, Visual * visual)
{
  Window window;
  XSetWindowAttributes attributes;
  unsigned long attr_mask;

  attributes.event_mask = event_mask;
  attributes.border_pixel = bordercolor;
  attributes.background_pixel = backcolor;
  attr_mask = CWEventMask | CWBackPixel | CWBorderPixel;

/*
 * attributes.override_redirect=True;
 * attr_mask|=CWOverrideRedirect;
 */

  window = XCreateWindow(display, parent, x, y, width, height,
		   BORDERWIDTH, CopyFromParent, InputOutput, CopyFromParent,
			 attr_mask, &attributes);
}

SetStandardHints(Display * display, Window window, char app_name[], char
		 wind_name[], int x, int y, int width, int height)
{
  SetSizeHints(display, window, x, y, width, height);
  SetWindowName(display, window, wind_name);
  SetClassHints(display, window, app_name, APP_CLASS);
  SetWMHints(display, window, NormalState);
}
