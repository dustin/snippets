#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

SetUpColormap(Display * display, int screen, Window window, Visual
	      * visual, Colormap * colormap)
{
  int status = False;

  if (visual == DefaultVisual(display, screen))
    {
      *colormap = DefaultColormap(display, screen);
      status = True;
    }
  else
    {
      *colormap = XCreateColormap(display, window, visual,
				  AllocNone);

      if (*colormap != None)
	{
	  XSetWindowColormap(display, window, *colormap);
	  status = True;
	}
      else
	*colormap = DefaultColormap(display, screen);
    }

  return (status);
}
