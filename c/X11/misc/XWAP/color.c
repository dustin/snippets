#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

unsigned long
AllocNamedColor(Display * display, Colormap colormap, char
		colorname[], unsigned long default_color)
{
  XColor hardwarecolor, exactcolor;
  unsigned long color;
  int status;

  status = XAllocNamedColor(display, colormap, colorname,
			    &hardwarecolor, &exactcolor);

  if (status != 0)
    color = hardwarecolor.pixel;
  else
    color = default_color;
}
