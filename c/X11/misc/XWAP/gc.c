#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

GC 
CreateGC(Display * display, Drawable drawable, unsigned long
	 forecolor, unsigned long backcolor)
{
  XGCValues xgcvalues;
  GC gc;

  xgcvalues.foreground = forecolor;
  xgcvalues.background = backcolor;

  gc = XCreateGC(display, drawable, (GCForeground | GCBackground),
		 &xgcvalues);

  return (gc);
}
