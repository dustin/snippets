#include <X11/Xlib.h>
#include <X11/Xutil.h>

SetClassHints(Display * display, Window window, char *res_name, char
	      *res_class)
{
  XClassHint class_hints;

  class_hints.res_class = res_class;
  class_hints.res_name = res_name;

  XSetClassHint(display, window, &class_hints);
}
