#include <X11/Xlib.h>

SetWindowName(Display * display, Window window, char name[])
{
  XStoreName(display, window, name);
}
