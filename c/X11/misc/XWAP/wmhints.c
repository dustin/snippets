#include <X11/Xlib.h>
#include <X11/Xutil.h>

SetWMHints(Display * display, Window window, int initial_state)
{
  XWMHints wm_hints;

  wm_hints.flags = InputHint | StateHint;
  wm_hints.initial_state = initial_state;
  wm_hints.input = True;
  XSetWMHints(display, window, &wm_hints);
}
