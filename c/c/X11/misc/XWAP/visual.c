
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

SetUpVisual(Display * display, int screen, Visual ** visual, int *depth)
{
  int number_visuals;
  XVisualInfo *visual_array;
  XVisualInfo visual_info_template;
  int status = False;

  if (DefaultVisual(display, screen)->class == PseudoColor)
    {
      *visual = DefaultVisual(display, screen);
      *depth = DefaultDepth(display, screen);
      status = True;
    }
  else
    {
      visual_info_template.class = PseudoColor;
      visual_info_template.screen = screen;
      visual_array = XGetVisualInfo(display, \
				    VisualClassMask | VisualScreenMask, \
				    &visual_info_template, &number_visuals);

      if ((number_visuals > 0) && (visual_array != NULL))
	{
	  *visual = visual_array[0].visual;
	  *depth = visual_array[0].depth;
	  XFree(visual_array);
	  status = True;
	}
      else
	{
	  *visual = CopyFromParent;
	  status = False;
	}
    }

  return (status);
}
