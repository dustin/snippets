#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "xhealth.h"

Display *display;
Window rootwindow, window;
GC gc;
int screen, font_height, have_font;
XFontStruct *font;

char *message;

int verbose = 0, alarmlength = ALARMLENGTHDEFAULT;

void
detach(void)
{
  int pid;

  if ((pid = fork()) < 0)
    {
      perror("fork");
      exit(1);
    }

  if (pid != 0)
    exit(0);
}

void
refresh(void)
{
int x, y;
XWindowAttributes stuff;

  XGetWindowAttributes(display, window, &stuff);
  x=( (stuff.width/2) - (XTextWidth(font, message, strlen(message) )/2) );
  y=( (stuff.height/2) + (font->ascent/2) );
  XDrawImageString(display, window, gc, x, y, message, strlen(message));
  XFlush(display);
}



void
xloop()
{
  XEvent event;

#ifndef DEBUG
  signal(SIGALRM, SIG_IGN);
#endif

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
          refresh();
	  break;
	case KeyPress:
	  keyevent(&event.xkey);
	  break;
	case ButtonPress:
	  XCloseDisplay(display);
	  signal(SIGALRM, xloop);
	  alarm(alarmlength);
	  return;
	  break;
	}
    }
}

int
main(int argc, char *argv[])
{

  if(argc>2)
	message=argv[1];
  else
        message=MESSAGE;

#ifdef DEBUG
  xloop();
#else
  detach();

  alarm(alarmlength);
  signal(SIGALRM, xloop);

  while (1)
    pause();
#endif

  exit(0);
}
