/* To compile, run it through your favorite ansi compiler something like
 * this :
 *
 *    gcc -o xkey xkey.c -lX11 -lm
 *
 * To run it, just use it like this :  xkey displayname:0
 * and watch as that display's keypresses show up in your shell window.
 *
 *    Dominic Giampaolo (nick@cs.maxine.wpi.edu)
 */
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xutil.h>
#include <X11/Shell.h>

char *TranslateKeyCode(XEvent *ev);
void dumpEvent(XKeyEvent *xke);
void resend(XEvent *xe, Window w);

Window source;
Window destination;

Display *source_d;
Display *dest_d;


void main(int argc, char **argv)
{
  char *s_hostname, *d_hostname;
  XEvent xev;

  if (argc<3) {
	fprintf(stderr, "Usage:  %s s_display d_display\n", argv[0]);
	exit(1);
  }
  s_hostname = argv[1];
  d_hostname = argv[2];

  source_d = XOpenDisplay(s_hostname);
  if (source_d == NULL) {
     fprintf(stderr, "Blah, can't open display: %s\n", s_hostname);
     exit(10);
   }

  dest_d = XOpenDisplay(d_hostname);
  if (source_d == NULL) {
     fprintf(stderr, "Blah, can't open display: %s\n", d_hostname);
     exit(10);
   }

/*
These were a couple of test windows I had open...
New Window:  0x3c00013
New Window:  0x4000013
*/

  XSelectInput(source_d, 0x3c00013, KeyPressMask);

  while(1) {
     XNextEvent(source_d, &xev);
     resend(&xev, 0x2c00522);
   }
}

void resend(XEvent *xe, Window w)
{
	XKeyEvent *xke;

	xke=(XKeyEvent *)xe;
	xke->window=w;
	XSendEvent(dest_d, w, False, KeyReleaseMask|KeyPressMask, (XEvent *)xke);
	XFlush(dest_d);
}

#define _dump(a) printf("%s:  0x%x\n", #a, xke->a);

void dumpEvent(XKeyEvent *xke)
{
	printf("\n--------------------------------------------\n");
	_dump(type);
	_dump(serial);
	_dump(send_event);
	_dump(display);
	_dump(window);
	_dump(root);
	_dump(subwindow);
	_dump(time);
	_dump(x);
	_dump(y);
	_dump(x_root);
	_dump(y_root);
	_dump(state);
	_dump(keycode);
	_dump(same_screen);
}
