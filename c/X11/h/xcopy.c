/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: xcopy.c,v 1.3 2000/05/05 06:02:02 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xutil.h>
#include <X11/Shell.h>

#define WIDTH  100
#define HEIGHT 20

char           *TranslateKeyCode(XEvent * ev);
void            dumpEvent(XKeyEvent * xke);
void            resend(Display * d, XEvent * xe, Window w);
void            docopy(Display * s, Window s_w, Display * d, Window d_w);
Window          openInputWindow(Display * d);

void
main(int argc, char **argv)
{
	char           *s_dpyname, *d_dpyname;
	/* Source and destination displays */
	Display        *source_d, *dest_d;
	/* Source and destination Windows */
	Window          s_w, d_w;

	if (argc < 3) {
		fprintf(stderr, "Usage:  %s d_display d_id\n", argv[0]);
		exit(1);
	}
	d_dpyname = argv[1];
	sscanf(argv[2], "0x%x", &d_w);

	s_dpyname = getenv("DISPLAY");
	if (s_dpyname == NULL) {
		s_dpyname = ":0";
	}
	source_d = XOpenDisplay(s_dpyname);
	if (source_d == NULL) {
		fprintf(stderr, "Blah, can't open source display: %s\n", s_dpyname);
		exit(10);
	}
	dest_d = XOpenDisplay(d_dpyname);
	if (source_d == NULL) {
		fprintf(stderr, "Blah, can't open destination display: %s\n",
			d_dpyname);
		exit(10);
	}
	s_w = openInputWindow(source_d);

	docopy(source_d, s_w, dest_d, d_w);
}

Window
openInputWindow(Display * d)
{
	Window          w;
	GC              gc;
	XGCValues       xgcvalues;
	XSetWindowAttributes attributes;
	int             screen;

	/* We'll be using this soon */
	screen = DefaultScreen(d);

	/* The Window itself */
	attributes.border_pixel = BlackPixel(d, screen);
	attributes.background_pixel = WhitePixel(d, screen);
	w = XCreateWindow(d, DefaultRootWindow(d), 0, 0, WIDTH, HEIGHT, 2,
			  CopyFromParent, InputOutput, CopyFromParent,
			  CWBackPixel | CWBorderPixel, &attributes);

	/* A graphics context */

	xgcvalues.foreground = BlackPixel(d, screen);
	xgcvalues.background = WhitePixel(d, screen);
	gc = XCreateGC(d, (Drawable) w, (GCForeground | GCBackground),
		       &xgcvalues);

	XFillRectangle(d, (Drawable) w, gc, 1, 1, WIDTH - 2, HEIGHT - 2);

	XStoreName(d, w, "xCopy Input");
	XMapRaised(d, w);
	XFlush(d);

	return (w);
}

void
docopy(Display * s, Window s_w, Display * d, Window d_w)
{
	XSelectInput(s, s_w, KeyPressMask);
	while (1) {
		XEvent          xev;
		XNextEvent(s, &xev);
		resend(d, &xev, d_w);
	}
}

void
resend(Display * d, XEvent * xe, Window w)
{
	XKeyEvent      *xke;

	xke = (XKeyEvent *) xe;
	xke->window = w;
	XSendEvent(d, w, False, KeyReleaseMask | KeyPressMask, (XEvent *) xke);
	XFlush(d);
}

#define _dump(a) printf("%s:  0x%x\n", #a, xke->a);

void
dumpEvent(XKeyEvent * xke)
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
