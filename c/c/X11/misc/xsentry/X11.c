/*
 * Copyright (c)  2001 Dustin Sallings <dustin@spy.net>
 *
 * $Id: X11.c,v 1.1 2001/01/09 08:05:18 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#include "xsentry.h"

extern Display *display;
extern Window   window, rootwindow;
extern GC       gc;
extern int      screen, verbose;

extern XFontStruct *font;
extern int      font_height, have_font;

void
init_window(void)
{
	XSetWindowAttributes attributes;
	unsigned long   attr_mask=0, event_mask=0;
	XSizeHints      sizehints;
	XClassHint      class_hints;
	XWMHints        wmhints;
	XGCValues       xgcvalues;
	Cursor          cursor;

	if (NULL == (display = XOpenDisplay(NULL))) {
		fprintf(stderr, "Cannot connect to X server: %s\n",
			XDisplayName(NULL));

		exit(1);
	}
	screen = DefaultScreen(display);
	rootwindow = RootWindow(display, screen);

	event_mask = ButtonPressMask | ExposureMask | KeyPressMask |
		PointerMotionMask;

	cursor = XCreateFontCursor(display, XC_pirate);

	attributes.event_mask = event_mask;
	attributes.border_pixel = BlackPixel(display, screen);
	attributes.background_pixel = WhitePixel(display, screen);
	attributes.cursor = cursor;

	attr_mask = CWEventMask | CWBackPixel | CWBorderPixel | CWCursor;

	window = XCreateWindow(display, rootwindow, 0, 0, WIN_WIDTH, WIN_HEIGHT, 2,
	CopyFromParent, InputOutput, CopyFromParent, attr_mask, &attributes);

	sizehints.height = WIN_HEIGHT;
	sizehints.width = WIN_WIDTH;
	sizehints.x = 0;
	sizehints.y = 0;
	sizehints.flags = USSize | USPosition;

	XSetWMNormalHints(display, window, &sizehints);

	XStoreName(display, window, WIN_TITLE);

	class_hints.res_class = PROGGYCLASS;
	class_hints.res_name = PROGGYNAME;
	XSetClassHint(display, window, &class_hints);

	wmhints.flags = InputHint | StateHint;
	wmhints.initial_state = NormalState;
	wmhints.input = True;
	XSetWMHints(display, window, &wmhints);

	xgcvalues.foreground = BlackPixel(display, screen);
	xgcvalues.background = WhitePixel(display, screen);
	gc = XCreateGC(display, (Drawable) window, (GCForeground | GCBackground),
		       &xgcvalues);

	font = XLoadQueryFont(display, FONT);

	have_font = True;

	if (font == NULL)
		font = XLoadQueryFont(display, BACKFONT);
	if (font == NULL)
		have_font = False;

	if (have_font) {
		font_height = font->ascent + font->descent;
		XSetFont(display, gc, font->fid);
	}
	XMapRaised(display, window);

	XFlush(display);

}

void
keyevent(XKeyEvent * event)
{
	KeySym          keysym;
	XComposeStatus  cs;
	int             x, y;
	char            whatkey[70];

	keysym = 0x0;
	whatkey[0] = '\0';
	x = event->x, y = event->y;
	XLookupString(event, whatkey, 1, &keysym, &cs);
#ifdef DEBUG
	printf("%d (%c) x:%d y:%d\n", whatkey[0], whatkey[0], x, y);
#endif
	switch (whatkey[0]) {
	case 'q':
		XCloseDisplay(display);
		exit(0);
		break;
	}
}

void
buttonevent(int x, int y, int button)
{
	switch (button) {

		case 1:
		break;

	case 2:
		exit(0);

	case 3:
		break;
	}
}
