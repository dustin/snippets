/*
 * Copyright (c)  2001 Dustin Sallings <dustin@spy.net>
 *
 * $Id: xsentry.h,v 1.2 2001/01/09 08:11:22 dustin Exp $
 */

#ifndef SIMPLEDEFINES

#define SIMPLEDEFINES
#define WIN_TITLE "X Sentry"
#define PROGGYCLASS "XSentry"
#define PROGGYNAME "XSentry"
#define WIN_WIDTH 320
#define WIN_HEIGHT 200

#ifdef DEBUG
#define FONT "variable"
#else
#define FONT "-adobe-new century schoolbook-bold-r-normal--15-20-*-*-p-*-iso8859-1"
#endif

#define BACKFONT "variable"
#endif

#ifndef ALARMLENGTHDEFAULT
#define ALARMLENGTHDEFAULT 2*60*60
#endif

void            init_window(void);
void            keyevent(XKeyEvent * event);
void            buttonevent(int x, int y, int button);
void            detach(void);
void            xloop();
