/*
 * Copyright (c)  2001 Dustin Sallings <dustin@spy.net>
 *
 * $Id: main.c,v 1.1 2001/01/09 08:05:20 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>


#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "xsentry.h"

Display        *display=NULL;
Window          rootwindow, window;
GC              gc;
int             screen=0, font_height=0, have_font=0;
XFontStruct    *font=NULL;

/* The listening socket */
int				lsocket=0;

void childLoop() {
	char msg[80];
	struct sockaddr_in fsin;
	int len=0;
	int cs=0;

	/* Loop forever waiting for connections */
	for(;;) {
		len=sizeof(fsin);
		/* Look for connections. */
		cs=accept(lsocket, (struct sockaddr *) & fsin, &len);
		/* Immediately close them */
		if(cs>=0) {
			close(cs);
		}

		sprintf(msg, "Connection attempt from %s", inet_ntoa(fsin.sin_addr));

		xloop(msg);
	}
}

/* Daemonize */
void
detach(void)
{
	int             pid;

	pid=fork();

	switch(pid) {
		case -1:
			perror("fork");
			exit(1);
			break;
		case 0:
			childLoop();
			break;
		default:
			exit(0);
	}
}

/* Refresh on expose events */
void
refresh(char *message)
{
	int             x, y;
	XWindowAttributes stuff;

	XGetWindowAttributes(display, window, &stuff);
	x = ((stuff.width / 2) - (XTextWidth(font, message, strlen(message)) / 2));
	y = ((stuff.height / 2) + (font->ascent / 2));
	XDrawImageString(display, window, gc, x, y, message, strlen(message));
	XFlush(display);
}

/* loop while the connection is supposed to be valid */
void
xloop(char *message)
{
	XEvent          event;

	init_window();


	while (1) {
		XNextEvent(display, &event);
		switch (event.type) {
		case MappingNotify:
			XRefreshKeyboardMapping(&event.xmapping);
			break;
		case Expose:
			refresh(message);
			break;
		case KeyPress:
			keyevent(&event.xkey);
			break;
		case ButtonPress:
			XCloseDisplay(display);
			return;
			break;
		}
	}
}

/* Bind to the socket we want to listen to */
void
dobindings(char *name, int port)
{
	unsigned int    bindaddr=0;
	struct sockaddr_in sin;
	int reuse=1;
	int rv=0;

	/* Get the socket */
	lsocket=socket(AF_INET, SOCK_STREAM, 0);
	assert(lsocket>0);

	/* get the bind address */
	bindaddr=inet_addr(name);
	assert(bindaddr!=INADDR_NONE);

	/* prepare to bind */
	memset((char *) &sin, 0x00, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);
	sin.sin_addr.s_addr = bindaddr;

	/* If we can't set this option, don't worry too much */
	setsockopt(lsocket, SOL_SOCKET, SO_REUSEADDR,
		(char *) &reuse, sizeof(int));

	printf("Binding to %s:%d\n", inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));

	rv=bind(lsocket, (struct sockaddr *) & sin, sizeof(sin));
	if(rv<0) {
		perror("bind");
		exit(1);
	}

	rv=listen(lsocket, 5);
	if(rv<0) {
		perror("listen");
		exit(1);
	}

	printf("Bound!\n");
}

int
main(int argc, char *argv[])
{
	if(argc<3) {
		fprintf(stderr, "Not enough arguments given.\nUsage:  %s ip port\n"
			"Where ip and port make up the local address to which to bind.\n",
				argv[0]);
		exit(1);
	}
	dobindings(argv[1], atoi(argv[2]));
	detach();
	exit(0);
}
