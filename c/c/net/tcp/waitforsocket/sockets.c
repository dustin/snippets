/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.3 2003/06/12 20:18:14 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <sys/errno.h>
#include <netinet/in.h>
#include <netdb.h>
#include <syslog.h>
#include <netinet/tcp.h>

static int waitForConnect(int s)
{
	int selected=0;
	fd_set rset;
	struct timeval tv;

	FD_ZERO(&rset);
	FD_SET(s, &rset);

	/* Wait up to five seconds */
	tv.tv_sec=5;
	tv.tv_usec=0;

	selected=select(s+1, &rset, NULL, NULL, &tv);

	/* True if there was at least one thing that hinted as being available */
	return(selected == 1);
}

int
attemptConnection(char *host, int port)
{
	struct hostent *hp;
	int     success, i, flag;
	register int s = -1;
	struct linger l;
	struct sockaddr_in sin;
	int fflags =0;

	if (host == NULL || port == 0) {
		return (0);
	}

	if ((hp = gethostbyname(host)) == NULL) {
#ifdef HAVE_HERROR
		herror("gethostbyname");
#else
		fprintf(stderr, "Error looking up %s\n", host);
#endif
		return (0);
	}
	success = 0;

	/* of course, replace that 1 with the max number of con attempts */
	for (i = 0; i < 1; i++) {
		if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			return (0);
		}
		sin.sin_family = AF_INET;
		sin.sin_port = htons(port);
		memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);

		l.l_onoff = 1;
		l.l_linger = 60;
		setsockopt(s, SOL_SOCKET, SO_LINGER, (char *) &l, sizeof(l));

		flag = 1;
		if (setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char *) &flag,
			sizeof(int)) < 0) {
			puts("Nagle algorithm not dislabled.");
		}

		/* Configure non-blocking IO */
		fflags = fcntl(s, F_GETFL);
		if(fcntl(s, F_SETFL, fflags | O_NONBLOCK) < 0) {
			perror("fcntl");
		}

		if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
			if(errno==EINPROGRESS) {
				success = 1;
			} else {
				perror("connect");
			}
		} else {
			success = 1;
			break;
		}
	}

	/* If we got this far, wait for data */
	if(success) {
		success=waitForConnect(s);
	}

	if(s>=0) {
		close(s);
	}

	return (success == 1);
}
