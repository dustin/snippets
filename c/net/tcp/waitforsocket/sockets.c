/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.4 2003/06/12 20:20:30 dustin Exp $
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
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
	fd_set wset;
	fd_set eset;
	struct timeval tv;
	int success=0;

	FD_ZERO(&rset);
	FD_ZERO(&wset);
	FD_ZERO(&eset);
	FD_SET(s, &rset);
	FD_SET(s, &wset);
	FD_SET(s, &eset);

	/* Wait up to five seconds */
	tv.tv_sec=5;
	tv.tv_usec=0;

	selected=select(s+1, &rset, &wset, &eset, &tv);
	if(selected > 0) {
		if(FD_ISSET(s, &rset)) {
			char buf[1];
			/* Make sure we can read a byte */
			if(read(s, &buf, 1) == 1) {
				success=1;
			}
		} else if(FD_ISSET(s, &wset)) {
			success=1;
		} else {
			success=0;
		}
	}

	/* True if there was at least one thing that hinted as being available */
	return(success == 1);
}

int
attemptConnection(char *host, char *svc)
{
	struct addrinfo hints, *res, *res0;
	int     success, i, flag;
	register int s = -1;
	struct linger l;
	int fflags =0;
	char *cause=NULL;

	if (host == NULL || svc == NULL) {
		return (0);
	}

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = PF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	if(getaddrinfo(host, svc, &hints, &res0) != 0) {
		perror(host);
		return(0);
	}

	success = 0;

	/* of course, replace that 1 with the max number of con attempts */
	cause="no addresses";
	for (res = res0; res; res = res->ai_next) {

		if ((s = socket(res->ai_family, res->ai_socktype,
			res->ai_protocol)) < 0) {

			cause="socket";
			continue;
		}

		l.l_onoff = 1;
		l.l_linger = 60;
		setsockopt(s, SOL_SOCKET, SO_LINGER, (char *) &l, sizeof(l));

		/* Configure non-blocking IO */
		fflags = fcntl(s, F_GETFL);
		if(fcntl(s, F_SETFL, fflags | O_NONBLOCK) < 0) {
			perror("fcntl");
		}

		if (connect(s, res->ai_addr, res->ai_addrlen) < 0) {
			if(errno==EINPROGRESS) {
				success = 1;
				break;
			} else {
				cause="connect";
			}
		} else {
			success = 1;
			break;
		}
	}

	/* If we got this far, wait for data */
	if(success) {
		success=waitForConnect(s);
	} else {
		perror(cause);
	}

	if(s>=0) {
		close(s);
	}

	return (success == 1);
}
