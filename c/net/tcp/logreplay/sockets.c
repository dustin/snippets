/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.2 1998/12/07 20:03:21 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <syslog.h>
#include <netinet/tcp.h>

#include <replay.h>

extern int _debug;

int
getclientsocket(char *host, int port)
{
	struct hostent *hp;
	int     success, i, flag;
	register int s = -1;
	struct linger l;
	struct sockaddr_in sin;

	_ndebug(2, ("Building client socket for %s:%d\n", host, port));

	if (host == NULL || port == 0)
		return (-1);

	if ((hp = gethostbyname(host)) == NULL) {
#ifdef HAVE_HERROR
		herror("gethostbyname");
#else
		fprintf(stderr, "Error looking up %s\n", host);
#endif
		return (-1);
	}
	success = 0;

	/* of course, replace that 1 with the max number of con attempts */
	for (i = 0; i < 1; i++) {
		if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			return (-1);
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
		if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
			sleep(1);
		} else {
			success = 1;
			break;
		}
	}

	if (!success)
		s = -1;

	return (s);
}
