/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.5 1998/10/03 08:02:33 dustin Exp $
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

#include <mbkd.h>
#include <nettools.h>

char   *
getHostName(unsigned int addr)
{
	struct hostent *h;
	char   *name;

	h = gethostbyaddr((void *) &addr, sizeof(unsigned int), AF_INET);
	if (h == NULL)
		name = nmc_intToDQ(ntohl(addr));
	else
		name = h->h_name;

	return (name);
}

int
getservsocket_udp(int port)
{
	int     reuse = 1, s;
	struct sockaddr_in sin;

	signal(SIGPIPE, SIG_IGN);

	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("server: socket");
		exit(1);
	}
	memset((char *) &sin, 0x00, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);
	sin.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		perror("server: bind");
		exit(1);
	}
	return (s);
}
