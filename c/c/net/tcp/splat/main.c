/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.2 1999/01/05 20:09:31 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <assert.h>

#include <splat.h>

static RETSIGTYPE serv_conn_alrm(int sig);

int     _debug = 3;

#define MAXSEL 1024

static void
resettraps(void)
{
	signal(SIGALRM, serv_conn_alrm);
}

static RETSIGTYPE
serv_conn_alrm(int sig)
{
	resettraps();
	return;
}

int
main(int argc, char **argv)
{
	char   *hostname;
	int     port, s, selected, size;
	fd_set  fdset, tfdset;
	char    buf[8192];
	struct timeval t;

	if (argc < 3) {
		printf("Too few arguments, usage:\n%s hostname port [data]\n",
			argv[0]);
		exit(0);
	}
	FD_ZERO(&tfdset);

	hostname = argv[1];
	port = atoi(argv[2]);

	resettraps();

	for (;;) {
		s = getclientsocket(hostname, port);
		if (s > 0) {
			printf("Got one: %d...\n", s);
			FD_SET(s, &tfdset);
			if (argc > 3) { /* send some data if we've got some */
				_ndebug(2, ("Sending ``%s''\n", argv[3]));
				send(s, argv[3], strlen(argv[3]), 0);
			}
		}
		fdset = tfdset;
		t.tv_sec = 0;
		t.tv_usec = 2600;

		_ndebug(2, ("Selecting...\n"));

		if ((selected = select(MAXSEL, &fdset, NULL, NULL, &t)) > 0) {
			int     i;
			for (i = 0; i < MAXSEL; i++) {
				if (FD_ISSET(i, &fdset)) {
					_ndebug(3, ("Caught %d\n", i));
					selected--;
					size = recv(i, buf, 8192, 0);
					if (size == 0) {
						_ndebug(2, ("Lost %d\n", i));
						close(i);
						FD_CLR(i, &fdset);
					} else {
						_ndebug(2, ("Got %d bytes from %d\n", size, i));
					}
				}
				if (selected == 0)
					break;
			}
		}		/* select */
	}			/* infinite loop */
}
