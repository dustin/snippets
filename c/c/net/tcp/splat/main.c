/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.3 2003/04/03 01:45:07 dustin Exp $
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

static void
usage(const char *name)
{
	fprintf(stderr,
		"Usage:\n  %s [-pb] [-n max_conns] [-d data] hostname port\n",
		name);
	fprintf(stderr, "\t-p Keep max_conns connections open\n");
	fprintf(stderr, "\t-b Disable non-blocking IO\n");
	fprintf(stderr, "\t-n Maximum number of connections to open\n");
}

int
main(int argc, char **argv)
{
	char   *hostname=0x00;
	char   *data=0x00;
	int     port=0, c=0, s=0, selected=0, size=0, currentlyOpen=0;
	int     keep_populated=0, sock_flags=NON_BLOCKING;
	fd_set  fdset, tfdset;
	char    buf[8192];
	struct timeval t;
	int maxconns=MAXINT;

	/* Process options */
	while ((c=getopt(argc, argv, "bpn:d:")) >= 0) {
		switch(c) {
			case 'p':
				keep_populated=1;
				break;
			case 'n':
				maxconns=atoi(optarg);
				break;
			case 'd':
				data=optarg;
				break;
			case 'b':
				sock_flags &= (~NON_BLOCKING);
				break;
			case '?':
				usage(argv[0]);
				return(1);
				break;
		}
	}

	if (optind >= argc) {
		usage(argv[0]);
		exit(0);
	}

	FD_ZERO(&tfdset);

	hostname = argv[optind];
	port = atoi(argv[optind+1]);

	resettraps();

	for (;;) {
		int numToOpen=1, i;

		/* Figure out how many connections we need to open */
		if(keep_populated) {
			numToOpen=(maxconns-currentlyOpen);
		}

		/* Open them (if the number is less than 1, it won't do anything */
		for(i=0; i<numToOpen; i++) {
			_ndebug(2, ("Connecting to ``%s:%d''\n", hostname, port));
			s = getclientsocket(hostname, port, sock_flags);
			if (s > 0) {
				printf("Got one: %d...\n", s);
				FD_SET(s, &tfdset);
				currentlyOpen++;
				if (data != NULL) { /* send some data if we've got some */
					_ndebug(2, ("Sending ``%s''\n", data));
					send(s, data, strlen(data), 0);
				}
			}
		}
		fdset = tfdset;
		t.tv_sec = 1;
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
						currentlyOpen--;
						FD_CLR(i, &fdset);
					} else {
						_ndebug(2, ("Got %d bytes from %d\n", size, i));
					}
				}
				if (selected == 0) {
					break;
				}
			}
		}		/* select */
	}			/* infinite loop */
}
