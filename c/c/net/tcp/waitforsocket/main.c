/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.2 2003/06/12 18:47:10 dustin Exp $
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
#include <time.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <assert.h>

/* Socket getter */
int getclientsocket(char *host, int port);

static RETSIGTYPE serv_conn_alrm(int sig);

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
	fprintf(stderr, "Usage:\n  %s hostname port\n", name);
}

int
main(int argc, char **argv)
{
	char   *hostname=0x00;
	int     port=0;
	int     s=-1;

	if (argc < 3) {
		usage(argv[0]);
		exit(0);
	}

	hostname = argv[1];
	port = atoi(argv[2]);

	resettraps();

	while(s<0) {
		time_t t=time(NULL);
		s = getclientsocket(hostname, port);
		if(s<0) {
			fprintf(stderr, "Failed to connect at %s", ctime(&t));
			sleep(1);
		} else {
			fprintf(stderr, "Connected on %d at %s", s, ctime(&t));
		}
	}
	close(s);

	return(0);
}
