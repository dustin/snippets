/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.3 2003/06/12 20:18:13 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <assert.h>

/* Socket getter */
int attemptConnection(char *host, int port);

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
	time_t  t=0;

	if (argc < 3) {
		usage(argv[0]);
		exit(0);
	}

	hostname = argv[1];
	port = atoi(argv[2]);
	if(port < 1 || port > 65535) {
		usage(argv[0]);
		exit(0);
	}

	while(!attemptConnection(hostname, port)) {
		t=time(NULL);
		fprintf(stderr, "Failed to connect at %s", ctime(&t));
		sleep(1);
	}
	t=time(NULL);
	fprintf(stderr, "Connected at %s", ctime(&t));

	return(0);
}
