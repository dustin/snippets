/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.3 2003/06/12 20:18:13 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include "waitforsocket.h"

static void
usage(const char *name)
{
	fprintf(stderr, "Usage:\n  %s hostname service|port\n", name);
}

int
main(int argc, char **argv)
{
	char   *hostname=0x00;
	char   *svc=0x00;
	time_t  t=0, status;
	extern int errno;

	if (argc < 3) {
		usage(argv[0]);
		exit(0);
	}

	hostname = argv[1];
	svc=argv[2];

	while((status=attemptConnection(hostname, svc)) != RV_SUCCESS) {
		t=time(NULL);
		char *err="unknown";
		if(status == ERR_ERRNO) {
			err=strerror(errno);
		} else if(status == ERR_TIMEOUT) {
			err="timeout";
		} else if(status == ERR_DNS) {
			err="getaddrinfo error";
		} else {
			/* This is basically a programming error, but it should at least
			   be able to tell us *what* programming error. */
			char buf[32];
			snprintf(buf, sizeof(buf)-1, "unknown: %d", status);
			err=buf;
		}
		assert(err != NULL);
		fprintf(stderr, "Failed to connect (%s) at %s", err, ctime(&t));
		sleep(1);
	}
	t=time(NULL);
	fprintf(stderr, "Connected at %s", ctime(&t));

	return(0);
}
