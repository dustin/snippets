/*
 * Copyright (c) 1998  dustin sallings
 *
 * $Id: main.c,v 1.2 2000/07/29 11:02:22 dustin Exp $
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
/*
#include <getopt.h>
*/
#include "acct.h"

void
usage(char *name)
{
	printf("Usage:  %s [-p] [-f <filter>]\n", name);
	printf("    -p turns on promiscious sniffing.\n");
	printf("    -f <filter> Filter.\n");
}

int
main(int argc, char **argv)
{
	int             flags = 0;
	int             c = 0;
	extern char    *optarg;
	char           *filter = NULL;

	while ((c = getopt(argc, argv, "pf:")) != -1) {
		switch (c) {
		case 'p':
			flags |= FLAG_BIT(FLAG_PROMISC);
			break;
		case 'f':
			filter = strdup(optarg);
			break;
		default:
			usage(argv[0]);
			exit(-1);
			break;	/* not reached */
		}
	}

	if (filter == NULL) {
		filter = "ip";
	}
	process(flags, filter);
	return (0);
}
