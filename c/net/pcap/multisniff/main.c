/*
 * Copyright (c) 1998  dustin sallings
 *
 * $Id: main.c,v 1.3 2000/07/30 03:01:51 dustin Exp $
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
/*
#include <getopt.h>
*/
#include "mymalloc.h"
#include "multisniff.h"

void
usage(char *name)
{
	fprintf(stderr, "Usage:  %s -i <intf> [-p] [-d <outdir>] [-f <filter>]\n",
		name);
	fprintf(stderr, "    -d specifies the output directory.\n");
	fprintf(stderr, "    -p turns on promiscious sniffing.\n");
	fprintf(stderr, "    -f pcap filter expression.\n");
	exit(1);
}

int
main(int argc, char **argv)
{
	int             flags = 0;
	int             c = 0;
	extern char    *optarg;
	char           *filter = NULL;
	char           *outdir = ".";
	char           *intf = NULL;

	while ((c = getopt(argc, argv, "pi:d:f:")) != -1) {
		switch (c) {
		case 'p':
			flags |= FLAG_BIT(FLAG_PROMISC);
			break;
		case 'f':
			filter = strdup(optarg);
			break;
		case 'd':
			outdir = strdup(optarg);
			break;
		case 'i':
			intf = strdup(optarg);
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
	if (intf == NULL) {
		fprintf(stderr, "Must supply an interface\n");
		usage(argv[0]);
	}
	process(flags, intf, outdir, filter);
	return (0);
}
