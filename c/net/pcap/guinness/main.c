/*
 * Copyright (c) 1998  dustin sallings
 *
 * $Id: main.c,v 1.2 1998/10/05 21:45:13 dustin Exp $
 */

#include <stdlib.h>
#include <getopt.h>
#include "guinness.h"

void
usage(char *name)
{
	printf("Usage:  %s [-p]\n", name);
	printf("    -p turns on promiscious sniffing.\n");
}

void
main(int argc, char **argv)
{
	int     flags = 0;
	int     c;
	extern char *optarg;

	while ((c = getopt(argc, argv, "p")) != -1) {
		switch (c) {
		case 'p':
			flags |= FLAG_BIT(FLAG_PROMISC);
			break;
		default:
			usage(argv[0]);
			exit(-1);
			break;	/* not reached */
		}
	}

	process(flags);
}
