/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: test.c,v 1.1 1998/06/21 21:56:47 dustin Exp $
 */

#include <stdio.h>
#include "radius.h"

int main(int argc, char **argv)
{
    int r;
    char *codes[]={
	0,
	"Request",
	"Accept",
	"Reject",
	0,0,0,0,0,0,0,
	"Challenge",
    };

    if(argc<3) {
	printf("Usage:  %s username password\n", argv[0]);
	exit(1);
    }

    r=rad_simpleauth(argv[1], argv[2]);

    if(r<0) {
        printf("Timeout\n");
    } else {
        printf("Server returned:  %d (%s)\n", r, codes[r]);
    }
}
