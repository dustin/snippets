/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: test.c,v 1.4 1998/12/26 05:06:01 dustin Exp $
 */

#include <stdio.h>
#include "radius.h"
#include "mymalloc.h"

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
    radius *rad;

    if(argc<3) {
	printf("Usage:  %s username password\n", argv[0]);
	exit(1);
    }

    rad=rad_init("localhost", 1645, "no");
    r=rad_simpleauth(rad, argv[1], argv[2]);
    rad_dump_att(rad);
    rad_destroy(rad);

    if(r<0) {
	switch(r) {
	    case RADIUS_TIMEOUT:
		printf("Error #%d, RADIUS_TIMEOUT\n", r);
		break;
	    case RADIUS_FAIL_VERIFY:
		printf("Error #%d, RADIUS_FAIL_VERIFY\n", r);
		break;
	    default:
		printf("Unknown error #%d...\n", r);
		break;
	}
    } else {
        printf("Server returned:  %d (%s)\n", r, codes[r]);
    }

#ifdef MYMALLOC
    _mdebug_dump();
#endif
    return(0);
}
