/*
 * Copyright (c)  Dustin Sallings
 *
 * $Id: doit.c,v 1.1 1998/06/10 08:41:58 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <sys/time.h>
#include "data.h"

void printtimediff(struct timeval tp[2])
{
    printf("Time: %d.%d seconds\n", tp[1].tv_sec-tp[0].tv_sec,
                                    tp[1].tv_usec-tp[0].tv_usec);
}

void main(int argc, char **argv)
{
    char buf[1024];
    FILE *f;
    struct hashtable *hash;
    struct hash_container *h;
    struct timeval tp[2];
    void *tzp;

    hash=hash_init(HASHSIZE);

    if(argc<2)
    {
	printf("Give me garbage to hash, bitch\n");
	exit(0);
    }

    f=fopen(argv[1], "r");
    assert(f);

    printf("Max is %d\n", hash->hashsize);

    for(;;)
    {
	fgets(buf, 1024, f);
	buf[strlen(buf)-1]=0x00;
	if(feof(f))
	    break;
	if(hash_store(hash, buf, NULL)==NULL){
	    printf("Problem storing...\n");
	    exit(1);
	}
    }

    for(;;)
    {
	printf("Lookup>  ");
	fflush(stdout);

	fgets(buf, 1024, stdin);
	buf[strlen(buf)-1]=0x00;
	if(feof(stdin))
	    break;

	gettimeofday(&tp[0], tzp);
        h=hash_find(hash, buf);
	gettimeofday(&tp[1], tzp);

        if(h)
	    printf("%s was found!\n", buf);
        else
	    printf("%s was not found.  :(\n", buf);

	printtimediff(tp);
    }

    puts("");
}
