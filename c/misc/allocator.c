/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: allocator.c,v 1.2 2002/05/02 23:01:51 dustin Exp $
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifndef DEFAULT_MAX_SIZE
#define DEFAULT_MAX_SIZE 1024*1024*1024
#endif /* DEFAULT_MAX_SIZE */

#define SUCCESS 2
#define FAIL 3

int allocate(size_t size)
{
	int rv=FAIL;
	char *p=NULL;

	printf("\rAllocating %dk          ", (int)size/1024);
	fflush(stdout);
	p=(char *)malloc(size);
	if(p==NULL) {
		puts("");
		perror("malloc");
		fprintf(stderr, "Failed at %dk\n", (int)size/1024);
	} else {
		rv=SUCCESS;
		memset(p, 0x00, size);
		free(p);
	}
	return(rv);
}

int
main(int argc, char **argv)
{
	int i=0;
	int incramount=128*1024*1024;
	int maxsize=DEFAULT_MAX_SIZE;

	/* Stay within the constraints of the user */
	if(argc>1) {
		maxsize=atoi(argv[1]);
		if(maxsize<1) {
			maxsize=DEFAULT_MAX_SIZE;
		}
	}

	/* Make sure we aren't starting out asking for too much memory */
	while(incramount >= maxsize) {
		incramount>>=1;
	}

	printf("Allocating no more than %dk of memory in increments of %dk\n",
		maxsize/1024, incramount/1024);

	for(i=incramount; incramount>=1 && i<=maxsize; i+=incramount) {
		if(allocate(i) != SUCCESS) {
			i-=incramount;
			incramount>>=1;
			printf("Counting up from %dk (by %d)\n", i/1024, incramount);
		}
	}

	return(0);
}
