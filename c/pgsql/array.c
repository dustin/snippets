/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: array.c,v 1.1 2002/03/01 09:14:57 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "array.h"

#define SPLITSIZE 64

char **split(const char *input, const char *delim)
{
	char **rv=NULL, *p=NULL, *in=NULL;
	int pos=0;

	assert(input);
	assert(delim);

	/* Copy the string so we can be more destructive */
	in=strdup(input);
	assert(in);

	/* Get the initial size */
	rv=calloc(sizeof(char *), SPLITSIZE);
	assert(rv);

	/* setup strtok */
	p=strtok(in, delim);
	assert(p);

	/* Get the tokens */
	for(; p!=NULL; p=strtok(NULL, delim)) {
		rv[pos++]=strdup(p);
		assert(pos<(SPLITSIZE-1));
	}
	rv[pos]=NULL;

	free(in);

	return(rv);
}

void freeList(char **list)
{
	int i=0;
	if(list!=NULL) {
		for(i=0; list[i]!=NULL; i++) {
			free(list[i]);
		}
		free(list);
	}
}

int listLength(char **list)
{
	int rv=0;
	for(rv=0; list[rv]!=NULL; rv++) {
		assert(rv<SPLITSIZE);
	}
	return(rv);
}
