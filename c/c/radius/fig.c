/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: fig.c,v 2.2 1997/09/21 00:32:57 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "parse.h"

/* kill whitey, eat all the whitespace on the end of a string */

char *kw(char *in)
{
    /* bounds checking */
    if(strlen(in)==0)
        return(in);

    while(isspace(in[strlen(in)-1]))
    {
        /* bounds checking */
        if(strlen(in)==0)
            return(in);

        in[strlen(in)-1]=0x00;
    }

    return(in);
}

char **getList(char *filename)
{
    FILE *f;
    char **ret;
    char buf[1024];
    int index=0, size=4;

    if( (f=fopen(filename, "r"))==NULL)
    {
	perror(filename);
	exit(1);
    }

    ret=(void *)malloc(size * sizeof(char *));

    for(;;)
    {
	fgets(buf, 1024, f);
	if(feof(f))
	    break;

	if(index==size-1)
	{
	    size<<=1;

	    ret=realloc(ret, size*sizeof(char **));
	}

	kw(buf);

	ret[index++]=strdup(buf);
    }

    ret[index]=NULL;
    return(ret);
}

void freeList(char **list)
{
    int i;

    for(i=0; list[i]!=NULL; i++)
	free(list[i]);

    free(list);
}
