/*
 * Copyright 1996 SPY Internetworking
 *
 * $Id: libcgi.c,v 1.1 1997/08/26 06:00:11 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <malloc.h>
#include "cgi.h"

#ifdef DEBUG
void hexdump(char *what)
{
    int i;

    for(i=0; i<strlen(what); i++)
	printf("%x ", what[i]);

    putchar('\n');
}
#endif

void cgiprintdata(struct cgiform *d)
{
    struct cgiform *tmp=d->next;

    while(tmp != NULL)
    {
	printf("%s\t= %s\n", tmp->name, tmp->value);
	tmp=tmp->next;
    }
}

char *cgigetdata(struct cgiform *d, char *n)
{
    struct cgiform *tmp=d->next;

    while(tmp != NULL)
    {
	if( (strcmp(n, tmp->name)) == 0)
	{
#ifdef DEBUG
            printf("Found your data, value is `%s' size is %d\n", tmp->value,
		strlen(tmp->value));
#endif
	    return(tmp->value);
	}

	tmp=tmp->next;
    }
    return(NULL);
}

void cgifree(struct cgiform *d)
{
    if(d->next != NULL)
        cgifree(d->next);

    free(d->name);
    free(d->value);
    free(d);
}
