/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dWeb.m,v 1.2 1997/04/15 21:49:52 dustin Exp $
 */

#define IWANTMETHODNAMES 1

#include <dWeb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

@implementation dRequest

-init
{
    [super init];
    request=[[dString alloc] init];
    requestpath=[[dString alloc] init];
    [self clear];
    return self;
}

-clear
{
    special=0;
    method=-1;
    docnum=-1;
    [request clear];
    [requestpath clear];
    return self;
}

- (int) isspecial
{
    return(special);
}

-setto:(const char*)astring
{
    [self clear];
    [request append: astring];
}

- (int) parse: string
{
    int i, j, tmp, start, finished=0;
    char *buf;

    if([string length] == 0)
        return(1);

    for(i=0; methodnames[i]!=NULL; i++)
    {
        if(( [string scompare :methodnames[i]
            len:strlen(methodnames[i]) ]) ==0)
        {
            printf("It's the %s method\n", methodnames[i]);

            buf=[string dup];

            method=i;
            start=strlen(methodnames[i])+1;
            tmp=strlen(buf);
            for(j=start; j<tmp; j++)
            {
                if(buf[j]==' ')
                {
                    buf[j]=0x00;
                    break;
                }
            }
            if(j==tmp)
            {
                version=0;
                finished=1;
            }
            else
            {
                version=1;
            }
            [request setto :buf+start];
            free(buf);
            break;
        }
    }
    return(finished);
}

- (int) length
{
    return [request length];
}

- (int) verify;
{
    id buf=[[dString alloc] init];
    char *b;  // quick and dirty string manipulations

    if(1) // Reserving space
    {
	[buf setto: WEBROOT];
	[buf oappend: request];
	b=[buf dup];

	if(b[strlen(b)-1] == '/')
	    [buf append: "index.html"];

	free(b);
	b=[buf dup];

	if(access(b, R_OK)==0)
	{
	   special=0;
	   [requestpath setto :b];
	   free(b);
	   return(0);
	}
	else
	{
	   free(b);
	   return(1);
	}
    }
}

- print;
{
    if([request length]>0)
    {
        printf("Request:  ");
        [request print];
	printf("Path:     ");
	[requestpath print];
    }

    printf("Version:  %d\n", version);
    printf("Method:   %s\n", methodnames[method]);
    printf("Special:  %d\n", special);
    printf("Docnum:   %d\n", docnum);

    return self;
}

- showdoc :s
{
    char *b;
    id string=[[dString alloc] init];

    b=[requestpath dup];
    [string readfile :b];
    free(b);

    b=[string dup];
    [string print];

    send([s getsocket], b, [string length], 0);

    free(b);
    return self;
}

@end
