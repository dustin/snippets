/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dWeb.m,v 1.1 1997/04/15 06:11:45 dustin Exp $
 */

#define IWANTMETHODNAMES 1

#include <dWeb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

@implementation dRequest

-init
{
    [super init];
    request=[[dString alloc] init];
    [self clear];
    return self;
}

-clear
{
    special=0;
    method=-1;
    docnum=-1;
    [request clear];
    return self;
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
        if(( [string sncompare :methodnames[i]
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

- print;
{
    if([request length]>0)
    {
        printf("Request:  ");
        [request print];
    }

    printf("Version:  %d\n", version);
    printf("Method:   %s\n", methodnames[method]);
    printf("Special:  %d\n", special);
    printf("Docnum:   %d\n", docnum);

    return self;
}

@end
