/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dString.m,v 1.1 1997/04/14 22:12:48 dustin Exp $
 */

#include <dString.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

@implementation dString

-init
{
    [super init];
    size=0;
    inc=128;
    [self clear];
    return self;
}

-init:(int)s;
{
    [super init];
    size=0;
    inc=s;
    [self clear];
    return self;
}

-clear
{
    if(size>0)
        free(string);
    size=inc;
    string=(char *)malloc(size*sizeof(char));
    string[0]=(char)NULL;
    return self;
}

- lowercase
{
    int i;

    for(i=[self length]-1; i>=0 ; i--)
    {
	string[i]=tolower(string[i]);
    }
    return self;
}

- uppercase
{
    int i;

    for(i=[self length]-1; i>=0 ; i--)
    {
	string[i]=toupper(string[i]);
    }
    return self;
}

-print
{
    puts(string);
    return self;
}

- (int) length
{
    int l;
    if(size>0)
	l=strlen(string);
    else
	l=0;

    return(l);
}

-appendString:(const char*)astring
{
    int i;

    i=[self length];
    i+=strlen(astring);
    i+=1;

    while(size<i)
    {
	printf("Had %d bytes, needed %d...\n", size, i);
	size+=inc;
    }

    string=(char *)realloc(string, size*sizeof(char));
    strcat(string, astring);
    return self;
}

@end
