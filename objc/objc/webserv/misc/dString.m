/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dString.m,v 1.3 1997/04/15 21:49:51 dustin Exp $
 */

#include <dString.h>
#include <dSocket.h>
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

-free
{
    if(size>0)
    free(string);
    return(self);
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

- (int) scompare :(const char *)nother
{
    return(strcmp(string, nother));
}

- (int) compare :nother
{
    return (![nother scompare: string]);
}

- (int) scompare :(const char *)nother len:(int)length
{
    return(strncmp(string, nother, length));
}

- (int) compare :nother len:(int)length
{
    return(![nother scompare :string len:length]);
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

-setto:(const char*)astring
{
    [self clear];
    [self append: astring];
}

-oappend: astring
{
    char *b;

    b=[astring dup];
    [self append :b];
    free(b);
    return self;
}

-append:(const char*)astring
{
    int i;

    i=[self length];
    i+=strlen(astring);
    i+=1;

    /* make it large enough to hold it */
    while(size<i)
	size+=inc;

    string=(char *)realloc(string, size*sizeof(char));
    strcat(string, astring);
    return self;
}

-kw
{
    if([self length]==0)
	return(self);

    while(isspace(string[ strlen(string) -1]))
    {
	if(strlen(string)==0)
	    return(self);

	string[strlen(string)-1]=0x00;
    }

    return(self);
}

- (char *) dup
{
    return(strdup(string));
}

- (int) readfile: (const char *)filename
{
    int f, s;
    char buf[1024];

    f=open(filename, O_RDONLY, 0);
    if(f<0)
    {
	perror(filename);
	return -1;
    }

    for(;;)
    {
	s=read(f, buf, 1023);
	printf("Got %d bytes\n", s);
	buf[s]=0x00;
        [self append:buf];
	if(s<1023)
	    break;
    }
    close(f);

    return 0;
}

@end
