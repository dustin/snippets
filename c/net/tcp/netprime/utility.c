/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: utility.c,v 1.1 1997/06/29 23:26:19 dustin Exp $
 * $State: Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/socket.h>

#include "netprime.h"

/* kill whitey, eat all the whitespace on the end of a string */

int gettext(int s, char *buf)
{
    int size;
    if( (size=read(s, buf, BUFLEN-1)) >0)
    {
        buf[size]=0x00;
        kw(buf);
        return(size);
    }
    else
    {
        /* Pipe breaking bastard */
        exit(0);
    }

    return(size);
}

int gettextcr(int s, char *buf)
{
    int size=1, len=0;

    /* eat any extra CR's and LF's */
    while( (len=read(s, buf, 1)) >0)
    {
	if(buf[size-1]=='\r')
	{
	    size=0;
	    break;
	}
	else if(buf[size-1]=='\n')
	{
	    size=0;
            break;
	}
	else
	{
	    break;
	}
    }

    /*
    {
        if(buf[size-1]!='\r' && buf[size-1]!='\n')
            break;
    }
    */

    if(len==0)
    {
	exit(0);
    }

    while( (len=read(s, buf+size, 1)) >0)
    {
        if(len==0)
        {
	    exit(0);
        }

	size+=len;
        buf[size]=0x00;
        if(buf[size-1]=='\r' || buf[size-1]=='\n')
            break;
    }

    kw(buf);


    return(size);
}


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

int f_exists(char *file)
{
    return(access(file, F_OK)==0);
}

int bit_set(int map, int bit)
{
    map>>=bit;
    map&=1;

    return(map);
}

int set_bit(int map, int bit)
{
    int blah;

    blah=1;
    blah<<=bit;

    map|=blah;

    return(map);
}
