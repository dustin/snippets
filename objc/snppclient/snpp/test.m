/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: test.m,v 1.3 1998/04/17 05:58:25 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <dSNPP.h>

void main(int argc, char **argv)
{
    id snpp;

    if(argc<2)
    {
	printf("Not enough arguments (whom do you want to page today?)\n");
	exit(0);
    }

    snpp=[[dSNPP alloc] init];
    [snpp connectTo :"pager" :1031];
    if([snpp sendAPage :argv[1]
               thatsays:"Hey, what's up? (from objc snpp)"]==0)
	printf("Page was successful\n");
    else
	printf("Page failed\n");
    [snpp reset];
    if([snpp sendAPage :argv[1]
	       thatsays:"Hey, what's up? (high, from objc snpp)"
               priority:"high"]==0)
	printf("Page was successful\n");
    else
	printf("Page failed\n");
    [snpp quit];
}
