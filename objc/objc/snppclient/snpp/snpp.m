/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: snpp.m,v 1.1 1998/03/17 04:04:56 dustin Exp $
 */

#include <dString.h>
#include <dSocket.h>
#include <dWeb.h>
#include <utility.h>

#include <stdio.h>
#include <stdlib.h>

void main(void)
{
    id socket=[[[dSocket alloc] init] listento: 8088];
    id ns, req;
    id string;

    req=[[dRequest alloc] init];

    for(;;)
    {
    ns=[socket accept];
    if([ns getsocket]>=0)
    {
        puts("I got a connection!!!!");
        string=[ns read];
        [string print];
        while([req parse :string]==0)
        {
	    [string free];
            string=[ns read];
	    [string print];
        }
    }

    if([req verify]==0)
    {
	puts("Request was valid.");
	puts("Doing showdoc");
	[req showdoc :ns];
	puts("Done with showdoc");
    }
    else
    {
	puts("Request was invalid.");
    }

    [req print];

    puts("done");
    [ns clear];
    }
}
