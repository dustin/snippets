/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: httpd.m,v 1.1 1997/04/15 06:09:17 dustin Exp $
 */

#include <dString.h>
#include <dSocket.h>
#include <dWeb.h>

#include <stdio.h>
#include <stdlib.h>

void main(void)
{
    id socket=[[[dSocket alloc] init] listento: 8080];
    id ns, req;
    id string;

    req=[[dRequest alloc] init];

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
    else
    {
        puts("Problem in houston...");
    }

    [req print];

    puts("done");
}
