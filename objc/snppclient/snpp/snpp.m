/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: snpp.m,v 1.2 1998/04/17 05:58:24 dustin Exp $
 */

#include <dString.h>
#include <dSocket.h>
#include <dWeb.h>
#include <utility.h>

#include <stdio.h>
#include <stdlib.h>

void main(void)
{
    id snpp;
    snpp=[[dSNPP alloc] init];
    [snpp connectTo :"pager" :1031];
    [snpp sendAPage :"dustin" thatsays:"Hey, what's up? (from objc snpp)"];
    [snpp quit];
}
