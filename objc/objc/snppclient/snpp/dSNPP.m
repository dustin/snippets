/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dSNPP.m,v 1.2 1998/04/16 20:02:22 dustin Exp $
 */

#include <config.h>
#include <snppclient.h>

#include <dSNPP.h>
#include <stdio.h>
#include <stdlib.h>

@implementation dSNPP

-init;
{
    [super init];
    sock=[dCSocket alloc];
    return self;
}

-clear;
{
    [sock clear];
    status=0;
    return(self);
}

-(int)connectTo: (char *)host :(int)port;
{
    id string;
    [sock connectTo :host :port];
    string=[sock readline];
    [self getStatus :string];
    return(0);
}

-(int)status;
{
    return(status);
}

-getStatus: string;
{
     char *what;
     status=0;

     what=[string dup];
     if(what && strlen(what))
	 sscanf(what, "%d", &status);

     free(what);
     return(self);
}

-protoSend :(char *)cmd :(char *)message;
{
    char *what;
    id str;

    str=[[dString alloc] init];

    [str setto:cmd];
    [str append:" "];
    [str append:message];
    [str append:"\n"];

    what=[str dup];
    [sock write:what];
    free(what);

    [str clear];
    str=[sock readline];
    [self getStatus :str];
    return(self);
}

-protoSend :(char *)cmd;
{
    char *what;
    id str;

    str=[[dString alloc] init];

    [str setto:cmd];
    [str append:"\n"];

    what=[str dup];
    [sock write:what];
    free(what);

    [str clear];
    str=[sock readline];
    [self getStatus :str];
    return(self);
}

-(int)sendAPage :(char *)to thatsays:(char *)message priority:(char *)pri;
{
    [self protoSend: "priority" :pri];
    if( ([self status]<200) || ([self status]>299 ) )
        return(-1);
    return([self sendAPage :to thatsays:message]);
}

-(int)sendAPage :(char *)to thatsays:(char *)message;
{
    [self protoSend: "page" :to];
    if( ([self status]<200) || ([self status]>299 ) )
	return(-1);

    [self protoSend: "message" :message];
    if( ([self status]<200) || ([self status]>299 ) )
	return(-1);

    [self protoSend: "send"];
    if( ([self status]<200) || ([self status]>299 ) )
	return(-1);

    return(0);
}

-reset
{
    return([self protoSend: "reset"]);
}

-quit;
{
    [sock write :"QUIT\n"];
    [self clear];
}

@end
