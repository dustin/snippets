/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dCSocket.m,v 1.1 1998/03/17 04:04:56 dustin Exp $
 */

#include <config.h>
#include <snppclient.h>

#include <dCSocket.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

@implementation dCSocket

-init;
{
    [super init];
    s=-1;
    return self;
}

-clear;
{
    close(s);
    [self init];
    return self;
}

- (int) isconnected
{
    return(1);
}

- connectTo: (char *)host :(int)p
{
    struct hostent *hp;
    int success, i, flag;
    struct linger l;
    struct sockaddr_in sin;

    _ndebug(2, ("Building client socket for %s:%d\n", host, p));

    if(host==NULL || p==0)
        return(self);

    if((hp=gethostbyname(host)) == NULL)
    {
#ifdef HAVE_HERROR
        herror("gethostbyname");
#else
        fprintf(stderr, "Error looking up %s\n", host);
#endif
        return(self);
    }

    success=0;

    /* of course, replace that 1 with the max number of con attempts */
    for(i=0; i<1; i++)
    {
        if((s=socket(AF_INET, SOCK_STREAM, 0))<0)
        {
            perror("socket");
            return(self);
        }

        sin.sin_family = AF_INET;
        sin.sin_port=htons(p);
        memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);

        l.l_onoff  = 1;
        l.l_linger = 60;
        setsockopt(s, SOL_SOCKET, SO_LINGER, (char *)&l, sizeof(l));

        flag=1;
        if (setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char *)&flag,
            sizeof(int)) <0)
        {
            puts("Nagle algorithm not dislabled.");
        }

        if(connect(s, (struct sockaddr *)&sin, sizeof(sin))<0)
        {
            sleep(1);
        }
        else
        {
            success=1;
            break;
        }
    }

    if(!success)
       s=-1;

    return(self);
}

- (int) getsocket
{
    return(s);
}

- (int) write: (char *)buf;
{
    return(send(s, buf, strlen(buf), 0));
}

- readline
{
    int size=1, len=0;
    char buf[2]={0x00, 0x00};
    id string=[[dString alloc] init: 1024];

    while( (len=recv(s, buf, 1, 0)) >0)
    {
	if(buf[0]=='\r' || buf[0]=='\n')
	{
	    size=0;
	    break;
	}
	else
	{
	    break;
	}
    }

    if(len==0)
	return [string clear];

    if(size>0)
	[string append: buf];

    while( (len=recv(s, buf, 1, 0)) >0)
    {
	if(len==0)
	{
	    return [string clear];
	}

	buf[1]=0x00;
	if(buf[0]=='\r' || buf[0]=='\n')
	    break;

	[string append: buf];
	size+=len;
    }

    [string kw];

    return(string);
}

-setsocket: (int)so
{
    s=so;
    return self;
}

@end
