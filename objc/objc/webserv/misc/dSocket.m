/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dSocket.m,v 1.2 1997/04/15 21:49:49 dustin Exp $
 */

#include <dSocket.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

@implementation dSocket

-init;
{
    [super init];
    listening=0;
    s=-1;
    return self;
}

-clear;
{
    close(s);
    [self init];
    return self;
}

- (int) islistening
{
    return(listening);
}

- listento: (int) p
{
    int reuse=1;
    struct sockaddr_in sin;

    port=p;

    if((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
	perror("server: socket");
	exit(1);
    }

    memset((char *) &sin, 0x00, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = htons(port);
    sin.sin_addr.s_addr = htonl(INADDR_ANY);

    setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
	(char *)&reuse, sizeof(int));

    reuse=1;
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY,
	(char *)&reuse, sizeof(int));

    if( bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0)
    {
	perror("server: bind");
	exit(1);
    }

    if(listen(s, 5) < 0)
    {
	perror("server: listen");
	exit(1);
    }

    return self;
}

- (int) getsocket
{
    return(s);
}

- (int) write: (char *)buf;
{
    return(send(s, buf, strlen(buf), 0));
}

- read
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

-accept
{
    id ns=[[dSocket alloc] init];
    int so, fromlen;
    struct sockaddr_in fsin;

    fromlen=sizeof(struct sockaddr_in);
    so=accept(s, (struct sockaddr *)&fsin, &fromlen);

    if(so>=0)
	[ns setsocket: so];

    return ns;
}

-setsocket: (int)so
{
    s=so;
    return self;
}

@end
