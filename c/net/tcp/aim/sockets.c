/*
 * Copyright (c) 1997-1999 Dustin Sallings
 *
 * $Id: sockets.c,v 1.1 1999/06/10 07:30:23 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <syslog.h>

int _aol_getservsocket(int port)
{
    int reuse=1, s;
    struct sockaddr_in sin;

    signal(SIGPIPE, SIG_IGN);

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

    return(s);
}

int _aol_getclientsocket(char *host, int port)
{
    struct hostent *hp;
    int success, i, flag;
    register int s=-1;
    struct linger l;
    struct sockaddr_in sin;

    if(host==NULL || port==0)
        return(-1);

    if((hp=gethostbyname(host)) == NULL)
    {
#ifdef HAVE_HERROR
        herror("gethostbyname");
#else
        fprintf(stderr, "Error looking up %s\n", host);
#endif
        return(-1);
    }

    success=0;

    /* of course, replace that 1 with the max number of con attempts */
    for(i=0; i<1; i++)
    {
        if((s=socket(AF_INET, SOCK_STREAM, 0))<0)
        {
            perror("socket");
            return(-1);
        }

        sin.sin_family = AF_INET;
        sin.sin_port=htons(port);
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

    return(s);
}
