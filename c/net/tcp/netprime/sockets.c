/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.1 1997/06/29 23:26:19 dustin Exp $
 * $State: Exp $
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

extern struct config conf;

int getservsocket(int port)
{
    int reuse=1, s, flag;
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

    flag=1;
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(int));

    return(s);
}
