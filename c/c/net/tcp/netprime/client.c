/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: client.c,v 1.1 1997/06/29 23:26:19 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>

#include "netprime.h"

void timeout(void)
{
    fputs("Connection timed out.\n", stderr);
    exit(1);
}

int openhost(void)
{
struct hostent *hp;
register int s;
int flag;
struct linger l;
struct sockaddr_in sin;

    if((hp=gethostbyname(REMHOST)) == NULL)
    {
#ifdef HAVE_HERROR
        herror("gethostbyname");
#else
        fprintf(stderr, "Error looking up %s\n", REMHOST);
#endif
        exit(1);
    }

    if((s=socket(AF_INET, SOCK_STREAM, 0))<0)
    {
        perror("socket");
        exit(1);
    }

    sin.sin_family = AF_INET;
    sin.sin_port=htons(PRIME_PORT);
    bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

    l.l_onoff  = 1;
    l.l_linger = 60;
    setsockopt(s, SOL_SOCKET, SO_LINGER, (char *)&l, sizeof(l));

    flag=1;
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char *)&flag,
	sizeof(int));

    if(connect(s, (struct sockaddr *)&sin, sizeof(sin))<0)
    {
        perror("connect");
        exit(1);
    }

    return(s);
}

char *ckw(char *in)
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

void cgettext(char *message, int size)
{
    fgets(message, size, stdin);
    ckw(message);
}

void main(void)
{
    int s, i;
    char buf[BUFLEN];

    s=openhost();

    send(s, "generate\r\n", strlen("generate\r\n"), 0);

    for(i=0; i<NGEN; i++)
    {
	gettextcr(s, buf);
	puts(buf);
    }
}
