/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.5 1998/06/19 18:30:17 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <syslog.h>
#include <netinet/tcp.h>

#include <redirect.h>
#include <readconfig.h>

extern struct confType *cf;
extern int _debug;

int getclientsocket(char *host, int port)
{
    struct hostent *hp;
    int success, i, flag;
    static unsigned int btdt=0, sourceaddr=0;
    register int s=-1;
    struct linger l;
    struct sockaddr_in sin, sout;
    char *p;

    /* grab outgoing address */

    if(btdt==0) {
        btdt=1;
        p=rcfg_lookup(cf, "etc.sourceaddr");
        if(p) {
	    if( (hp=gethostbyname(p)) == NULL) {
#ifdef HAVE_HERROR
                herror("gethostbyname");
#else
                fprintf(stderr, "Error looking up %s\n", p);
#endif
                sourceaddr=0;
	    } else {
	        memcpy(&sourceaddr, hp->h_addr, hp->h_length);
	    }
        }
    }

    _ndebug(2, ("Building client socket for %s:%d\n", host, port));

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
            puts("Nagle algorithm not disabled.");
        }

	if(sourceaddr>0) {
	    memset(&sout, 0x00, sizeof(struct sockaddr_in));
	    sout.sin_family = AF_INET;
	    memcpy(&sout.sin_addr, &sourceaddr, 4);
	    sout.sin_port = 0;

	    if(bind(s, (struct sockaddr *)&sout, sizeof(struct sockaddr_in)) <0)
	    {
		perror("client source bind");
	    }
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

int getservsocket(char *host, int port)
{
    int reuse=1, s;
    unsigned int bindaddr;
    struct sockaddr_in sin;
    struct hostent *hp;

    _ndebug(2, ("Building server socket for %s:%d\n", host, port));

    signal(SIGPIPE, SIG_IGN);

    if((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        perror("server: socket");
        return(-1);
    }

    if(host==NULL)
    {
	 _ndebug(2, ("Binding to INADDR_ANY\n"));
	 bindaddr=INADDR_ANY;
    }
    else
    {
	_ndebug(2, ("Binding to %s\n", host));
        if((hp=gethostbyname(host)) == NULL)
        {
#ifdef HAVE_HERROR
            herror(host);
#else
            fprintf(stderr, "Error looking up %s\n", host);
#endif
            return(-1);
        }

	memcpy(&bindaddr, hp->h_addr, hp->h_length);
    }

    memset((char *) &sin, 0x00, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = htons(port);
    sin.sin_addr.s_addr = bindaddr;

    setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
        (char *)&reuse, sizeof(int));

    if( bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0)
    {
        perror("server: bind");
        return(-1);
    }

    if(listen(s, 5) < 0)
    {
        perror("server: listen");
        return(-1);
    }

    return(s);
}
