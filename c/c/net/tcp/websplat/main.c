/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.2 1998/06/27 04:09:23 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <assert.h>

#include <splat.h>

static RETSIGTYPE serv_conn_alrm(int sig);

int _debug=0;

#define MAXSEL 1024

static void resettraps(void)
{
    signal(SIGALRM, serv_conn_alrm);
}

static RETSIGTYPE serv_conn_alrm(int sig)
{
    resettraps();
    return;
}

/*
 * Parse a url string into a struct url;
 * port is -1, and host and req are NULL if it fails.
 */
struct url parseurl(char *url)
{
    char *tmp;
    struct url u;
    int i, port;

    u.host=NULL;
    u.req=NULL;
    u.port=-1;

    /* We only do http and maybe https urls */
    if(strncmp(url, "http://", 7) == 0) {
        u.ssl=0;
    } else {
#ifdef USE_SSLEAY
        if(strncmp(url, "https://", 7) == 0)
            u.ssl=1;
        else
#endif
            return(u);
    }

    /* Host is the first thing, after http:// or https:// */
    u.host=strdup(url+7+u.ssl);

    /*
     * OK, request comes along eventually, so let's mark it as host, and
     * go from there.  Look for a :, /, or NULL to let us know we're done
     * with host.
     */
    u.req=u.host;
    while(*u.req && *u.req!=':' && *u.req!='/')
        u.req++;

    /*
     * Let's see which one we got.
     */
    switch(*u.req)
    {
        case NULL:                /* format http://host.domain.com */
            u.req=strdup("/");
            port=(u.ssl?443:80);
            break;
        case ':':                 /* format http://host.domain:port/ */
            port=atoi(u.req+1);
            assert(port);
            *u.req=NULL;
            u.req++;
            while(*u.req && *u.req!='/')
                u.req++;
            u.req=*u.req?strdup(u.req) : "/";
            break;
        case '/':                 /* format http://host.domain.com/ */
            port=(u.ssl?443:80);
            tmp=u.req;
            u.req=strdup(u.req);
            *tmp=NULL;
            break;
    }

    assert( strlen(u.req) < (REQ_LEN - 16)); /* TMI */
    strcpy(u.httpreq, "GET ");
    strcat(u.httpreq, u.req);
    strcat(u.httpreq, " HTTP/1.0\n\n");

    u.port=port;
    return(u);
}

void usage(char **argv)
{
    printf("Usage:  %s -r request_url\n", argv[0]);
}

int main(int argc, char **argv)
{
    int s, selected, size, c, i;
    fd_set fdset, tfdset;
    char buf[8192];
    struct url req;
    struct timeval t;

    req.port=-1;

    while( (c=getopt(argc, argv, "r:d")) >=0) {
	switch(c) {
	    case 'r':
		req=parseurl(optarg);
		if(req.port==-1) {
		    printf("Invalid URL format:  %s\n", optarg);
		    return(1); /* I don't like exit */
		}
	        break;

	    case 'd':
		_debug=3;
		break;

	    case '?':
	        usage(argv);
	        return(1);     /* I still don't like exit */
	        break;
	}
    }

    if(req.port==-1) {
	printf("Nothing to do\n");
	usage(argv);
	return(1);
    }

    printf("Host:  %s\nPort:  %d\nFile:  %s\n", req.host, req.port,
           req.req);

    FD_ZERO(&tfdset);

    resettraps();

    for(;;)
    {
        s=getclientsocket(req.host, req.port);
	if(s>0) {
	    printf("Got one: %d...\n", s);
	    FD_SET(s, &tfdset);
	}

	/* Sending */
	i=send(s, req.httpreq, strlen(req.httpreq), 0);
	printf("Sent %d out of %d bytes\n", i, strlen(req.httpreq));

	fdset=tfdset;
	t.tv_sec=0;
	t.tv_usec=2600;

	_ndebug(2, ("Selecting...\n"));

        if( (selected=select(MAXSEL, &fdset, NULL, NULL, &t)) > 0)
	{
	    int i;
	    for(i=0; i<MAXSEL; i++)
	    {
		if(FD_ISSET(i, &fdset))
		{
		    _ndebug(3, ("Caught %d\n", i));
		    selected--;
		    size=recv(i, buf, 8192, 0);
		    if(size==0)
		    {
			_ndebug(2, ("Lost %d\n", i));
			close(i);
			FD_CLR(i, &fdset);
		    }
		    else
		    {
			_ndebug(2, ("Got %d bytes from %d\n", size, i));
		    }
		}

		if(selected==0)
		    break;
	    }
	} /* select */
    } /* infinite loop */
}
