/*
 * Check Webserver Status
 * Copyright (c) 1997 SPY Internetworking
 *
 * $Id: checkweb.c,v 1.5 1998/04/20 17:54:15 dustin Exp $
 * $Source: /Users/dustin/stuff/cvstest/c/net/tcp/checkweb.c,v $
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <assert.h>
#include <ctype.h>

/*
 * It was this or a global variable.  It'll probably change in the future
 */
#define HTMLOUT 1

/*
 * URL request holder.
 */
struct url {
    char *host;
    int port;
    char *req;
};

/*
 * Status return.
 */
struct status {
    int status;
    int bytesread;
    char *message;
};

/* Kill Whitey(tm) */
#define iswhitey(a) (a=='\n' || a=='\r')

/*
 * Open and return a tcp socket to host:port, -1 if it fails.
 */

int openhost(char *host, int port)
{
  struct hostent *hp;
  register int s;
  struct sockaddr_in sin;

  if ((hp = gethostbyname(host)) == NULL)
    {
      printf("ERR: gethostbyname\n");
      return(-1);
    }

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
      perror("socket");
      return(-1);
    }

  sin.sin_family = AF_INET;
  sin.sin_port = htons(port);
  bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

  alarm(30);
  if(connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0)
      return(-1);
  alarm(0);

  return (s);
}

/* What to do when a connection times out.  */
void timeout(int c)
{
    alarm(0);
    signal(SIGALRM, timeout);
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

    /* We only do http urls */
    if(strncmp(url, "http://", 7) != 0)
	return(u);

    /* Host is the first thing, after http:// */
    u.host=strdup(url+7);

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
            port=80;
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
            port=80;
            tmp=u.req;
            u.req=strdup(u.req);
            *tmp=NULL;
            break;
    }

    u.port=port;
    return(u);
}

/*
 * Free any data that might be in a struct url
 */
void freeurl(struct url u)
{
    if(u.host)
	free(u.host);
    if(u.req)
	free(u.req);
}

/*
 * Free any data that might be in a struct status
 */
void freestatus(struct status s)
{
    if(s.message)
	free(s.message);
}

/*
 * Accept a url string, return a struct status.
 *
 * A negative status indicates a problem either connecting to the
 * machine, or a url parse problem.  The message will tell you what,
 * specifically happened (although it doesn't distinguish between a
 * timeout, and a connection refused).
 */
struct status getstatus(char *url)
{
    int s, i;
    char line[1024];
    char *p, *q;
    struct url u;
    struct status st;

    st.status=-1;
    st.message=NULL;
    st.bytesread=0;

    u=parseurl(url);
    if(u.port==-1)
    {
	st.message=strdup("Invalid url request format");
	return(st);
    }

    s=openhost(u.host, u.port);

    if(s<0)
    {
	st.message=strdup("Could not connect to host");
	return(st);
    }

    send(s, "GET ", 4, 0);
    send(s, u.req, strlen(u.req), 0);
    send(s, " HTTP/1.0\n\n", 11, 0);

    alarm(120);
    i=recv(s, line, 1024, 0);
    alarm(0);
    if(i<1)
    {
	st.message=strdup("Timeout, or nothing returned.");
	return(st);
    }
    line[i]=NULL;

    /*
     * My keen parsing techniques, flip through it with a pointer
     * to get the status number
     */
    p=&line[0];
    while(*p++ && *p!=' ');
    st.status=atoi(p);

    /* Now we want the status message */
    while(*++p && *p!=' ');

    /* Kill Whitey */
    q=p;
    while(*++q && !iswhitey(*q));
    *q=NULL;
    st.message=strdup(p+1);

    /* Eat the rest of the page */
    while(i=recv(s, line, 1024, 0))
	line[i]=NULL;

    close(s);
    freeurl(u);
    return(st);
}

/*
 * Print a status struct
 */
void printstatus(char *url, struct status st)
{
#if HTMLOUT
    printf("\t<li><font color=\"#%s\">%d</font> -- "
	   "<a href=\"%s\">%s</a></li>\n",
	   st.status==200 ? "007f00" : "ff0000",
	   st.status,
	   url, url);
#else
    printf("%s:  %d (%s)\n", url, st.status, st.message);
#endif
}

/*
 * This is the function that reads in a # commented file and prints the
 * status report for every url found in it.  The file should be in the
 * format of one url per line, blank lines and lines beginning with # are
 * ignored.
 */
void dofile(char *filename)
{
    FILE *f;
    char line[8192];
    struct status st;

    f=fopen(filename, "r");
    if(!f)
    {
	perror(filename);
	exit(1);
    }

    while(fgets(line, 8192, f))
    {
	/* KILL WHITEY! */
	while(*line && isspace(line[strlen(line)-1]))
	    line[strlen(line)-1]=NULL;

	/* Ignore blank lines and lines beginning with # */
	if( ! (line[0] == NULL || line[0] == '#'))
	{
	    st=getstatus(line);
	    printstatus(line, st);
	    freestatus(st);
	}
    }

    fclose(f);
}

void main(int argc, char **argv)
{
    struct status st;

    signal(SIGPIPE, SIG_IGN);
    signal(SIGALRM, timeout);

    if(argc < 2)
    {
	printf("checkweb, copyright (c) 1997  Dustin Sallings\n"
	    "$Id: checkweb.c,v 1.5 1998/04/20 17:54:15 dustin Exp $\n");
	printf("Error, argument required.  Usage:\n%s filename\n"
	       "Where filename is the file containing the url list.\n",
	       argv[0]);
	exit(0);
    }
    dofile(argv[1]);
}
