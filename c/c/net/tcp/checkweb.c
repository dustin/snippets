/*
 * Check Webserver Status
 * Copyright (c) 1997 SPY Internetworking
 *
 * $Id: checkweb.c,v 1.1 1997/12/19 05:49:55 dustin Exp $
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

#define HTMLOUT 0

struct url {
    char *host;
    int port;
    char *req;
};

struct status {
    int status;
    char *message;
};

#define iswhitey(a) (a=='\n' || a=='\r')

int openhost(char *host, int port)
{
  struct hostent *hp;
  register int s;
  struct sockaddr_in sin;

  if ((hp = gethostbyname(host)) == NULL)
    {
      printf("ERR: gethostbyname\n");
      exit(1);
    }

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
      perror("socket");
      exit(1);
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

void timeout(int c)
{
    alarm(0);
    signal(SIGALRM, timeout);
    return;
}

struct url parseurl(char *url)
{
    char *host, *req, *tmp;
    struct url u;
    int i, port;

    u.host=NULL;
    u.req=NULL;
    u.port=-1;

    if(strncmp(url, "http://", 7) != 0)
    {
	return(u);
    }

    host=url+7;
    req=host;
    while(*req && *req!=':' && *req!='/')
        req++;

    switch(*req)
    {
        case NULL:
            req=strdup("/");
            port=80;
            break;
        case ':':
            port=atoi(req+1);
            assert(port);
            *req=NULL;
            req++;
            while(*req && *req!='/')
                req++;
            req=*req?strdup(req) : "/";
            break;
        case '/':
            port=80;
            tmp=req;
            req=strdup(req);
            *tmp=NULL;
            break;
    }

    u.host=host;
    u.port=port;
    u.req=req;
    return(u);
}

void freeurl(struct url u)
{
    if(u.host)
	free(u.host);
    if(u.req)
	free(u.req);
}

void freestatus(struct status s)
{
    if(s.message)
	free(s.message);
}

struct status getstatus(char *url)
{
    int s, i;
    char line[1024];
    char *p, *q;
    struct url u;
    struct status st;

    st.status=-1;
    st.message=NULL;

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

    i=recv(s, line, 1024, 0);
    assert(i);
    line[i]=NULL;

    p=&line[0];

    while(*p++ && *p!=' ');

    st.status=atoi(p);

    while(*++p && *p!=' ');

    q=p;
    while(*++q && !iswhitey(*q));
    *q=NULL;

    st.message=strdup(p+1);

    while(i=recv(s, line, 1024, 0))
    {
	line[i]=NULL;
    }
    close(s);
    freeurl(u);
    return(st);
}

void main(int argc, char **argv)
{
    struct status st;

    signal(SIGPIPE, SIG_IGN);
    signal(SIGALRM, timeout);

    st=getstatus(argv[1]);

#if HTMLOUT
    printf("Status is <color tag>%d</color>\n", st.status);
#else
    printf("Status was:  %d (%s)\n", st.status, st.message);
#endif
    freestatus(st);
}
