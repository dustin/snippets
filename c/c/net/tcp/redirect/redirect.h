/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: redirect.h,v 1.4 1998/01/05 00:15:33 dustin Exp $
 */

#ifndef REDIRECT_H
#define REDIRECT_H

#define CONFFILE "redir.conf"
#define DEFPIDFILE "/tmp/redirect.pid"
#define DEFCONTIME 10 /* Ten seconds to connect */

#define MAPSIZE 1024
#define BUFLEN 1024

/* PID returns */
#define  PID_NOFILE 1
#define  PID_STALE  2
#define  PID_ACTIVE 3

/* Debug stuff */
#ifndef PDEBUG
#define PDEBUG 1
#endif

#if (PDEBUG>0)
# ifndef _ndebug
#  define _ndebug(a, b) if(_debug > a ) printf b;
# endif
#endif

/* In case it didn't make it */
#ifndef _ndebug
#define _ndebug(a, b)
#endif

struct cluster {
    char *hostname;
    int port;
    int tcptimeout;
};

struct cluster **getcluster(char *p, int stats);
void freeCluster(struct cluster **c);
int getclientsocket(char *host, int port);
int getservsocket(char *host, int port);
void resettraps(void);

#endif
