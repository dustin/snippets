/* $Id: protocol.h,v 1.1 1998/01/15 09:17:57 dustin Exp $ */

#define BIGBROTHER "bigbrother.west.spy.net"

#define MAXCHARSIZE 1000
#define PROTVER 3

#ifdef __svr4__
#define herror perror
#define getloadavg(x,y) 0
#endif

#ifndef UTMP_FILE
#define UTMP_FILE _PATH_UTMP
#endif

#ifdef DEBUG
#define SLEEPTIME 3
#else
#define SLEEPTIME 30
#endif

struct people {
    char login[8];
    char tty[8];
    char host[16];
    time_t time;
    time_t idle;
} ;

#define NUSERS (1000 / sizeof(struct people))

struct sysinfo {
	int version;
	int cmd;
	int users;
	int load[3];
	struct people user[NUSERS];
} ;

#define INFOSIZE sizeof(struct sysinfo)
