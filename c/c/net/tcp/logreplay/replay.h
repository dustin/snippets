/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: replay.h,v 1.3 1998/12/07 19:51:53 dustin Exp $
 */

#ifndef REPLAY_H
#define REPLAY_H

/* Debug stuff */
#ifndef PDEBUG
#define PDEBUG 1
#endif

#if (PDEBUG>0)
#ifndef _ndebug
#define _ndebug(a, b) if(_debug > a ) printf b;
#endif
#endif

/* In case it didn't make it */
#ifndef _ndebug
#define _ndebug(a, b)
#endif

#define REQ_LEN 1024

/*
 * URL request holder.
 */
struct url {
	char   *host;
	int     port;
	char   *req;
	char    httpreq[REQ_LEN];
	int     ssl;
};

/*
 * Status return.
 */
struct status {
	int     status;
	int     bytesread;
	char   *message;
};

/*
 * Things that hold log entries
 */
struct log_entry {
	int timeoffset;
	char *IP;
	char *request;
};

struct growstring {
	size_t size;
	char *string;
};

struct http_status {
	int status;
	char *string;
};

int     getclientsocket(char *host, int port);
int		getlog(FILE *f, struct log_entry *log);
int		str_append(struct growstring *s, char *buf);

#endif
