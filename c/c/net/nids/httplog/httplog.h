/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 *
 * $Id: httplog.h,v 1.1 2001/02/25 01:08:58 dustin Exp $
 */

#ifndef HTTPLOG_H
#define HTTPLOG_H 1

/*
 * Persistent data for a connection.
 */
struct http_param {
	char *req;   /* The HTTP request */
	char *host;  /* The source address */
	char *vhost; /* The Host header */
	char *ref;   /* The referer */
	char *agent; /* The user agent */
	char *ts;    /* The user agent */
	int status;  /* The status */
	int start;   /* How far into the response the data starts */
	int length;  /* The response length */
	char *status_str;  /* The status string */
};

#endif /* HTTPLOG_H */
