/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 *
 * $Id: httplog.h,v 1.2 2001/02/25 11:03:40 dustin Exp $
 */

#ifndef HTTPLOG_H
#define HTTPLOG_H 1

#define CONNECTION_UNDEFINED 0
#define CONNECTION_CLOSE 1
#define CONNECTION_KEEPALIVE 2

#define LENGTH_CALCULATE 0
#define LENGTH_BODYCHUNK 1
#define LENGTH_HEADER 2

#define ENCODING_UNDEFINED 0
#define ENCODING_CHUNKED 1

/*
 * HTTP response stuff.
 */
struct http_response {
	char *status_str; /* The status string */
	int status;		/* HTTP status */
	int header_len;	/* Length of the header */
	int connection;	/* CONNECTION_CLOSE or CONNECTION_KEEPALIVE */
	int trans_enc;	/* Transfer encoding */
	int body_len;	/* Length, as found in the header */
	int length;		/* Calculated length */
	int remaining;	/* Remaining length in the current block */
};

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
	char *extra; /* Extra data */
	struct http_response response;
};

#endif /* HTTPLOG_H */
