/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <unistd.h>
#include <string.h>
#include <regex.h>
#include <time.h>
#include <err.h>
#include <libnet.h>
#include <nids.h>
#include <pcap.h>
#include <assert.h>

#include "httplog.h"
#include "mymalloc.h"

/*
 * This gives us an easy way to switch between server and client streams.
 */
#define SERVER_STREAM 1
#define CLIENT_STREAM 2

/* Free the guts of the http_param */
void partial_free_param(struct http_param *param)
{
	if(param->req)
		free(param->req);
	if(param->ref)
		free(param->ref);
	if(param->vhost)
		free(param->vhost);
	if(param->agent)
		free(param->agent);
	if(param->ts)
		free(param->ts);
	if(param->response.status_str)
		free(param->response.status_str);
	param->req=NULL;
	param->ref=NULL;
	param->vhost=NULL;
	param->agent=NULL;
	param->ts=NULL;
	param->response.status_str=NULL;
	param->response.status=0;
	param->response.header_len=0;
	param->response.connection=0;
	param->response.trans_enc=0;
	param->response.body_len=0;
	param->response.length=0;
	param->response.remaining=0;
}

/* Free all of the guts of the http_param */
void free_param(struct http_param *param)
{
	partial_free_param(param);
	if(param->extra)
		free(param->extra);
	if(param->host)
		free(param->host);
	param->extra=NULL;
	param->host=NULL;
}

/*
 * Return 1 if a request is OK.
 */
int ok_request(char *req)
{
	int rv=0;
	if(req!=NULL) {
		if(strncmp(req, "GET", 3)==0) {
			rv=1;
		} else if(strncmp(req, "POST", 4)==0) {
			rv=1;
		} else if(strncmp(req, "HEAD", 4)==0) {
			rv=1;
		}
	}
	return(rv);
}

void log_request(struct http_param *param)
{
	char *ref=NULL;
	char *host=NULL;
	char *agent=NULL;
	int length=0;
	/* Only log of a request came through */
	if(param->req && ok_request(param->req)) {
		ref=param->ref;
		host=param->vhost;
		agent=param->agent;
		if(ref==NULL) ref="-";
		if(host==NULL) host="-";
		if(agent==NULL) agent="-";
		if(param->response.connection==CONNECTION_KEEPALIVE) {
		} else {
			length=param->response.length;
		}
		/* host ident user date request status bytes vhost agent */
		printf("%s - - [%s] \"%s\" %d %d \"%s\" \"%s\" %s\n",
			param->host, param->ts, param->req,
			param->response.status,
			length, ref, agent, host);
		fflush(stdout);
	}
	partial_free_param(param);
}

/*
 * Stolen from dug song.
 */
char *timestamp(void)
{
	static char tstr[32], sign;
	struct tm *t, gmt;
	time_t tt = time(NULL);
	int days, hours, tz, len;

	gmt = *gmtime(&tt);
	t = localtime(&tt);

	days = t->tm_yday - gmt.tm_yday;
	hours = ((days < -1 ? 24 : 1 < days ? -24 : days * 24) +
		t->tm_hour - gmt.tm_hour);
	tz = hours * 60 + t->tm_min - gmt.tm_min;

	len = strftime(tstr, sizeof(tstr), "%e/%b/%Y:%X", t);
	if (len < 0 || len > sizeof(tstr) - 5)
		return(NULL);
	if (tz < 0) {
		sign = '-';
		tz = -tz;
	} else {
		sign='+';
	}

	snprintf(tstr + len, sizeof(tstr) - len, " %c%.2d%.2d",
		sign, tz / 60, tz % 60);

	return (tstr);
}

/*
 * My ntoa
 */
char *my_ntoa(int n)
{
	unsigned char a[4];
	static char ret[16];

	memcpy( (void *)a, (void *)&n, sizeof(int));
	sprintf(ret, "%d.%d.%d.%d", a[0], a[1], a[2], a[3]);
	return(ret);
}

/*
 * Parse the request
 */
void parse_request(char *data, struct http_param *param)
{
	char *p=NULL;

	if( (p=strtok(data, "\r\n"))==NULL) {
		return;
	} else {
		/* make a copy for our struct */
		param->req=strdup(p);
		param->ts=strdup(timestamp());
	}
	while((p=strtok(NULL, "\r\n"))!=NULL) {
		if(strncasecmp(p, "Host: ", 6) == 0) {
			param->vhost=strdup(p+6);
		} else if(strncasecmp(p, "Referer: ", 9) == 0) {
			param->ref=strdup(p+9);
		} else if(strncasecmp(p, "User-Agent: ", 12) == 0) {
			param->agent=strdup(p+12);
		}
	}
}

/*
 * Get the next request out of the saved data here.
 */
void next_request(struct http_param *param)
{
	char *p=NULL;
	if(param->extra!=NULL) {
		parse_request(param->extra, param);
		if( (p=strstr(param->extra, "\r\n\r\n"))!=NULL) {
			*p=0x00;
			p++;
			if(strlen(p)>3) {
				while(*p && (*p=='\r' || *p=='\n')) { p++; }
				p=strdup(p);
				free(param->extra);
				param->extra=p;
			}
		}
	}
}

/*
 * Process the request data.
 */
int processRequest(char *data, int length, struct http_param *param)
{
	int processed=0;
	char *p=NULL;

	/* Just go ahead and record how much data is here */
	processed=length;

	/* Don't do any of this stuff unless we don't have it yet */
	if(param->req==NULL) {
		/* Wait until we have enough headers */
		if( (p=strstr(data, "\r\n\r\n"))==NULL) {
			return(0);
		}

		*p=0x00;
		p++;
		if(strlen(p)>3) {
			while(*p && (*p=='\r' || *p=='\n')) { p++; }
			param->extra=strdup(p);
		}

		parse_request(data, param);

	}

	return(processed);
}

/*
 * Process the response data.
 */
int processResponse(char *data, int length, struct http_param *param)
{
	int processed=0;
	char *p=NULL;

	processed=length;

	/* Find out how far it is to the end of the headers */
	if(param->response.header_len==0) {
		/* Just go ahead and keep adding here */
		param->response.length+=length;
		p=strstr(data, "\r\n\r\n");

		if(p==NULL) {
			/* We don't have the end of the headers, ask for more data */
			processed=0;
		} else {
			/* Figure out how far in the headers stop */
			param->response.header_len=(p-data)+4;
			param->response.length-=param->response.header_len;

			/* Actually process the headers, see what we're dealing with. */
			p=strtok(data, "\r\n");
			param->response.status_str=strdup(p);
			/* Find the status */
			p=strchr(p, ' ');
			if(p!=NULL) {
				param->response.status=atoi(p);
			}

			/* Look through the headers and see what kind of stuff we've got */
			while( (p=strtok(NULL, "\r\n"))!=NULL) {
				if(strncasecmp(p, "Connection: ", 16) == 0) {
					if(strncasecmp(p+16, "close", 5)) {
						param->response.connection=CONNECTION_CLOSE;
					} else if(strncasecmp(p+16, "keepalive", 9)) {
						param->response.connection=CONNECTION_KEEPALIVE;
					}
				} else if(strncasecmp(p, "Transfer-Encoding: ", 19) == 0) {
					if(strncasecmp(p+19, "chunked", 7) == 0) {
						param->response.trans_enc=ENCODING_CHUNKED;
					}
				} else if(strncasecmp(p, "Content-Length: ", 16) == 0) {
					param->response.body_len=atoi(p+16);
				}
			}
		}

		/* If we're doing chunked encoding, let's get set up for that */
		if(param->response.trans_enc==ENCODING_CHUNKED) {
			char *start=data+param->response.header_len;
			char *end=data+param->response.header_len;
			int old_remaining=0;
			/* Find the last hex digit */
			while(*end && isxdigit(*end)) { end++; }
			/* Figure out what the value of that hex string is */
			param->response.remaining=strtoul(start, &end, 16);
			fprintf(stderr, "*** Chunk size is %d\n",
				param->response.remaining);
			/* Skip to the newline */
			while(*end!='\r' && *end!='\n') { end++; }
			/* and then past it */
			while(*end=='\r' || *end=='\n') { end++; }
			/* Adjust the length */
			param->response.length-=(end-start);
			/* Adjust the remaining length */
			old_remaining=param->response.remaining;
			/* The length minus the length of the length */
			param->response.remaining-=(length-(end-data));
			assert(param->response.remaining>=0);
			assert(param->response.remaining < old_remaining);
			fprintf(stderr, "*** Remaining size is %d, current length is %d\n",
				param->response.remaining, param->response.length);
		}
	} else { /* This is not the first packet */
		/* Special processing for chunked encoding */
		if(param->response.trans_enc==ENCODING_CHUNKED) {
			/* Find out how much of the current chunk we've got */
			if(length>param->response.remaining) {
				/* We've got the entire chunk here */
				char *start=NULL, *end=NULL;

				param->response.length+=param->response.remaining;

				start=data+param->response.remaining;
				end=start;
				while(*end && isxdigit(*end)) { end++; }
				param->response.remaining=strtoul(start, &end, 16);
				/* Skip past the newline */
				while(*end!='\r' && *end!='\n') { end++; }
				while(*end=='\r' || *end=='\n') { end++; }

				fprintf(stderr, "*** New remaining is %d\n",
					param->response.remaining);
				if(param->response.remaining>0) {
					param->response.length-=(end-start);
					param->response.remaining-=(length-(end-data));
					assert(param->response.remaining>=0);
				} else {
					/* Log here, we're done for a while */
					log_request(param);

					/* OK, let's drop off some of the ``processed'' data.  */
					processed=(end-data);

					/* If this was a keepalive connection, see if we have
					 * another request in there. */
					if(param->response.connection!=CONNECTION_CLOSE) {
						next_request(param);
					}
				}

			} else {
				/* This is part of our current chunk */
				param->response.remaining-=length;
				param->response.length+=length;
				fprintf(stderr, "%d bytes of the same chunk.\n", length);
			}
		} else {
			param->response.length+=length;
		}
	}

	return(processed);
}

/*
 * Return the number of bytes used from this stream.
 */
int process(struct tcp_stream *ts, int which,
	struct half_stream hs, void **data)
{
	int rv=0;
	struct http_param *param=NULL;

	/* Get the HTTP parameters */
	param=*data;

	hs.data[hs.count_new]=0x00;
	switch(which) {
		case SERVER_STREAM:
			/* rv=processRequest(hs.data, hs.count_new, param); */
			rv=processRequest(hs.data, hs.count_new, param);
			break;
		case CLIENT_STREAM:
			rv=processResponse(hs.data, hs.count_new, param);
			break;
	}

	return(rv);
}

/*
 * Figure out what kind of data we've got and do something about it.
 */
void got_packet(struct tcp_stream *ts, void **data)
{
	int todiscard=0;
	struct http_param *param=NULL;

	switch (ts->nids_state) {
		case NIDS_JUST_EST:
			ts->server.collect = 1;
			ts->client.collect = 1;
			param=calloc(sizeof(struct http_param), 1);
			assert(param);
			param->host=strdup(my_ntoa(ts->addr.saddr));
			*data=param;
			break;
		case NIDS_DATA:
			if(ts->server.count_new!=0) {
				todiscard=process(ts, SERVER_STREAM, ts->server, data);
			}
			if(ts->client.count_new!=0) {
				todiscard=process(ts, CLIENT_STREAM, ts->client, data);
			}
			break;
		case NIDS_CLOSE:
		case NIDS_RESET:
		case NIDS_TIMED_OUT:
			param=*data;
			if(param) {
				/* Log automatically frees all the internal state */
				log_request(param);
				free_param(param);
				free(param);
			}
			break;
	}
	nids_discard(ts, todiscard);
}

/*
 * Log odd events.
 */
void logger(int type, int errnum, struct ip *iph, void *data)
{
	printf("LOG:  %d %d\n", type, errnum);
}

int main(int argc, char **argv)
{
	if(argc>1) {
		nids_params.pcap_filter=argv[1];
		fprintf(stderr, "Set filter to \"%s\"\n", nids_params.pcap_filter);
	}
	/*
	nids_params.syslog = logger;
	*/
	nids_params.scan_num_hosts=0;
	nids_params.scan_num_ports=0;
	nids_init();
	nids_register_tcp(got_packet);
	nids_run();

	return(0);
}
