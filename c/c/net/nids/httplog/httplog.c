/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
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
		if( (p=strtok(data, "\r\n"))==NULL) {
			return(processed);
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

	return(processed);
}

/*
 * Process the response data.
 */
int processResponse(char *data, int length, struct http_param *param)
{
	int processed=0;
	char *p=NULL;

	param->length+=length;

	processed=length;

	/* Find out how far it is to the end of the headers */
	if(param->start==0) {
		p=strstr(data, "\r\n\r\n");
		if(p==NULL) {
			/* We don't have the end of the headers, ask for more data */
			processed=0;
		} else {
			/* Figure out how far in the headers stop */
			param->start=(p-data)+4;
		}
	}

	if(param->status_str==NULL) {
		if( (p=strtok(data, "\r\n"))==NULL) {
			return(processed);
		} else {
			/* Find the status */
			param->status_str=strdup(p);
			p=strchr(p, ' ');
			if(p!=NULL) {
				param->status=atoi(p);
			}

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
			rv=processRequest(hs.data, hs.count_new, param);
			break;
		case CLIENT_STREAM:
			rv=processResponse(hs.data, hs.count_new, param);
			break;
	}

	return(rv);
}

/*
 * Return 1 if a request is OK.
 */
int ok_request(char *req) {
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

	/* Only log of a request came through */
	if(param->req && ok_request(param->req)) {
		ref=param->ref;
		host=param->vhost;
		agent=param->agent;

		if(ref==NULL) ref="-";
		if(host==NULL) host="-";
		if(agent==NULL) agent="-";

		/* host ident user date request status bytes vhost agent */
		printf("%s - - [%s] \"%s\" %d %d \"%s\" \"%s\" %s\n",
			param->host, param->ts, param->req,
			param->status, (param->length-param->start),
			ref, agent, host);
	}
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
				log_request(param);
				if(param->req)
					free(param->req);
				if(param->ref)
					free(param->ref);
				if(param->vhost)
					free(param->vhost);
				if(param->host)
					free(param->host);
				if(param->agent)
					free(param->agent);
				if(param->ts)
					free(param->ts);
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
