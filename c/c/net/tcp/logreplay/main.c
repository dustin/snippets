/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.5 1998/12/07 20:00:29 dustin Exp $
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

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <replay.h>

static RETSIGTYPE serv_conn_alrm(int sig);

int     _debug = 0;

#define MAXSEL 1024

#define DO_STATS      1
#define MACHINE_STATS 2
#define FLUSH_OUT     3

#define TVDIFF(tv1, tv2, a, b, c) \
	c=0; \
    a=tv2.tv_sec-tv1.tv_sec; \
    if(tv2.tv_usec<tv1.tv_usec) { \
        a--; \
		c=1000000; \
    } \
    b=(tv2.tv_usec+c)-tv1.tv_usec;

static void
resettraps(void)
{
	signal(SIGALRM, serv_conn_alrm);
}

static  RETSIGTYPE
serv_conn_alrm(int sig)
{
	resettraps();
	return;
}

/*
 * Parse a url string into a struct url;
 * port is -1, and host and req are NULL if it fails.
 */
struct url
parseurl(char *url)
{
	char   *tmp;
	struct url u;
	int     port = 0;

	u.host = NULL;
	u.req = NULL;
	u.port = -1;

	/* We only do http and maybe https urls */
	if (strncmp(url, "http://", 7) == 0) {
		u.ssl = 0;
	} else {
#ifdef USE_SSLEAY
		if (strncmp(url, "https://", 7) == 0)
			u.ssl = 1;
		else
#endif
			return (u);
	}

	/* Host is the first thing, after http:// or https:// */
	u.host = strdup(url + 7 + u.ssl);

	/*
	 * OK, request comes along eventually, so let's mark it as host, and
	 * go from there.  Look for a :, /, or NULL to let us know we're done
	 * with host.
	 */
	u.req = u.host;
	while (*u.req && *u.req != ':' && *u.req != '/')
		u.req++;

	/*
	 * Let's see which one we got.
	 */
	switch (*u.req) {
	case NULL:		/* format http://host.domain.com */
		u.req = strdup("/");
		port = (u.ssl ? 443 : 80);
		break;
	case ':':		/* format http://host.domain:port/ */
		port = atoi(u.req + 1);
		assert(port);
		*u.req = NULL;
		u.req++;
		while (*u.req && *u.req != '/')
			u.req++;
		u.req = *u.req ? strdup(u.req) : "/";
		break;
	case '/':		/* format http://host.domain.com/ */
		port = (u.ssl ? 443 : 80);
		tmp = u.req;
		u.req = strdup(u.req);
		*tmp = NULL;
		break;
	}

	assert(strlen(u.req) < (REQ_LEN - 16));		/* TMI */
	strcpy(u.httpreq, "GET ");
	strcat(u.httpreq, u.req);
	strcat(u.httpreq, " HTTP/1.0\n\n");

	u.port = port;
	return (u);
}

void
usage(char *progname)
{
	printf("Usage:  %s host port access_log\n", progname);
}

struct http_status
getstatus(char *response, char *IP)
{
	char *p, *p2, *in;
	struct http_status status;

	status.status=-1;
	status.string=NULL;

	in=strdup(response);

	p=strchr(in, ' ');
	assert(p);
	p++;

	p2=p;

	while(*p2 && (*p2!='\r' || *p2!='\n')) p2++;
	*p2=NULL;

	status.status=atoi(p);
	p=strchr(p, ' ');
	assert(p);
	p++;

	status.string=strdup(p);
	assert(status.string);

	free(in);

	return(status);
}

void
dostats(int i, char *request, struct timeval timers[3],
    int bytes, int delay, char *response, int flags)
{
	int     a, b, c;
	char    times[3][50];
	char    tmp[20];
	float   tmpf;
	struct	http_status status;

	static int whatsup = 0;

	/* label machine stats if we're doing them */
	if (flags & MACHINE_STATS) {
		if (whatsup == 0) {
			whatsup = 1;
			printf("# descriptor:delay:start_sec:con_time:"
			    "trans_time:bytes:bps:status:request\n");
		}
	}

	status=getstatus(response, "127.0.0.1");

	/* descriptor and number of connections */
	if (flags & MACHINE_STATS)
		printf("%d:%d:", i, delay);
	else
		printf("Stats for %d (delay %d, status (%d %s))\n",
			i, delay, status.status, status.string);

	strcpy(times[0], ctime(&timers[0].tv_sec));
	strcpy(times[1], ctime(&timers[1].tv_sec));
	strcpy(times[2], ctime(&timers[2].tv_sec));

	/* timestamps */
	if (flags & MACHINE_STATS) {
		printf("%ld:", timers[0].tv_sec);
	} else {
		printf("\t%ld/%lu %s\t%ld/%lu %s\t%ld/%lu %s",
		    timers[0].tv_usec, timers[0].tv_sec, times[0],
		    timers[1].tv_usec, timers[1].tv_sec, times[1],
		    timers[2].tv_usec, timers[2].tv_sec, times[2]);
	}

	TVDIFF(timers[0], timers[1], a, b, c);

	/* Connect time */
	if (flags & MACHINE_STATS)
		printf("%u.%u:", a, b);
	else
		printf("\tConnect time: %u.%u seconds\n", a, b);

	TVDIFF(timers[1], timers[2], a, b, c);

	/* Transfer time */
	if (flags & MACHINE_STATS)
		printf("%u.%u:", a, b);
	else
		printf("\tTransfer time: %u.%u seconds\n", a, b);
	sprintf(tmp, "%u.%u", a, b);
	tmpf = atof(tmp);

	/* Bytes and bps */
	if (flags & MACHINE_STATS)
		printf("%d:%.2f:%d:%s\n", bytes, (float) ((float) bytes / tmpf),
			status.status, request);
	else
		printf("\tAbout %.2f Bytes/s\n", (float) ((float) bytes / tmpf));

	if (flags & FLUSH_OUT)
		fflush(stdout);

	if(status.string)
		free(status.string);
}

int
open_connection(char *host, int port, struct log_entry log)
{
	int     s, i;

	s = getclientsocket(host, port);
	if (s < 0)
		return (-1);

	i = send(s, log.request, strlen(log.request), 0);
	i += send(s, "\r\n\r\n", 4, 0);
	return (s);
}

int
str_append(struct growstring *s, char *buf)
{
	while( (strlen(s->string)+strlen(buf)) > s->size) {
		s->size+=(1024*(sizeof(char)));
		s->string=realloc(s->string, s->size);
		assert(s->string);
	}

	strcat(s->string, buf);
}

void
process(char *host, int port, FILE * f, int flags)
{
	int     s, size, index, connected = 0, selected;
	fd_set  fdset, tfdset;
	time_t  start_time, current_time;
	char    buf[8192];
	struct timeval t;
	struct timeval timers[MAXSEL][3], tmptime;
	int     bytes[MAXSEL], done = 0;
	char   *requests[MAXSEL];
	int     delays[MAXSEL];
	void   *tzp = NULL;
	struct log_entry log;
	struct growstring strings[MAXSEL];

	if (getlog(f, &log) != 0)
		return;

	start_time = time(NULL);

	FD_ZERO(&tfdset);

	/* zero the strings */
	memset(&strings, 0x00, sizeof(strings));

	/*
	 * The Loop(tm).  Here, we loop as long as we have more logs entries to
	 * process, or we have connections open.
	 */
	while ((!done) || connected > 0) {

		current_time = time(NULL);

		/* This loop runs if/as long as there are new log entries that need
		 * to be processed on this loop run */
		while ((!done)
		    && log.timeoffset <= (current_time - start_time)) {

			/*
			 * printf("Need to start one for %d (we're at %d):\n\t%s\n",
			 * logs[index]->timeoffset,
			 * (current_time-start_time), logs[index]->request );
			 */

			gettimeofday(&tmptime, tzp);
			s = open_connection(host, port, log);
			if (s > 0) {
				/* Record timers, etc... */
				timers[s][0] = tmptime;
				gettimeofday(&timers[s][1], tzp);
				requests[s] = strdup(log.request);
				delays[s] = log.timeoffset;

				/* Go ahead and pre-allocate 1k, if it's allocated already,
				 * just prepare to use it again */
				if(strings[s].string==NULL) {
					strings[s].size=1024*sizeof(char);
					strings[s].string=malloc(strings[s].size);
				}
				strings[s].string[0]=0x00;

				bytes[s] = 0;
				connected++;
				FD_SET(s, &tfdset);
			}
			index++;
			if (log.IP)
				free(log.IP);
			if (log.request)
				free(log.request);
			if (getlog(f, &log) != 0)
				done = 1;
		}

		fdset = tfdset;
		t.tv_sec = 1;
		t.tv_usec = 0;

		if ((selected = select(MAXSEL, &fdset, NULL, NULL, &t)) > 0) {
			int     i;
			for (i = 0; selected > 0 && i < MAXSEL; i++) {
				if (FD_ISSET(i, &fdset)) {
					selected--;
					size = recv(i, buf, 8190, 0);
					if (size < 1) {
						/* printf("--- Lost %d...\n", i); */
						gettimeofday(&timers[i][2], tzp);
						dostats(i, requests[i], timers[i], bytes[i],
						    delays[i], strings[i].string, flags);
						if(requests[i])
							free(requests[i]);
						if(strings[i].string) {
							free(strings[i].string);
							strings[i].string=NULL;
						}
						close(i);
						connected--;
						FD_CLR(i, &tfdset);
					} else {
						bytes[i] += size;
						str_append(&strings[i], buf);
					}	/* Figured out whether it's data or disco-nect */
				}	/* Found a match */
			}	/* checking who's selected */
		}		/* select */
	}			/* The Loop(tm) */
}

int
main(int argc, char **argv)
{
	FILE   *f;

	if(argc<4) {
		usage(argv[0]);
		return(1);
	}

	if (strcmp(argv[3], "-") == 0) {
		f = stdin;
	} else {
		f = fopen(argv[3], "r");
		if (f == NULL) {
			perror(argv[3]);
			return (-1);
		}
	}

	process(argv[1], atoi(argv[2]), f, 0xffffffff);
	return (0);
}
