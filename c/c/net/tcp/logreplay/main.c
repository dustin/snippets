/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.2 1998/12/06 19:58:59 dustin Exp $
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
	int     port=0;

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
usage(char **argv)
{
	printf("Usage:  %s [-dsS] [-n max_conns] request_url\n", argv[0]);
}

void
dostats(int i, char *request, struct timeval timers[3],
        int bytes, int delay, int flags)
{
	int     a, b, c;
	char    times[3][50];
	char    tmp[20];
	float   tmpf;

	static int whatsup = 0;

	/* label machine stats if we're doing them */
	if (flags & MACHINE_STATS) {
		if (whatsup == 0) {
			whatsup = 1;
			printf("# descriptor:delay:start_sec:con_time:"
			    "trans_time:bytes:bps:request\n");
		}
	}
	/* descriptor and number of connections */
	if (flags & MACHINE_STATS)
		printf("%d:%d:", i, delay);
	else
		printf("Stats for %d (delay %d)\n", i, delay);

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
		printf("%d:%.2f:%s\n", bytes, (float) ((float) bytes / tmpf),
		    request);
	else
		printf("\tAbout %.2f Bytes/s\n", (float) ((float) bytes / tmpf));

	if (flags & FLUSH_OUT)
		fflush(stdout);
}

struct
log_entry **
get_config(char *path)
{
	FILE   *f;
	char    buf[64 * 1024];	/* Nothing wrong with a 64k buffer, right? */
	struct log_entry **list;
	struct log_entry log;
	int     size = 64 * 1024, index = 0;
	char   *p, *p2;

	f = fopen(path, "r");
	if (f == NULL)
		return (NULL);

	list = (void *) malloc(size * sizeof(struct log_entry **));
	assert(list);

#define LAPPEND(a) if(index == size-1) { \
		size=size*(64*1024); \
		list=realloc(list, size*sizeof(struct log_entry **)); \
		assert(list); \
	} \
	list[index]=(void *)malloc(sizeof(struct log_entry)); \
	assert(list[index]); \
	list[index]->timeoffset=a.timeoffset; \
	list[index]->IP=a.IP; \
	list[index]->request=a.request; \
	index++;

	while (fgets(buf, 64 * 1024, f)) {
		log.timeoffset = 0;
		log.IP = NULL;
		log.request = NULL;
		log.timeoffset = atoi(buf);
		p = strchr(buf, '\t');
		p++;
		p2 = strchr(p, '\t');
		*p2 = 0x00;
		log.IP = strdup(p);

		p2++;
		p2[strlen(p2) - 1] = 0x00;
		log.request = strdup(p2);
		LAPPEND(log);
	}
	list[index] = 0x00;

	return (list);
}

void
print_conf(struct log_entry **logs)
{
	int     i;

	for (i = 0; logs[i]; i++) {
		assert(logs[i]->IP);
		assert(logs[i]->request);
		printf("Time offset:  %d\n\tIP:  %s\n\tRequest:  %s\n\n",
		    logs[i]->timeoffset, logs[i]->IP, logs[i]->request);
	}
}

void
freelogs(struct log_entry **logs)
{
	int     i;
	for (i = 0; logs[i]; i++) {
		free(logs[i]->IP);
		free(logs[i]->request);
		free(logs[i]);
	}
	free(logs);
}

int
open_connection(char *host, int port, struct log_entry *log)
{
	int     s, i;

	s = getclientsocket(host, port);
	if (s < 0)
		return (-1);

	i = send(s, log->request, strlen(log->request), 0);
	i += send(s, "\r\n\r\n", 4, 0);
	return (s);
}

void
process(char *host, int port, struct log_entry **logs, int flags)
{
	int     s, size, index, connected = 0, selected;
	fd_set  fdset, tfdset;
	time_t  start_time, current_time;
	char    buf[8192];
	struct timeval t;
	struct timeval timers[MAXSEL][3], tmptime;
	int     bytes[MAXSEL];
	char   *requests[MAXSEL];
	int    delays[MAXSEL];
	void   *tzp=NULL;

	start_time = time(NULL);

	FD_ZERO(&tfdset);

	/*
	 * The Loop(tm).  Here, we loop as long as we have more logs entries to
	 * process, or we have connections open.
	 */
	for (index = 0; logs[index] || connected > 0;) {

		current_time = time(NULL);

		/* This loop runs if/as long as there are new log entries that need
		 * to be processed on this loop run */
		while (logs[index]
		    && logs[index]->timeoffset <= (current_time - start_time)) {

			/*
			 * printf("Need to start one for %d (we're at %d):\n\t%s\n",
			 * logs[index]->timeoffset,
			 * (current_time-start_time), logs[index]->request );
			 */

			gettimeofday(&tmptime, tzp);
			s = open_connection(host, port, logs[index]);
			if (s > 0) {
				/* Record timers, etc... */
				timers[s][0] = tmptime;
				gettimeofday(&timers[s][1], tzp);
				requests[s] = logs[index]->request;
				delays[s] = logs[index]->timeoffset;

				bytes[s] = 0;
				connected++;
				FD_SET(s, &tfdset);
			}
			index++;
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
							delays[i], flags);
						close(i);
						connected--;
						FD_CLR(i, &tfdset);
					} else {
						bytes[i]+=size;
					} /* Figured out whether it's data or disco-nect */
				}	/* Found a match */
			}	/* checking who's selected */
		}		/* select */
	}			/* The Loop(tm) */
}

int
main(int argc, char **argv)
{
	struct log_entry **logs;

	logs = get_config(argv[1]);
	if (logs == NULL) {
		printf("Couldn't parse any logs...\n");
		return (1);
	}
	process("bleu.west.spy.net", 80, logs, 0xffffffff);
	freelogs(logs);
	return(0);
}
