/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.7 1998/08/26 03:02:53 dustin Exp $
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

#include <splat.h>

static RETSIGTYPE serv_conn_alrm(int sig);

int     _debug = 0;

#define MAXSEL 1024

#define DO_STATS      1
#define MACHINE_STATS 2

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
	int     i, port;

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
dostats(int i, struct timeval timers[3], int bytes, int flags)
{
	int     a, b, c;
	char    times[3][50];
	char    tmp[20];
	float   tmpf;

	if (flags & MACHINE_STATS) {
		static int whatsup = 0;

		if (whatsup == 0) {
			whatsup = 1;
			/* Show what's up */
			printf("# descriptor:start_sec:start_usec:connect_sec:connect_usec"
			    ":end_sec:end_usec\n");
		}
		printf("%d:%u:%u:%u:%u:%u:%u:%d\n",
		    i, timers[0].tv_sec, times[0],
		    timers[1].tv_sec, times[1],
		    timers[2].tv_sec, times[2], bytes);

	} else {

		printf("Stats for %d\n", i);

		strcpy(times[0], ctime(&timers[0].tv_sec));
		strcpy(times[1], ctime(&timers[1].tv_sec));
		strcpy(times[2], ctime(&timers[2].tv_sec));

		printf("\t%d/%u %s\t%d/%u %s\t%d/%u %s",
		    timers[0].tv_usec, timers[0].tv_sec, times[0],
		    timers[1].tv_usec, timers[1].tv_sec, times[1],
		    timers[2].tv_usec, timers[2].tv_sec, times[2]);

		TVDIFF(timers[0], timers[1], a, b, c);

		printf("\tConnect time: %u.%u seconds\n", a, b);

		TVDIFF(timers[1], timers[2], a, b, c);

		printf("\tTransfer time: %u.%u seconds\n", a, b);
		sprintf(tmp, "%u.%u", a, b);
		tmpf = atof(tmp);

		printf("\tAbout %.2f Bytes/s\n", (float) ((float) bytes / tmpf));
	}
}

int
main(int argc, char **argv)
{
	int     s, selected, size, c, i, maxhits = 65535, n = 0, flags = 0;
	fd_set  fdset, tfdset;
	struct timeval timers[MAXSEL][3];
	int     bytes[MAXSEL];
	struct timeval tmptime;
	char    buf[8192];
	struct url req;
	void   *tzp;

	req.port = -1;

	while ((c = getopt(argc, argv, "dn:sS")) >= 0) {
		switch (c) {

		case 'd':
			_debug = 3;
			break;

		case 'n':
			maxhits = atoi(optarg);
			break;

		case 's':
			flags |= DO_STATS;
			break;

		case 'S':
			flags |= (DO_STATS | MACHINE_STATS);
			break;

		case '?':
			usage(argv);
			return (1);	/* I still don't like exit */
			break;
		}
	}

	if (optind >= argc) {
		printf("Nothing to do\n");
		usage(argv);
		return (1);
	}
	req = parseurl(argv[optind]);
	if (req.port == -1) {
		printf("Invalid URL format:  %s\n", argv[optind]);
		return (1);	/* I don't like exit */
	}
	_ndebug(2, ("Host:  %s\nPort:  %d\nFile:  %s\nMax:   %d\n",
		req.host, req.port, req.req, maxhits));

	FD_ZERO(&tfdset);

	resettraps();

	for (;;) {
		if (n < maxhits) {

			if (flags & DO_STATS)
				gettimeofday(&tmptime, tzp);

			s = getclientsocket(req.host, req.port);

			if (flags & DO_STATS) {
				gettimeofday(&timers[s][1], tzp);
				timers[s][0] = tmptime;
			}
			if (s > 0) {
				_ndebug(2, ("Got one: %d...\n", s));
				bytes[s] = 0;
				FD_SET(s, &tfdset);

				/* Sending */
				i = send(s, req.httpreq, strlen(req.httpreq), 0);
				_ndebug(2, ("Sent %d out of %d bytes\n",
					i, strlen(req.httpreq)));
				n++;
			}
		}
		fdset = tfdset;

		_ndebug(2, ("Selecting...\n"));

		if ((selected = select(MAXSEL, &fdset, NULL, NULL, NULL)) > 0) {
			int     i;
			for (i = 0; i < MAXSEL; i++) {
				if (FD_ISSET(i, &fdset)) {
					_ndebug(3, ("Caught %d\n", i));
					selected--;
					size = recv(i, buf, 8192, 0);
					if (size == 0) {
						_ndebug(2, ("Lost %d\n", i));
						close(i);

						if (flags & DO_STATS) {
							gettimeofday(&timers[i][2], tzp);
							dostats(i, timers[i], bytes[i], flags);
						}
						FD_CLR(i, &fdset);
						n--;
					} else {
						bytes[i] += size;
						_ndebug(2, ("Got %d bytes from %d\n", size, i));
					}
				}
				if (selected == 0)
					break;
			}
		}		/* select */
	}			/* infinite loop */
}
