/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.20 1999/06/18 17:49:23 dustin Exp $
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
	int     port;
	struct growstring grow;

	grow.size = 1024 * sizeof(char);
	grow.string = calloc(1, grow.size);

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

	if(getenv("WEBSPLAT_POST")) {
		str_append(&grow, "POST ");
	} else {
		str_append(&grow, "GET ");
	}
	str_append(&grow, u.req);
	str_append(&grow,  " HTTP/1.0\r\n");

	if(getenv("WEBSPLAT_COOKIE")) {
		str_append(&grow,  "Cookie: ");
		str_append(&grow,  getenv("WEBSPLAT_COOKIE"));
		str_append(&grow,  "\r\n");
	}

	if(getenv("WEBSPLAT_POST")) {
		FILE *f;
		char buf[8192];
		struct stat st;
		int ret;

		ret=stat(getenv("WEBSPLAT_POST"), &st);
		assert(ret>=0);

		sprintf(buf, "Content-Length: %d\r\n", st.st_size);
		str_append(&grow, buf);

		str_append(&grow, "Content-Type: application/x-www-form-urlencoded\r\n");

		str_append(&grow,  "\r\n");

		f=fopen(getenv("WEBSPLAT_POST"), "r");
		assert(f);

		while(fgets(buf, 8190, f)) {
			str_append(&grow, buf);
		}

		fclose(f);
	} else {
		/* This ends the request if we're doing a GET */
		str_append(&grow,  "\r\n");
	}

	u.httpreq=grow.string;

	u.port = port;
	return (u);
}

void
usage(char **argv)
{
	printf("Usage:  %s [-NfdsS] [-n max_conns] [-t total_hits] request_url\n",
	    argv[0]);
	printf("\t-d Turns on debugging\n");
	printf("\t-s Produces human readable statistics\n");
	printf("\t-S Produces machine readable statistics\n");
	printf("\t-n Maximum number of simultaneous connections\n");
	printf("\t-t Total number of hits to complete\n");
	printf("\t-N Use Nagle algorithm\n");
	printf("\t-f Flush after every print\n");
}

struct http_status
getstatus(char *response)
{
	char   *p, *p2, *in;
	struct http_status status;

	status.status = -1;
	status.string = NULL;

	in = strdup(response);

	p = strchr(in, ' ');
	if (p==NULL)
		return(status);
	p++;

	p2 = p;

	while (*p2 && (*p2 != '\r' || *p2 != '\n'))
		p2++;
	*p2 = NULL;

	status.status = atoi(p);
	p = strchr(p, ' ');
	if (p==NULL) {
		status.status=-1;
		return(status);
	}
	p++;

	status.string = strdup(p);
	assert(status.string);

	free(in);

	return (status);
}

void
dostats(int i, struct timeval timers[3], int bytes,
    int flags, char *response, int maxhits)
{
	int     a, b, c;
	char    times[3][50];
	char    tmp[20];
	float   tmpf;
	struct http_status status;

	static int whatsup = 0;

	status = getstatus(response);

	/* label machine stats if we're doing them */
	if (flags & MACHINE_STATS) {
		if (whatsup == 0) {
			whatsup = 1;
			printf("# descriptor:conns:start_sec:con_time:"
			    "trans_time:bytes:bps:status\n");
		}
	}
	/* descriptor and number of connections */
	if (flags & MACHINE_STATS)
		printf("%d:%d:", i, maxhits);
	else
		printf("Stats for %d (%d connections)\n", i, maxhits);

	strcpy(times[0], ctime(&timers[0].tv_sec));
	strcpy(times[1], ctime(&timers[1].tv_sec));
	strcpy(times[2], ctime(&timers[2].tv_sec));

	/* timestamps */
	if (flags & MACHINE_STATS) {
		printf("%ld:", timers[0].tv_sec);
	} else {
		printf("\t%lu/%lu %s\t%lu/%lu %s\t%lu/%lu %s",
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
		printf("%d:%.2f:%d\n", bytes, (float) ((float) bytes / tmpf),
		    status.status);
	else
		printf("\tAbout %.2f Bytes/s\n", (float) ((float) bytes / tmpf));

	if (flags & FLUSH_OUT)
		fflush(stdout);

	if (status.string)
		free(status.string);
}

void
str_append(struct growstring *s, char *buf)
{
	while ((strlen(s->string) + strlen(buf)) > s->size) {
		s->size += (1024 * (sizeof(char)));
		s->string = realloc(s->string, s->size);
		assert(s->string);
	}

	strcat(s->string, buf);
}

int
send_data(struct host_ret conn, struct url u, char *data)
{
	int	r = 0;
	if (u.ssl) {
#ifdef USE_SSLEAY
		r = SSL_write(conn.ssl, data, strlen(data));
#else
		assert(u.ssl == 0);
#endif
	} else {
		r = send(conn.s, data, strlen(data), 0);
	}
	return(r);
}

int
recv_data(struct host_ret conn, struct url u, char *buf, size_t len)
{
	int	size=0;
	if (u.ssl) {
#ifdef USE_SSLEAY
		size = SSL_read(conn.ssl, buf, len);
#else
		assert(u.ssl == 0);
#endif
	} else {
		size = recv(conn.s, buf, len, 0);
	}
	buf[size]=0x00;
	return(size);
}

void
close_conn(struct url u, struct host_ret conn)
{
	assert(conn.s>=0);
	close(conn.s);
	if(u.ssl) {
		SSL_free(conn.ssl);
		SSL_CTX_free(conn.ctx);
	}
}

int
main(int argc, char **argv)
{
	int     s, selected, size, c, i, maxhits = 65535, totalhits = 0,
	        n = 0, flags = 0, hit, sock_flags = 0;
	fd_set  fdset, tfdset;
	struct timeval timers[MAXSEL][3];
	int     bytes[MAXSEL];
	struct timeval tmptime;
	char    buf[8192];
	struct url req;
	struct host_ret conn, conns[MAXSEL];
	void   *tzp;
	struct growstring strings[MAXSEL];

	/* zero the strings */
	memset(&strings, 0x00, sizeof(strings));

	req.port = -1;

	while ((c = getopt(argc, argv, "Nfd:t:n:sS")) >= 0) {
		switch (c) {

		case 'd':
			_debug = atoi(optarg);
			break;

		case 'N':
			sock_flags|=DO_NAGLE;
			break;

		case 'n':
			maxhits = atoi(optarg);
			break;

		case 't':
			totalhits = atoi(optarg);
			break;

		case 's':
			flags |= DO_STATS;
			break;

		case 'S':
			flags |= (DO_STATS | MACHINE_STATS);
			break;

		case 'f':
			flags |= (FLUSH_OUT);
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
	if (totalhits < 1) {
		totalhits = 1024 * 1024 * 2;	/* a few */
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

	/* hit will be incremented on the inside, we count a hit by a closed
	 * connection */
	for (hit = 0; hit < totalhits;) {
		if (n < maxhits) {

			if (flags & DO_STATS)
				gettimeofday(&tmptime, tzp);

			conn = getclientsocket(req, sock_flags);
			s=conn.s;
			conns[s]=conn;

			if (flags & DO_STATS) {
				gettimeofday(&timers[s][1], tzp);
				timers[s][0] = tmptime;
			}
			if (s > 0) {
				_ndebug(1, ("Got one: %d...\n", s));
				bytes[s] = 0;
				FD_SET(s, &tfdset);

				if (strings[s].string == NULL) {
					strings[s].size = 1024 * sizeof(char);
					strings[s].string = malloc(strings[s].size);
				}
				strings[s].string[0] = 0x00;

				/* Sending */
				i = send_data(conn, req, req.httpreq);
				_ndebug(2, ("Sent %d out of %d bytes\n",
					i, strlen(req.httpreq)));
				n++;
			}
		}
		fdset = tfdset;

		_ndebug(2, ("Selecting...\n"));

		tmptime.tv_sec = 1;
		tmptime.tv_usec = 0;

		if ((selected = select(MAXSEL, &fdset, NULL, NULL, &tmptime)) > 0) {
			int     i;
			for (i = 0; i < MAXSEL; i++) {
				if (FD_ISSET(i, &fdset)) {
					_ndebug(3, ("Caught %d\n", i));
					selected--;
					size = recv_data(conns[i], req, buf, 8192);
					if (size == 0) {
						_ndebug(1, ("Lost %d\n", i));
						_ndebug(3, (strings[i].string));
						hit++;
						close_conn(req, conns[i]);

						if (flags & DO_STATS) {
							gettimeofday(&timers[i][2], tzp);
							dostats(i, timers[i], bytes[i],
							    flags, strings[i].string, maxhits);
							if (strings[i].string) {
								free(strings[i].string);
								strings[i].string = NULL;
							}
						}
						FD_CLR(i, &fdset);
						n--;
					} else {
						bytes[i] += size;
						str_append(&strings[i], buf);
						_ndebug(2, ("Got %d bytes from %d\n", size, i));
					}
				}
				if (selected == 0)
					break;
			}
		}		/* select */
	}			/* splat loop */
	return(0);
}
