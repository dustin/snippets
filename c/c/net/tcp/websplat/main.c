/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.26 2001/12/07 01:12:48 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <assert.h>

#include <splat.h>

static RETSIGTYPE serv_conn_alrm(int sig);

int     _debug = 0;
extern int errno;

fd_set rfdset, wfdset;

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
	signal(SIGPIPE, SIG_IGN);
}

static  RETSIGTYPE
serv_conn_alrm(int sig)
{
	resettraps();
	return;
}

void
usage(char **argv)
{
	printf("Usage:  %s [-NbfdsSp] [-n max_conns] [-t total_hits] request_url\n",
	    argv[0]);
	printf("\t-d Turns on debugging\n");
	printf("\t-s Produces human readable statistics\n");
	printf("\t-S Produces machine readable statistics\n");
	printf("\t-n Maximum number of simultaneous connections\n");
	printf("\t-t Total number of hits to complete\n");
	printf("\t-N Use Nagle algorithm\n");
	printf("\t-b Use non-blocking socket IO (warning: fast)\n");
	printf("\t-f Flush after every print\n");
	printf("\t-p Keep max_conns connections open\n");
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
		printf("%u.%06u:", a, b);
	else
		printf("\tConnect time: %u.%06u seconds\n", a, b);

	TVDIFF(timers[1], timers[2], a, b, c);

	/* Transfer time */
	if (flags & MACHINE_STATS)
		printf("%u.%06u:", a, b);
	else
		printf("\tTransfer time: %u.%06u seconds\n", a, b);
	sprintf(tmp, "%u.%06u", a, b);
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
send_data(struct host_ret conn, struct url u, char *data, int offset)
{
	int	r = 0;
	if (u.ssl) {
#ifdef USE_SSLEAY
		r = SSL_write(conn.ssl, data, strlen(data));
#else
		assert(u.ssl == 0);
#endif
	} else {
		/* Calculate a send size, don't send more than 256 bytes at a time
		 * for now, so we can timeslice properly */
		int send_size=0;
		send_size = strlen(data+offset);
		/*
		if(send_size > 256) {
			send_size = 256;
		}
		*/
		r = send(conn.s, data+offset, send_size, 0);
		if(r<0) {
			/* Handle acceptable errors here */
			if(errno==EWOULDBLOCK || errno==EAGAIN || errno==ENOBUFS) {
				r=0;
			} else {
				perror("Error on send?");
			}
		}
	}
	return(r);
}

/* This allows us to do asynchronous writes, and clear the write descriptor
 * when we're done */
void handle_write(int s, struct host_ret c, struct url u, int *b)
{
	int r;
	/* Calculate the amount sent and update it in the passed in int pointer */
	if(*b>0) {
		fprintf(stderr, "*** Hey! Sending more data!\n");
	}
	/* Get the amount of data written */
	r=send_data(c, u, u.httpreq, *b);
	/* If r is not less than 0, the send is considered succesful */
	if(r>=0) {
		/* increment the current counter */
		(*b)+=r;
		_ndebug(2, ("Sent %d out of %d bytes\n", *b, strlen(u.httpreq)));
		if(*b==strlen(u.httpreq)) {
			/* We won't need to write to this again */
			FD_CLR(s, &wfdset);
		}
	} else {
		/* Whatever our error is, we're shutting down the write side,
		 * acceptable errors should be handled in send_data */
		FD_CLR(s, &wfdset);
	}
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
#ifdef USE_SSLEAY
	if(u.ssl) {
		SSL_free(conn.ssl);
		SSL_CTX_free(conn.ctx);
	}
#endif
}

int
main(int argc, char **argv)
{
	int     s, selected, size, c, i, maxhits = 65535, totalhits = 0,
	        n = 0, flags = 0, hit, sock_flags = 0;
	fd_set  trfdset, twfdset;
	struct timeval timers[MAXSEL][3];
	int     bytes[MAXSEL];  /* bytes read */
	int     wbytes[MAXSEL]; /* Bytes written */
	int		keep_populated=0;
	struct timeval tmptime;
	char    buf[8192];
	struct url req;
	struct host_ret conn, conns[MAXSEL];
	void   *tzp;
	struct growstring strings[MAXSEL];
	struct timeval *tv_s;

	/* zero the strings */
	memset(&strings, 0x00, sizeof(strings));

	req.port = -1;

	while ((c = getopt(argc, argv, "Nbfd:t:n:sSp")) >= 0) {
		switch (c) {

		case 'd':
			_debug = atoi(optarg);
			break;

		case 'N':
			sock_flags|=DO_NAGLE;
			break;

		case 'b':
			sock_flags|=NO_BLOCKING;
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

		case 'p':
			keep_populated=1;
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

	FD_ZERO(&rfdset);
	FD_ZERO(&wfdset);

	resettraps();

	/* hit will be incremented on the inside, we count a hit by a closed
	 * connection */
	for (hit = 0; hit < totalhits;) {
		if (n < maxhits && (n + hit) < totalhits) {

			int numtoopen=1, iteration=0;

			/* If the keep_populated flag is set, we want to keep all of
			 * our connections open */
			if(keep_populated) {
				numtoopen=maxhits-n;
				_ndebug(2, ("n is %d, need to open %d connections\n",
					n, numtoopen));
			}

			/* Make sure we don't open too many connections */
			if(maxhits+numtoopen > totalhits) {
				_ndebug(2,
					("Adjusting the numtoopen for the end of the hits\n"));
				numtoopen-=(totalhits-maxhits);
			}

			/* just so we can do this loop */
			s=1;
			for(iteration=0; iteration<numtoopen && s>=0; iteration++) {
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
					wbytes[s] = 0;
					if (strings[s].string == NULL) {
						strings[s].size = 1024 * sizeof(char);
						strings[s].string = malloc(strings[s].size);
					}
					strings[s].string[0] = 0x00;
					n++;
				}
			}
		}
		trfdset = rfdset;
		twfdset = wfdset;

		_ndebug(2, ("Selecting...\n"));

		tmptime.tv_sec = 1;
		tmptime.tv_usec = 0;

		/* We don't need a timeout on the select if the keep_populated flag
		 * is given, enough damage will be done. */
		if(keep_populated) {
			tv_s=NULL;
		} else {
			tv_s=&tmptime;
		}

		if ((selected=select(MAXSEL, &trfdset, &twfdset, NULL, tv_s)) >0) {
			for (i = 0; i < MAXSEL; i++) {

				/* Do we need to send anything? */
				if(FD_ISSET(i, &twfdset)) {
					selected--;
					handle_write(i, conns[i], req, &wbytes[i]);
				}

				/* Do we need to read anything? */
				if (FD_ISSET(i, &trfdset)) {
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
						FD_CLR(i, &rfdset);
						n--;
					} else {
						/* Let's only get about the first 1k of data */
						if(bytes[i]<1024) {
							str_append(&strings[i], buf);
						}
						bytes[i] += size;
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
