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

#include "antinap.h"

/*
 * This gives us an easy way to switch between server and client streams.
 */
#define SERVER_STREAM 1
#define CLIENT_STREAM 2

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
 * Return the number of matches found.
 */
int memstr(void *mem, int msize, char *str)
{
	int rv=0;
	int offset=0;
	int sl=0;
	char *p;

	assert(mem);
	assert(str);

	sl=strlen(str);

	p=memchr(mem, str[0], msize);
	while(p!=NULL) {
		offset=((int)p-(int)mem);
		if( (msize-offset) >= sl) {
			if(memcmp(p, str, sl)==0) {
				rv++;
			}
			p=memchr(p+1, str[0], msize-offset);
		} else {
			p=NULL;
		}
	}

	return(rv);
}

/*
 * Postprocess this packet.
 */
int postprocess(struct tcp_stream *ts, int which,
	struct half_stream hs, struct antinap *an)
{
	char *type=NULL;

	switch(an->type) {
		case TRAF_NAPSTER:
			type="napster data";
			break;
		case TRAF_GNUTELLA:
			type="gnutella data";
			break;
		default:
			type="unknown";
			break;
	}

	if(an->fireatwill != 0) {
		char *src;
		char *dst;

		src=strdup(my_ntoa(ts->addr.saddr));
		dst=strdup(my_ntoa(ts->addr.daddr));
		printf("Found a fire at will stream:  %s:%d->%s:%d (%s)\n",
			src, ts->addr.source,
			dst, ts->addr.dest,
			type);
		free(src);
		free(dst);
		nids_killtcp(ts);
	}
}

/*
 * Check to see if this is a gnutella client connect.
 */
int checkGnutellaClient(struct tcp_stream *ts,
	struct half_stream hs, struct antinap *an)
{
	int rv=0;
	if(memstr(hs.data, hs.count_new, "GNUTELLA CONNECT")==1) {
		an->type=TRAF_GNUTELLA;
		an->fireatwill=1;

		/* We don't need any more data, we're in fire at will mode */
		ts->server.collect=0;
	}
	/* We got it all */
	rv=hs.count_new;

	return(rv);
}

/*
 * Check to see if this is a gnutella server accepting a connection
 */
int checkGnutellaServer(struct tcp_stream *ts,
	struct half_stream hs, struct antinap *an)
{
	int rv=0;
	if(memstr(hs.data, hs.count_new, "GNUTELLA OK")==1) {
		an->type=TRAF_GNUTELLA;
		an->fireatwill=1;

		/* We don't need any more data, we're in fire at will mode */
		ts->client.collect=0;
	}
	/* We got it all */
	rv=hs.count_new;

	return(rv);
}

/*
 * Count up how many times we've seen ``.mp3"'' in this stream.
 */
int checkNapster(struct tcp_stream *ts, struct half_stream hs,
	struct antinap *an)
{
	int rv=0;
	/* Add the number of .mp3"'s in this packet. */
	an->mp3count+=memstr(hs.data, hs.count_new, ".mp3\"");

	/* If there's more than five in either direction, fire at will. */
	if(an->mp3count > 5) {
		an->type=TRAF_NAPSTER;
		an->fireatwill=1;
		/* We've got enough */
		ts->client.collect=0;
		ts->server.collect=0;
	}

	return(rv);
}

/*
 * Return the number of bytes used from this stream.
 */
int process(struct tcp_stream *ts, int which,
	struct half_stream hs, void **data)
{
	int rv=0;
	struct antinap *an=NULL;

	/* Get the Antinap parameters */
	an=*data;

	hs.data[hs.count_new]=0x00;

	switch(which) {
		case SERVER_STREAM:
			if(an->type == TRAF_UNKNOWN) {
				rv=checkGnutellaClient(ts, hs, an);
			}
			if(an->type == TRAF_UNKNOWN) {
				rv=checkNapster(ts, hs, an);
			}
			break;
		case CLIENT_STREAM:
			if(an->type == TRAF_UNKNOWN) {
				rv=checkGnutellaServer(ts, hs, an);
			}
			if(an->type == TRAF_UNKNOWN) {
				rv=checkNapster(ts, hs, an);
			}
			break;
	}

	postprocess(ts, which, hs, an);

	return(rv);
}

/*
 * Figure out what kind of data we've got and do something about it.
 */
void got_packet(struct tcp_stream *ts, void **data)
{
	int todiscard=0;
	struct antinap *an=NULL;

	switch (ts->nids_state) {
		case NIDS_JUST_EST:
			ts->server.collect = 1;
			ts->client.collect = 1;
			an=calloc(sizeof(struct antinap), 1);
			assert(an);
			*data=an;
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
			an=*data;
			if(an) {
				free(an);
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
	nids_params.n_tcp_streams=65536;
	nids_params.scan_num_hosts=0;
	nids_params.scan_num_ports=0;
	nids_init();
	nids_register_tcp(got_packet);
	nids_run();

	return(0);
}
