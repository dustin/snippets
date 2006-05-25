/*
 * Copyright (c) 1998  dustin sallings
 *
 * $Id: filter.c,v 1.6 2000/07/30 07:41:53 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <arpa/inet.h>
#include <pcap.h>
#include <string.h>
#include <assert.h>

#ifdef USE_PTHREAD
#include <pthread.h>
#endif /* USE_PTHREAD */

#include "mymalloc.h"
#include "multisniff.h"
#include "hash.h"

static pcap_t  *pcap_socket = NULL;
static int      dlt_len = 0;
static struct hashtable *hash=NULL;

static int signalled=0;

static void     filter_packet(u_char *, struct pcap_pkthdr *, u_char *);
static void     showStats();
static void     signal_handler(int);
static char    *itoa(int in);

#ifdef USE_PTHREAD
# define lock(a)   pthread_mutex_lock(&(hash->mutexen[a]))
# define unlock(a) pthread_mutex_unlock(&(hash->mutexen[a]))
static pthread_cond_t threadcond;
static pthread_mutex_t threadmutex;
#else
# define lock(a)
# define unlock(a)
#endif

#ifdef USE_PTHREAD
void *statusPrinter(void *data)
{
	int oldstate=0;
	for(;;) {
		time_t now=time(NULL);
		struct timespec ts;

		ts.tv_nsec=0;
		ts.tv_sec=now + PTHREAD_PRINT_INTERVAL;

		pthread_testcancel();
		pthread_mutex_lock(&threadmutex);
		pthread_cond_timedwait(&threadcond, &threadmutex, &ts);
		pthread_mutex_unlock(&threadmutex);
		pthread_testcancel();

		if(pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldstate) < 0) {
			perror("pthread_setcancelstate(disable)");
			exit(1);
		}

		showStats();

		if(pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &oldstate) < 0) {
			perror("pthread_setcancelstate(enable)");
			exit(1);
		}
	}
	printf("! Status thread shutting down.\n");
	return NULL;
}
#else
void nonThreadsStats()
{
	static time_t last_time=0;
	time_t t=0;

	t=time(NULL);

	if(t-last_time > NON_PTHREAD_PRINT_INTERVAL) {
		last_time=t;
		showStats();
	}
}
#endif /* USE_PTHREAD */

#ifdef USE_PTHREAD
static void cleanup(pthread_t sp) {
	printf("Waiting for status printer thread.\n");
	pthread_cancel(sp);
	pthread_mutex_lock(&threadmutex);
	pthread_cond_signal(&threadcond);
	pthread_mutex_unlock(&threadmutex);
	pthread_join(sp, NULL);
	printf("Joined status printer thread.\n");
#else
static void cleanup() {
#endif

	hash_destroy(hash);
	pcap_close(pcap_socket);
#ifdef MYMALLOC
	_mdebug_dump();
#endif /* MYMALLOC */
	exit(signalled);
}

static void setupSignals() {
	sigset_t sigBlockSet;
	struct sigaction sa;

	sigemptyset(&sigBlockSet);
	sigaddset(&sigBlockSet, SIGHUP);
	if(sigprocmask(SIG_BLOCK, &sigBlockSet, NULL) < 0) {
		perror("sigprocmask");
		exit(1);
	}

	sa.sa_handler=signal_handler;
	sa.sa_flags=0;
	sigemptyset(&sa.sa_mask);

	if(sigaction(SIGINT, &sa, NULL) < 0) {
		perror("sigaction(INT)");
		exit(1);
	}
	if(sigaction(SIGQUIT, &sa, NULL) < 0) {
		perror("sigaction(QUIT)");
		exit(1);
	}
	if(sigaction(SIGTERM, &sa, NULL) < 0) {
		perror("sigaction(TERM)");
		exit(1);
	}
}

void
process(int flags, const char *intf, const char *outdir, char *filter)
{
	char            errbuf[PCAP_ERRBUF_SIZE];
	struct bpf_program prog;
	bpf_u_int32     netmask=0;
	int             flagdef;
	char			*use_pthread="no";
#ifdef USE_PTHREAD
	pthread_t		sp;
#endif

	setupSignals();

	if (flags & FLAG_BIT(FLAG_PROMISC))	{
		flagdef = 1;
	} else {
		flagdef = 0;
	}

	pcap_socket = pcap_open_live(intf, 65535, flagdef, 10, errbuf);

	if (pcap_socket == NULL) {
		fprintf(stderr, "pcap_open_live: %s\n", errbuf);
		exit(-1);
	}
	switch (pcap_datalink(pcap_socket)) {
	case DLT_EN10MB:
		dlt_len = 14;
		break;
	case DLT_SLIP:
		dlt_len = 16;
		break;
	case DLT_PPP:
		dlt_len = 4;
		break;
	case DLT_FDDI:
		fprintf(stderr, "Sorry, can't do FDDI\n");
		signal_handler(-1);
		break;
	default:
		dlt_len = 4;
	}

	if (pcap_compile(pcap_socket, &prog, filter, 1, netmask) < 0) {
		fprintf(stderr, "pcap_compile: %s\n", errbuf);
		signal_handler(-1);
	}
	if (pcap_setfilter(pcap_socket, &prog) < 0) {
		fprintf(stderr, "pcap_setfilter: %s\n", errbuf);
		signal_handler(-1);
	}

	hash=hash_init(637);

#ifdef USE_PTHREAD
	use_pthread="yes";
	/* OK, create the status printer thread */
	if(pthread_mutex_init(&threadmutex, NULL) < 0) {
		perror("pthread_mutex_init");
		exit(1);
	}
	if(pthread_cond_init(&threadcond, NULL) < 0) {
		perror("pthread_cond_init");
		exit(1);
	}
	if(pthread_create(&sp, NULL, statusPrinter, NULL) < 0) {
		perror("pthread_create");
		exit(1);
	}
#endif /* USE_PTHREAD */

	fprintf(stderr,"interface: %s, filter: %s, promiscuous: %s, threads: %s\n",
		intf,
		filter,
		(flags & FLAG_BIT(FLAG_PROMISC)) ? "yes" : "no",
		use_pthread);
	fflush(stderr);

	if(chdir(outdir) < 0) {
		perror("chdir");
		exit(1);
	}

	while (signalled==0) {
		pcap_loop(pcap_socket, 100, (pcap_handler) filter_packet, NULL);
#ifdef USE_PTHREAD
		/* This is for bad pthread implementations */
		usleep(1);
#else
		nonThreadsStats();
#endif /* USE_PTHREAD */
	}

#if USE_PTHREAD
	cleanup(sp);
#else
	cleanup();
#endif
}

static void
showStats()
{
	static unsigned int last_pcount=0, last_dropcount=0;
	struct pcap_stat stats;
	struct hash_container *p;
	int i;
	struct timeval now;

	if(gettimeofday(&now, NULL) < 0) {
		perror("gettimeofday");
		exit(1);
	}

	if (pcap_stats(pcap_socket, &stats) == 0) {
		printf("# Processed %d packets, dropped %d\n",
		       stats.ps_recv-last_pcount,
			   stats.ps_drop-last_dropcount);
		last_pcount=stats.ps_recv;
		last_dropcount=stats.ps_drop;
	} else {
		printf("# Error getting pcap statistics.\n");
	}
	/* Look for anything old enough to get cleaned up */
	for(i=0; i<hash->hashsize; i++) {
		lock(i);
		p=hash->buckets[i];
		unlock(i);
		if(p) {
			int ci=0;
			int toClose[1024];
			int closeOffset=0;

			lock(i);
			for(; p; p=p->next) {
				pcap_dump_flush(p->pcap_dumper);
				if(p->last_addition.tv_sec + MAX_PKT_AGE < now.tv_sec) {
					toClose[closeOffset++]=p->key;
				}
			}
			unlock(i);

			for(ci=0; ci<closeOffset; ci++) {
				p=hash_find(hash, toClose[ci]);
				assert(p != NULL);
				printf("# Closing %s (too old)\n", p->filename);
				p=NULL; /* Can't use this anymore */
				hash_delete(hash, toClose[ci]);
			}
		}
	}
	fflush(stdout);
}

/* this is the function that's called when pcap reads a packet */
void
filter_packet(u_char * u, struct pcap_pkthdr * p, u_char * packet)
{
#define IP_SIZE  20
#define TCP_SIZE 20

	unsigned short  ip_options = 0;
	struct ip      *ip;
	/* this is used to store the output line */
	char            out_buf[8192];

	/* p->len should never be smaller than the smallest possible packet */
	if (p->len < (dlt_len + IP_SIZE + TCP_SIZE))
		return;

	/* cast an ip pointer */
	ip = (struct ip *) (packet + dlt_len);

	/* determine length of ip options (usually 0) */
	ip_options = ip->ip_hl;
	ip_options -= 5;
	ip_options *= 4;

	/* nuke any flags in the offset field */
	ip->ip_off &= 0xFF9F;

	/* toss packets where the fragmentation offset is not 0 */
	if (ip->ip_off != 0)
		return;

	/* Null the out_buf */
	out_buf[0] = 0x00;

	hash_add(hash, pcap_socket, ntohl(ip->ip_src.s_addr), p, packet);
	hash_add(hash, pcap_socket, ntohl(ip->ip_dst.s_addr), p, packet);
}

static char    *
itoa(int in)
{
	static char     buf[16];
	int             i = 15;

	buf[i--] = 0x00;

	while (in >= 10) {
		buf[i--] = ((in % 10) + '0');
		in /= 10;
	}
	buf[i--] = (in + '0');
	/*
	 * The beginning is calculated by the distance from i to the the
	 * character before the end (NULL) The end is 15, so the first
	 * available character is 14.  If the string representation of the
	 * number is one character long, 15 is 0x00, 14 is the digit, and 13
	 * is the current pointer.  Thus, the number should be buf + (i+1) or
	 * i+1
	 */
	return (buf + i + 1);
}

char    *
ntoa(int a)
{
	static char     ret[40];

	ret[0] = 0x00;
	strcat(ret, itoa((a & 0xff000000) >> 24));
	strcat(ret, ".");
	strcat(ret, itoa((a & 0x00ff0000) >> 16));
	strcat(ret, ".");
	strcat(ret, itoa((a & 0x0000ff00) >> 8));
	strcat(ret, ".");
	strcat(ret, itoa(a & 0x000000ff));

	return(ret);
}

char    *
mynet_ntoa(struct in_addr in)
{
	return(ntoa(ntohl(in.s_addr)));
}

/* shut down in a controlled way, close log file, close socket, and exit */
static void
signal_handler(int s)
{
	signalled=s;
	pcap_breakloop(pcap_socket);
}
