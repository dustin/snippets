/*
 * Copyright (c) 1998  dustin sallings
 *
 * $Id: filter.c,v 1.1 2000/07/29 10:33:39 dustin Exp $
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

#include "acct.h"

static pcap_t  *pcap_socket = NULL;
static int      dlt_len = 0;

static void     filter_packet(u_char *, struct pcap_pkthdr *, u_char *);
static char    *log_tcp(struct ip *, struct tcphdr *);
static char    *log_udp(struct ip *, struct udphdr *);
static char    *log_ip(struct ip * ip);
static void     showStats();
static char    *hostlookup(char *buf, unsigned int);
static void     signal_handler(int);
static char    *itoa(int in);


void
process(int flags, char *filter)
{
	char           *interface = NULL;
	char            errbuf[PCAP_ERRBUF_SIZE];
	struct bpf_program prog;
	bpf_u_int32     network, netmask;
	int             flagdef;

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, signal_handler);
	signal(SIGQUIT, signal_handler);
	signal(SIGTERM, signal_handler);

	/* Attempt to find the interface */
	interface = pcap_lookupdev(errbuf);
	if (interface == NULL) {
		fprintf(stderr, "pcap_lookupdev: %s\n", errbuf);
		exit(-1);
	}
	if (pcap_lookupnet(interface, &network, &netmask, errbuf) < 0) {
		fprintf(stderr, "pcap_lookupnet: %s\n", errbuf);
		exit(-1);
	}
	if (flags & FLAG_BIT(FLAG_PROMISC))	/* find out if we want to be
						 * promisc */
		flagdef = 1;
	else
		flagdef = 0;

	pcap_socket = pcap_open_live(interface, 768, flagdef, 1000, errbuf);

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
	fprintf(stderr, "interface: %s, filter: %s, promiscuous: %s\n",
	interface, filter, (flags & FLAG_BIT(FLAG_PROMISC)) ? "yes" : "no");

	for (;;) {
		pcap_loop(pcap_socket, 100, (pcap_handler) filter_packet, NULL);
		showStats();
	}
}

static void
showStats()
{
	struct pcap_stat stats;
	if (pcap_stats(pcap_socket, &stats) == 0) {
		printf("# Processed %d packets, dropped %d\n",
		       stats.ps_recv, stats.ps_drop);
	} else {
		printf("# Error getting pcap statistics.\n");
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

	switch (ip->ip_p) {
	case IPPROTO_TCP:{
			struct tcphdr  *tcp;
			tcp = (struct tcphdr *) ( (char *)ip + IP_SIZE + ip_options);
			strcat(out_buf, log_tcp(ip, tcp));
			break;
		}
	case IPPROTO_UDP:{
			struct udphdr  *udp;
			udp = (struct udphdr *) ( (char *)ip + IP_SIZE + ip_options);
			strcat(out_buf, log_udp(ip, udp));
			break;
		}
	default:{
			strcpy(out_buf, log_ip(ip));
			break;
		}
	}
	assert(strlen(out_buf) < sizeof(out_buf));
	fputs(out_buf, stdout);
}

static char    *
hostlookup(char *buf, unsigned int in)
{
	struct in_addr  i;
	assert(buf);
	i.s_addr = in;
	strcpy(buf, inet_ntoa(i));
	return buf;
}

static char    *
log_ip(struct ip * ip)
{
	char            saddr[4096], daddr[4096];
	static char     ret[8192];

	assert(ip);

	sprintf(ret, "time=%d s=%s d=%s l=%d PROTO=%d\n",
		(int) time(NULL),
		hostlookup(saddr, ip->ip_src.s_addr),
		hostlookup(daddr, ip->ip_dst.s_addr),
		ntohs(ip->ip_len),
		ip->ip_p
		);
	return (ret);
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

static char *mynet_ntoa(struct in_addr in)
{
	static char ret[40];
	int a=0;

	a=ntohl(in.s_addr);

	ret[0]=0x00;
	strcat(ret, itoa( (a & 0xff000000) >> 24));
	strcat(ret, ".");
	strcat(ret, itoa( (a & 0x00ff0000) >> 16));
	strcat(ret, ".");
	strcat(ret, itoa( (a & 0x0000ff00) >> 8));
	strcat(ret, ".");
	strcat(ret, itoa(a & 0x000000ff));

	return(ret);
}

static char    *
log_tcp(struct ip * ip, struct tcphdr * tcp)
{
	static char     ret[8192];

	assert(ip);
	assert(tcp);

	/* Time */
	strcpy(ret, "time=");
	strcat(ret, itoa(time(NULL)));

	/* Source address */
	strcat(ret, " s=");
	strcat(ret, mynet_ntoa(ip->ip_src));

	/* Destination address */
	strcat(ret, " d=");
	strcat(ret, mynet_ntoa(ip->ip_dst));

	/* Length */
	strcat(ret, " l=");
	strcat(ret, itoa(ntohs(ip->ip_len)));

	/* Protocol and source port */
	strcat(ret, " PROTO=TCP sp=");
	strcat(ret, itoa(ntohs(tcp->th_sport)));

	/* destination port */
	strcat(ret, " dp=");
	strcat(ret, itoa(ntohs(tcp->th_dport)));

	strcat(ret, "\n");

	return (ret);
}

static char    *
log_udp(struct ip * ip, struct udphdr * udp)
{
	char            saddr[4096], daddr[4096];
	static char     ret[8192];
	assert(ip);
	assert(udp);
	sprintf(ret, "time=%d s=%s d=%s l=%d PROTO=UDP sp=%d dp=%d\n",
		(int) time(NULL),
		hostlookup(saddr, ip->ip_src.s_addr),
		hostlookup(daddr, ip->ip_dst.s_addr),
		ntohs(ip->ip_len),
		ntohs(udp->uh_sport), ntohs(udp->uh_dport));
	return (ret);
}

/* shut down in a controlled way, close log file, close socket, and exit */
static void
signal_handler(int s)
{
	pcap_close(pcap_socket);
	exit(s);
}
