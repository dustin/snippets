/*
 * Copyright (c) 1998  dustin sallings
 *
 * $Id: filter.c,v 1.3 1998/10/04 09:48:46 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <pcap.h>
#include <pwd.h>

#include <mbkd.h>

#include "guinness.h"

static pcap_t *pcap_socket;
static int    dlt_len;

static void filter_packet(u_char *, struct pcap_pkthdr *, u_char *);
static void log_syn(struct ip *, struct tcphdr *);
static char *hostlookup(unsigned int);
static void signal_handler(int);

void process(int flags)
{
    char *interface=NULL;
	char errbuf[PCAP_ERRBUF_SIZE];
	struct bpf_program prog;
	bpf_u_int32 network, netmask;
	char *filter=NULL;

	filter="tcp";

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, signal_handler);
	signal(SIGQUIT, signal_handler);
	signal(SIGTERM, signal_handler);

	/* Attempt to find the interface */
	interface=pcap_lookupdev(errbuf);
	if(interface==NULL) {
	    fprintf(stderr, "pcap_lookupdev: %s\n", errbuf);
		exit(-1);
	}

	if(pcap_lookupnet(interface, &network, &netmask, errbuf) < 0) {
	    fprintf(stderr, "pcap_lookupnet: %s\n", errbuf);
		exit(-1);
	}

	pcap_socket=pcap_open_live(interface, 1024, /* promisc */ 0, 1024, errbuf);

	if(pcap_socket==NULL) {
	    fprintf(stderr, "pcap_open_live: %s\n", errbuf);
		exit(-1);
	}

	switch(pcap_datalink(pcap_socket)) {
	    case DLT_EN10MB:
		    dlt_len=14;
			break;
	    case DLT_SLIP:
		    dlt_len=16;
			break;
		case DLT_PPP:
			dlt_len=4;
		    break;
		case DLT_FDDI:
			fprintf(stderr, "Sorry, can't do FDDI\n");
			signal_handler(-1);
			break;
	    default:
			dlt_len=4;
	}

	if(pcap_compile(pcap_socket, &prog, filter, 1, netmask) < 0) {
	    fprintf(stderr, "pcap_compile: %s\n", errbuf);
		signal_handler(-1);
	}

	if(pcap_setfilter(pcap_socket, &prog) < 0) {
	    fprintf(stderr, "pcap_setfilter: %s\n", errbuf);
		signal_handler(-1);
	}

	fprintf(stderr, "interface: %s, filter: %s\n", interface, filter);

	for(;;)
	    pcap_loop(pcap_socket, -1, (pcap_handler)filter_packet, NULL);
}

/* this is the function that's called when pcap reads a packet */
void filter_packet(u_char *u, struct pcap_pkthdr *p, u_char *packet)
{
    #define IP_SIZE  20
	#define TCP_SIZE 20

	unsigned short ip_options = 0;
	struct ip *ip;
	struct tcphdr *tcp;
	static u_char *align_buf=NULL;

   /* p->len should never be smaller than the smallest possible packet */
   if(p->len < (dlt_len + IP_SIZE + TCP_SIZE)) return;

   /* cast a ip pointer */
   ip = (struct ip *)(packet + dlt_len);

   /* align packet if needed */
   if(align_buf == NULL) align_buf=(u_char *)malloc(4096);
   bcopy((char *)ip, (char *)align_buf, p->len);
   packet = align_buf;
   ip = (struct ip *)align_buf;

   /* lame hack needed for PPP */
   if(ip->ip_p != IPPROTO_TCP) return;

   /* determine length of ip options (usually 0) */
   ip_options = ip->ip_hl;
   ip_options -= 5;
   ip_options *= 4;

   /* cast tcp pointer */
   tcp = (struct tcphdr *)(packet + IP_SIZE + ip_options);

   /* nuke any flags in the offset field */
   ip->ip_off &= 0xFF9F;

   /* toss packets where the fragmentation offset is not 0 */
   if(ip->ip_off != 0) return;

   /* if a syn was recieved, but a ack was not, log the packet */
   if((tcp->th_flags & TH_SYN) && !(tcp->th_flags & TH_ACK))
      log_syn(ip, tcp);
}

static void log_syn(struct ip *ip, struct tcphdr *tcp)
{
    char buf[32];
	MBK *mbk;
	int uid;
	struct in_addr ip_src, ip_dst;
	struct passwd *p;

	mbk=mbk_new("localhost", 1099, "630712e3e78e9ac261f13b8918c1dbdc");
    sprintf(buf, "%u", ntohs(tcp->th_sport));
	mbk->append(mbk, "sport", buf);
    sprintf(buf, "%u", ntohs(tcp->th_dport));
	mbk->append(mbk, "dport", buf);
	mbk->append(mbk, "src", hostlookup(ip->ip_src.s_addr));
	mbk->append(mbk, "dest", hostlookup(ip->ip_dst.s_addr));

	ip_src=ip->ip_src;
	ip_dst=ip->ip_dst;

	if(k_getuid (&ip_dst, tcp->th_dport, &ip_src, tcp->th_sport, &uid) < 0) {
		if(k_getuid (&ip_src, tcp->th_sport,
				     &ip_dst, tcp->th_dport, &uid) < 0) {
			uid=-1; /* If I can't find it, set it to -1 */
		}
	}

    if(uid>=0) {
		sprintf(buf, "%d", uid);
		mbk->append(mbk, "uid", buf);
		if( (p=getpwuid(uid)) != NULL) {
		    mbk->append(mbk, "username", p->pw_name);
		} else {
			log_msg("getpwuid(%d) did not return a valid passwd entry\n", uid);
		}
    }

	mbk->send(mbk);

    mbk->destroy(mbk);
}

static char *hostlookup(unsigned int in)
{
   static char blah[4096];
   struct in_addr i;
   struct hostent *he = NULL;

   i.s_addr = in;
   if(he == NULL)
		strncpy(blah, inet_ntoa(i), 4095);
   else
		strncpy(blah, he->h_name, 4095);
   return (char *)blah;
}

/* shut down in a controlled way, close log file, close socket, and exit */
void signal_handler(int s)
{
   pcap_close(pcap_socket);
   exit(s);
}

