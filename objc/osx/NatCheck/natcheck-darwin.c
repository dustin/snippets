
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <netdb.h>
#include <sys/types.h>
#define _BSD_SOCKLEN_T_
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifdef HAVE_STDINT
#include <stdint.h>
#else
#define uint16_t unsigned short
#define uint32_t unsigned long
#endif

#include <natcheck.h>

#define HOST_NAME_MAX	256

#define NTRIES		5

#define SERV1		"pdos.lcs.mit.edu"
#define SERV2		"tears.lcs.mit.edu"
#define SERV3		"sure.lcs.mit.edu"

#define SERVPORT	9856
#define CLIPORT		9857

#define REQMAGIC	0x76849268
#define REPLMAGIC	0x01967293

struct request {
	uint32_t magic;
};

struct reply {
	uint32_t magic;
	struct in_addr pubaddr;
	uint16_t pubport;
};

int verbose = 0;

void getaddr(const char *hostname, struct in_addr *addr)
{
	struct hostent *h;

	h = gethostbyname(hostname);
	if (h == 0)
		perrordie(hostname);
	if (h->h_addrtype != AF_INET) {
		fprintf(stderr, "%s: unexpected address type %d\n",
				hostname, h->h_addrtype);
	}
	*addr = *(struct in_addr*)(h->h_addr);
}

void lookup(int n, const char *hostname, struct sockaddr_in *sin)
{
	getaddr(hostname, &sin->sin_addr);

	sin->sin_family = AF_INET;
	sin->sin_port = htons(SERVPORT);

	if (verbose)
		fprintf(stderr, "server %d: %s at %s:%d\n",
				n, hostname, inet_ntoa(sin->sin_addr),
				ntohs(sin->sin_port));
}

int checkmsg(int servn, struct reply *rp, struct sockaddr_in *sin,
		struct sockaddr_in *servsin, struct sockaddr_in *mysin)
{
	if (sin->sin_addr.s_addr != servsin->sin_addr.s_addr ||
			sin->sin_port != servsin->sin_port)
		return 0;

	if (verbose)
		fprintf(stderr, "Server %d reports my address as %s:%d\n",
				servn, inet_ntoa(rp->pubaddr),
				ntohs(rp->pubport));

	if (mysin->sin_addr.s_addr != INADDR_ANY) {
		/* We already received a response from this server */

		if (mysin->sin_addr.s_addr != rp->pubaddr.s_addr ||
				mysin->sin_port != rp->pubport) {
			fprintf(stderr, "Server %d reports my address "
					"INCONSISTENTLY as %s:%d and %s:%d\n",
					servn,
					inet_ntoa(mysin->sin_addr),
					ntohs(mysin->sin_port),
					inet_ntoa(rp->pubaddr),
					ntohs(rp->pubport));
		}

		return 1;
	}

	/* record the address reported by the server */
	mysin->sin_family = AF_INET;
	mysin->sin_addr = rp->pubaddr;
	mysin->sin_port = rp->pubport;

	return 1;
}

struct check_result performNatCheck(int ntries)
{
    static struct check_result rv;
	int sock;
	struct sockaddr_in sin, sin1, sin2, sin3, my1, my2, my3;
	uint16_t myport;
	socklen_t sinlen;
	struct request rq;
	struct reply rp;
	fd_set fds;
	struct timeval tv;
	int i, rc, fl;

	/* Set the nat_type to UNKNOWN */
    rv.nat_type=UNKNOWN;
	/* Lookup the three well-known servers */
	lookup(1, SERV1, &sin1);
	lookup(2, SERV2, &sin2);
	lookup(3, SERV3, &sin3);

	/* Make me a socket */
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0)
		perrordie("socket");

	/* Bind the socket to our well-known client port number */
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(CLIPORT);
	if (bind(sock, (struct sockaddr*)&sin, sizeof(sin)) < 0)
		perrordie("bind");

	/* Find the port number we were given */
	sinlen = sizeof(sin);
	if (getsockname(sock, (struct sockaddr*)&sin, &sinlen) < 0)
		perrordie("getsockname");
	myport = sin.sin_port;
	if (verbose)
		printf("Local port number: %d\n", ntohs(myport));

	/* Place our socket in nonblocking mode */
	fl = fcntl(sock, F_GETFL);
	if (fl < 0)
		perrordie("fcntl");
	if (fcntl(sock, F_SETFL, fl | O_NONBLOCK) < 0)
		perrordie("fcntl");

	/* Ping the servers 10 times */
	my1.sin_addr.s_addr = INADDR_ANY;
	my2.sin_addr.s_addr = INADDR_ANY;
	my3.sin_addr.s_addr = INADDR_ANY;
	for (i = 1; i <= ntries; i++) {

		fprintf(stderr, "Request %d of %d...\n", i, ntries);

		rq.magic = htonl(REQMAGIC);
		if (sendto(sock, &rq, sizeof(rq), 0, (struct sockaddr*)&sin1,
					sizeof(sin1)) < 0)
			perrordie("sendto");
		if (sendto(sock, &rq, sizeof(rq), 0, (struct sockaddr*)&sin2,
					sizeof(sin2)) < 0)
			perrordie("sendto");

		while (1) {
			FD_ZERO(&fds);
			FD_SET(sock, &fds);
			tv.tv_sec = 1;	/* wait 1 sec between retries */
			tv.tv_usec = 0;
			rc = select(sock+1, &fds, 0, 0, &tv);
			if (rc < 0)
				perrordie("select");
			if (rc == 0) /* timeout */
				break;

			/* Receive all available messages */
			while (1) {
				sinlen = sizeof(sin);
				rc = recvfrom(sock, &rp, sizeof(rp), 0,
						(struct sockaddr*)&sin,
						&sinlen);
				if (rc < 0) {
				       if (errno == EAGAIN ||
						       errno == EWOULDBLOCK)
						break;
				       perrordie("recvmsg");
				}

				if (rc < sizeof(rp)) {
					if (verbose)
						fprintf(stderr,
							"received runt packet\n");
					continue;
				}
				if (rp.magic != htonl(REPLMAGIC)) {
					if (verbose)
						fprintf(stderr,
							"received reply with bad magic value\n");
					continue;
				}

				/* Check the source of the packet */
				if (!checkmsg(1, &rp, &sin, &sin1, &my1) &&
					!checkmsg(2, &rp, &sin, &sin2, &my2) &&
					!checkmsg(3, &rp, &sin, &sin3, &my3)) {
					if (verbose)
						fprintf(stderr,
							"stray packet from %s\n", inet_ntoa(sin.sin_addr));
					continue;
				}
			}
		}
	}

	if (my1.sin_addr.s_addr == INADDR_ANY) {
		fprintf(stderr, "Could not contact server 1 (%s at %s)\n",
				SERV1, inet_ntoa(sin1.sin_addr));
		exit(1);
	}
	if (my2.sin_addr.s_addr == INADDR_ANY) {
		fprintf(stderr, "Could not contact server 2 (%s at %s)\n",
				SERV2, inet_ntoa(sin1.sin_addr));
		exit(1);
	}

	/* Check what type of NAT we have */
	if (my1.sin_port == myport && my2.sin_port == myport) {
		rv.nat_type = BASIC;
	} else {
		rv.nat_type = PNAT;
	}

	/* Check for public address/port number consistency */
	if (my1.sin_addr.s_addr == my2.sin_addr.s_addr &&
			my1.sin_port == my2.sin_port) {
		rv.consistent=1;
	} else {
		rv.consistent=0;
	}

	/* See if messages from SERV3 ("attacker") got through */
	if (my3.sin_addr.s_addr != INADDR_ANY) {
		rv.unsolicitedFilt=0;
	} else {
		rv.unsolicitedFilt=1;
	}

	close(sock);

	return(rv);
}

#ifdef NATCHECK_TEXT_MAIN
int main(int argc, char **argv)
{
	struct check_result res;

	res=performNatCheck(NTRIES);

	printf("\nRESULTS:\n");

	/* Check what type of NAT we have */
	switch(res.nat_type) {
		case UNKNOWN:
			printf("Address translation:           UNKNOWN \n");
			break;
		case BASIC:
			printf("Address translation:           Basic NAT "
					"(IP address only)\n");
			break;
		case PNAT:
			printf("Address translation:           NAPT "
					"(Network Address and Port Translation)\n");
			break;
	}

	/* Check for public address/port number consistency */
	if (res.consistent == 1) {
		printf("Consistent translation:        YES "
				"(GOOD for peer-to-peer)\n");
	} else {
		printf("Consistent translation:        NO  "
				"(BAD for peer-to-peer)\n");
	}

	/* See if messages from SERV3 ("attacker") got through */
	if (res.unsolicitedFilt == 1) {
		printf("Unsolicited messages filtered: YES "
				"(GOOD for security)\n");
	} else {
		printf("Unsolicited messages filtered: NO  "
				"(BAD for security)\n");
	}

}
#endif /* NATCHECK_TEXT_MAIN */
