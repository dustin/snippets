/*
 * listener.c -- joins a multicast group and echoes all data it receives from
 *		the group to its stdout...
 *
 * Antony Courtney,	25/11/94
 */

#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>
#include <stdio.h>

#ifndef HELLO_PORT
# define HELLO_PORT 6789
#endif
#ifndef HELLO_GROUP
# define HELLO_GROUP "225.0.0.37"
#endif
#ifndef MSGBUFSIZE
# define MSGBUFSIZE 256
#endif

void
usage(const char *prog)
{
	fprintf(stderr, "Usage %s [-v] [-g group] [-p port]\n", prog);
	fprintf(stderr, " default group:  %s\n", HELLO_GROUP);
	fprintf(stderr, " default port:   %d\n", HELLO_PORT);
	exit(1);
}

int
main(int argc, char *argv[])
{
	struct sockaddr_in addr;
	int             fd=0, nbytes=0, addrlen=0, reuse=1, c=0, verbose=0;
	struct ip_mreq  mreq;
	char            msgbuf[MSGBUFSIZE];
	char           *group=HELLO_GROUP;
	int             port=HELLO_PORT;

	/* Parse the args */
	while((c=getopt(argc, argv, "g:p:v")) != -1) {
		switch(c) {
			case 'g':
				group=optarg;
				break;
			case 'p':
				port=atoi(optarg);
				break;
			case 'v':
				verbose++;
				break;
			default:
				usage(argv[0]);
		}
	}

	if(verbose) {
		fprintf(stderr, "Listening on %s:%d\n", group, port);
	}

	/* create what looks like an ordinary UDP socket */
	if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket");
		exit(1);
	}
	/* set up destination address */
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	/* N.B.: differs from * sender */
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port = htons(port);

	if(setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char*)&reuse, sizeof(int))<0){
		perror("setsockopt");
	}

	/* bind to receive address */
	if (bind(fd, (struct sockaddr *) & addr, sizeof(addr)) < 0) {
		perror("bind");
		exit(1);
	}
	/* use setsockopt() to request that the kernel join a multicast group */
	mreq.imr_multiaddr.s_addr = inet_addr(group);
	mreq.imr_interface.s_addr = htonl(INADDR_ANY);
	if(setsockopt(fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq)) < 0){
		perror("setsockopt");
		exit(1);
	}
	/* now just enter a read-print loop */
	addrlen = sizeof(addr);
	for(;;) {
		if ((nbytes = recvfrom(fd, msgbuf, MSGBUFSIZE-1, 0,
			       (struct sockaddr *) & addr, &addrlen)) < 0) {
			perror("recvfrom");
			exit(1);
		}
		msgbuf[nbytes]=0x00;
		printf("%s: %s\n", inet_ntoa(addr.sin_addr), msgbuf);
	}
}
