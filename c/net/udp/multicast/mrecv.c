/*
 * listener.c -- joins a multicast group and echoes all data it receives from
 *		the group to its stdout...
 *
 * Antony Courtney,	25/11/94
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>
#include <string.h>
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

main(int argc, char *argv[])
{
	struct sockaddr_in addr;
	int             fd=0, nbytes=0, addrlen=0, reuse=1;
	struct ip_mreq  mreq;
	char            msgbuf[MSGBUFSIZE];

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
	addr.sin_port = htons(HELLO_PORT);

	if(setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char*)&reuse, sizeof(int))<0){
		perror("setsockopt");
	}

	/* bind to receive address */
	if (bind(fd, (struct sockaddr *) & addr, sizeof(addr)) < 0) {
		perror("bind");
		exit(1);
	}
	/* use setsockopt() to request that the kernel join a multicast group */
	mreq.imr_multiaddr.s_addr = inet_addr(HELLO_GROUP);
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
		puts(msgbuf);
	}
}
