/*
 * Erlang port that listens on a multicast socket and sends the data to stdout.
 *
 * arch-tag: 00E76BA0-9820-11D8-A634-000A957659CC
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

#define HELLO_PORT 6789
#define HELLO_GROUP "225.0.0.37"
#define MSGBUFSIZE 256

void
write_surely(int fd, const void *buf, size_t nbytes)
{
	int written=0;

	written=write(fd, buf, nbytes);
	if(written != nbytes) {
		perror("write");
		exit(1);
	}
}

int
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
		unsigned char psize=0;
		if ((nbytes = recvfrom(fd, msgbuf, MSGBUFSIZE, 0,
			       (struct sockaddr *) & addr, &addrlen)) < 0) {
			perror("recvfrom");
			exit(1);
		}
		/* Send one less byte than received */
		nbytes--;
		psize=(nbytes & 0xff);
		write_surely(STDOUT_FILENO, &psize, 1);
		write_surely(STDOUT_FILENO, msgbuf, nbytes);
		fflush(stdout);
	}
}
