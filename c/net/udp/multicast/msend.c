/*
 * Basic multicast string sender.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>

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
	fprintf(stderr, "Usage %s [-v] [-f] [-s] [-g group] [-p port] msg\n", prog);
	fprintf(stderr, " default group:  %s\n", HELLO_GROUP);
	fprintf(stderr, " default port:   %d\n", HELLO_PORT);
	fprintf(stderr, " -v turns on verbose mode\n");
	fprintf(stderr, " -s turns on netstring encoding of arguments\n");
	exit(1);
}

int
main(int argc, char *argv[])
{
	struct sockaddr_in addr;
	int             i=0, nsenc=0, fd=0, nbytes=0, addrlen=0, c=0, verbose=0;
	int             shouldflush=0;
	struct ip_mreq  mreq;
	char            msgbuf[MSGBUFSIZE];
	char           *group=HELLO_GROUP;
	int             port=HELLO_PORT;
	char           *msg=NULL;

	/* Parse the args */
	while((c=getopt(argc, argv, "g:p:vs")) != -1) {
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
			case 's':
				nsenc=1;
				break;
			default:
				usage(argv[0]);
		}
	}

	if(optind >= argc) {
		usage(argv[0]);
	}

	/* create what looks like an ordinary UDP socket */
	if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket");
		exit(1);
	}
	/* set up destination address */
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = inet_addr(group);
	addr.sin_port = htons(port);

	/* Count the bytes for allocation */
	for(i=optind; i<argc; i++) {
		nbytes+=strlen(argv[optind]);
		if(nsenc) {
			/* Add enough space to deal with the size stuff around the
			 * netstring */
			nbytes += 16;
		} else {
			/* Add enough space for a space */
			nbytes++;
		}
	}

	/* One more for the road */
	nbytes++;

	msg=calloc(nbytes, sizeof(char));

	/* Add the actual data */
	for(i=optind; i<argc; i++) {
		if(nsenc) {
			char *buf=NULL;
			int buflen=strlen(argv[i]) + 16;
			buf=calloc(buflen+1, sizeof(char));
			snprintf(buf, buflen, "%d:%s,", strlen(argv[i]), argv[i]);
			strcat(msg, buf);
			free(buf);
		} else {
			strcat(msg, argv[i]);
			strcat(msg, " ");
		}
	}

	if(verbose) {
		fprintf(stderr, "Sending ``%s'' on %s:%d\n", msg, group, port);
	}

	/* now just sendto() our destination! */
	if (sendto(fd, msg, strlen(msg), 0, (struct sockaddr *) & addr,
		sizeof(addr)) < 0) {
		perror("sendto");
		exit(1);
	}
}
