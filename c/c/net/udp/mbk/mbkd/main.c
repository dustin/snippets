/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: main.c,v 1.8 1998/10/03 08:02:26 dustin Exp $
 */

#include <config.h>
#include <mbkd.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <assert.h>

#include <md5.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#define AUTHDATA "630712e3e78e9ac261f13b8918c1dbdc"
#define MBKD_PID "/tmp/mbkd.pid"

static void
writepid(int pid)
{
	FILE   *f;
	int     r;

	r = checkpidfile(MBKD_PID);

	switch (r) {
	case PID_NOFILE:
		break;
	case PID_STALE:
		puts("Stale PID file found, overriding.");
		break;
	case PID_ACTIVE:
		puts("Active PID file found, exiting...");
		kill(pid, SIGTERM);
		exit(1);
	}

	if (NULL == (f = fopen(MBKD_PID, "w"))) {
		perror(MBKD_PID);
		return;
	}
	fprintf(f, "%d\n", pid);

	fclose(f);
}

static void
detach(void)
{
	int     pid, i;

	pid = fork();

	if (pid > 0) {
		printf("Running on PID %d\n", pid);
		writepid(pid);
		exit(0);
	}
	setsid();

	/* close uneeded file descriptors */

	for (i = 0; i < 256; i++)
		close(i);

	chdir("/");
	umask(7);
}

void
process_main()
{
	int     stat, s, len, i;
	struct sockaddr_in from;
	MBK *mbk_packet;
	log_msg("Processing...\n");

	s = getservsocket_udp(1099);

	for (i=0;i<100;i++) {
		len = sizeof(from);
		mbk_packet=mbk_new(NULL, 0, AUTHDATA);

		stat = recvfrom(s, (char *) &(mbk_packet->pkt),
		    MAXPACKETLEN,
		    0, (struct sockaddr *) &from, &len);
		if (stat < 0) {
			perror("recvfrom");
		}
		log_debug("Read %d bytes from %s\n", stat,
		    nmc_intToDQ(ntohl(from.sin_addr.s_addr)));

		/*
		printf("Read %d bytes\n", stat);
		printf("Length:\t%d\nData:\t%s\n", mbk_packet->pkt.len,
		    mbk_packet->pkt.data);
        */

		mbk_packet->parse(mbk_packet);
		/*
		_hash_dump(mbk_packet->hash);
		*/

		if(mbk_packet->verify(mbk_packet)<0) {
		    printf("Packet failed auth\n");
		} else {
		    printf("Packet passed auth\n");
		}

        mbk_packet->destroy(mbk_packet);
		printf("Processed %d packets\n", i+1);
	}
}

int
main(int argc, char **argv)
{
	/* detach(); */
	process_main();
#ifdef MYMALLOC
	_mdebug_dump();
#endif /* MYMALLOC */
	return (0);
}
