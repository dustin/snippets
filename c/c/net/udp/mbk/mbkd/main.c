/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: main.c,v 1.6 1998/10/03 06:21:10 dustin Exp $
 */

#include <config.h>
#include <mbkd.h>
#include <readconfig.h>

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

struct config conf;

static void
writepid(int pid)
{
	FILE   *f;
	int     r;

	r = checkpidfile(conf.pidfile);

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

	if (NULL == (f = fopen(conf.pidfile, "w"))) {
		perror(conf.pidfile);
		return;
	}
	fprintf(f, "%d\n", pid);

	fclose(f);
}

static void
detach(void)
{
	int     pid, i;
	char   *tmp;

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

	tmp = rcfg_lookup(conf.cf, "etc.working_directory");
	if (tmp == NULL)
		tmp = "/";

	chdir(tmp);
	umask(7);
}

void
process_main()
{
	int     s, len, stat, i;
	struct sockaddr_in from;
	struct mbk mbk_packet;
	log_msg("Processing...\n");

	s = getservsocket_udp(1099);

	for (i=0;i<100;i++) {
		len = sizeof(from);

		stat = recvfrom(s, (char *) &mbk_packet, sizeof(mbk_packet),
		    0, (struct sockaddr *) &from, &len);
		if (stat < 0) {
			perror("recvfrom");
		}
		log_debug("Read %d bytes from %s\n", stat,
		    nmc_intToDQ(ntohl(from.sin_addr.s_addr)));
		printf("Read %d bytes\n", stat);
		printf("Length:\t%d\nData:\t%s\n", mbk_packet.len,
		    mbk_packet.data);

		parsepacket(&mbk_packet);
		_hash_dump(mbk_packet.hash);

		if(verify_auth(AUTHDATA, mbk_packet)<0) {
		    printf("Packet failed auth\n");
		} else {
		    printf("Packet passed auth\n");
		}

		hash_destroy(mbk_packet.hash);
		printf("Processed %d packets\n", i+1);
	}
}

int
main(int argc, char **argv)
{
	conf.pidfile = "/tmp/mbkd.pid";
	/* detach(); */
	process_main();
#ifdef MYMALLOC
	_mdebug_dump();
#endif /* MYMALLOC */
	return (0);
}
