/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: rrdrstat.c,v 1.2 2002/02/01 10:32:28 dustin Exp $
 */

#include <rpcsvc/rstat.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <rrd.h>

static void
process(const char *host, statstime *stat)
{
	char buf[8192];

	/* usr wio sys idl pgin pgout intr ipkts opkts coll errors cs load */
	snprintf(buf, sizeof(buf),
		"update %s.rrd N:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%f:%f:%f",
		host,
		stat->cp_time[0], stat->cp_time[1],
		stat->cp_time[2], stat->cp_time[3],
		stat->v_pgpgin, stat->v_pgpgout,
		stat->v_intr,
		stat->if_ipackets, stat->if_opackets,
		stat->if_collisions, stat->if_oerrors,
		stat->v_swtch,
		((float)stat->avenrun[0] /FSCALE),
		((float)stat->avenrun[1] /FSCALE),
		((float)stat->avenrun[2] /FSCALE));
	puts(buf);
	fflush(stdout);
}

static int
processHost(const char *host)
{
	CLIENT *client;
	statstime *stat;
	char *arg;
	int rv=0;

	client=clnt_create((char *)host, RSTATPROG, RSTATVERS_TIME, "udp");
	if(client==NULL) {
		perror("client creation failed");
		rv=-1;
	} else {
		stat = rstatproc_stats_3((void *)&arg, client);
		if(stat==NULL) {
			fprintf(stderr, "Call failed.\n");
			rv=-1;
		} else {
			process(host, stat);
		}
		clnt_destroy(client);
	}

	return(rv);
}

int
main(int argc, char **argv)
{
	int i=0;

	for(;;) {
		for(i=1; i<argc; i++) {
			processHost(argv[i]);
		}
		sleep(60);
	}
}
