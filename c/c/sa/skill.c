/*
 * Copyright (c) 1999 Beyond.com, Dustin Sallings <dustin@beyond.com>
 *
 * $Id: skill.c,v 1.1 1999/05/04 17:58:16 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#define ISRUNNING(a) (kill(a, 0)==0)

/* get the owner of a process */
uid_t owner(int pid)
{
	struct stat st;
	char path[80];
	int size;

	size=snprintf(path, 79, "/proc/%d", pid);
	if(size>79) {
		return(-1);
	}

	/* Null terminate it, just in case */
	path[size]=0x00;

	if(stat(path, &st)<0) {
		return(-1);
	}

	return(st.st_uid);
}

int main(int argc, char **argv)
{
	int pid, ret, i, o, blah;

	/* Flip through the arguments looking for pids */
	for(i=1;i<argc; i++) {

		/* What's the PID? */
		pid=atoi(argv[i]);
		o=owner(pid);

		if( (pid<1) || (o==-1) ) {
			/* We're not going to worry about process groups */
			blah=fprintf(stderr, "Warning: %s is not a valid PID\n", argv[i]);
		} else {

			syslog(LOG_NOTICE|LOG_AUTH,
				"superkill: %s (%d) wishes to kill %d, owned by %d\n",
				getlogin(), getuid(), pid, o);

			/* We start with an INT, same as ^C */
			if( kill(pid, SIGINT) <0 ) {
				perror(argv[i]);
			}

			/* Give it a couple of seconds to die */
			blah=sleep(2);

			if( ISRUNNING(pid) ) {

				/* Do this if it didn't kill the first time */
				if( kill(pid, SIGKILL) <0 ) {
					perror(argv[i]);
				}

				/* Check to see if it survived the 9 */
				if(ISRUNNING(pid)) {
					syslog(LOG_NOTICE|LOG_AUTH,
						"superkill: %s (%d) could not kill %d\n",
						getlogin(), getuid(), pid);
					blah=fprintf(stderr, "Couldn't kill %d\n", pid);
					ret=1;
				}
			}
		}
	}

	closelog();

	return(ret);
}
