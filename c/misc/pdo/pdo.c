/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: pdo.c,v 1.3 1998/10/18 21:42:38 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <getopt.h>
#include <assert.h>

/* spawn a child to do the work */
void
dowork(char *job)
{
	int     pid, ret;
	pid = fork();
	assert(pid >= 0);
	if (pid == 0) {
		printf("%05d: system(%s)\n", getpid(), job);
		system(job);
		exit(0);
	}
}

char   *
kw(char *in)
{
	/* bounds checking */
	if (strlen(in) == 0)
		return (in);

	while (isspace(in[strlen(in) - 1])) {
		/* bounds checking */
		if (strlen(in) == 0)
			return (in);

		in[strlen(in) - 1] = 0x00;
	}

	return (in);
}

void
usage(char *progname)
{
	printf("Parallel Do\n"
	    "$Id: pdo.c,v 1.3 1998/10/18 21:42:38 dustin Exp $\n"
	    "Usage:  %s [-j n_jobs] jobfile\n", progname);
}

int
main(int argc, char **argv)
{
	extern char *optarg;
	extern int optind;
	int     pid, c, i, maxkids = 1, index = 0, size = 64;
	char    buf[8192];

	/* the job holder */
	char **jobs = 0;
	int job_index = 0;

	FILE   *f;

	while ((c = getopt(argc, argv, "j:")) != -1) {
		switch (c) {
		case 'j':
			maxkids = atoi(optarg);
			break;
		case '?':
			usage(argv[0]);
			exit(1);
		}
	}

	if (optind >= argc) {
		usage(argv[0]);
		exit(1);
	}
#define LAPPEND(a) if(index == size-1) { \
        jobs=realloc(jobs, (size<<=1)*sizeof(char *)); \
            assert(jobs); \
    } \
    jobs[index++]=strdup(a);

	f = fopen(argv[optind], "r");
	if (f == NULL) {
		perror(argv[optind]);
		exit(1);
	}
	jobs = (char **) malloc(size * sizeof(char *));
	assert(jobs);

	for (;;) {
		fgets(buf, 8190, f);
		if (feof(f))
			break;
		assert(strlen(buf) < 8190);	/* long ass line */
		kw(buf);
		if (buf[0] != '#') {
			LAPPEND(buf);
		}
	}

	printf("Found %d jobs, maxkids is %d\n", index, maxkids);

	/* fork off the initial kids */
	for (job_index = 0; job_index < maxkids && jobs[job_index]; job_index++) {
		dowork(jobs[job_index]);
	}

	/* Do the processing */
	while (jobs[job_index]) {
		pid = waitpid(0, NULL, WUNTRACED);
		if (pid > 0) {
			printf("%05d finished\n", pid);
			dowork(jobs[job_index]);
			job_index++; /* get the next job */
		}
	}
	/* wait for the last children */
	for(i=0; i<maxkids; i++) {
		pid = waitpid(0, NULL, WUNTRACED);
		if(pid>0) {
			printf("%05d finished\n", pid);
		} else {
			break; /* we're done */
		}
	}
}
