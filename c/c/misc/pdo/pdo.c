/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: pdo.c,v 1.1 1998/10/18 09:59:22 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <getopt.h>
#include <assert.h>

/* Work to do is global so the signal handler can handle it. */
static char **jobs=0;
static int job_index=0;

int dowork()
{
	int pid, ret;
	if(jobs[job_index]) {
		pid=fork();
		assert(pid>=0);
		if(pid==0) {
			signal(SIGCHLD, SIG_DFL);
			printf("system(%s)\n", jobs[job_index]);
			system(jobs[job_index]);
			exit(0);
		} else {
			job_index++;
			ret=0;
		}
	} else {
		ret=-1;
	}
	return(ret);
}

void will_work_for_food(int sig)
{
	int pid;
	pid=1;
	while(pid>0) {
		pid=waitpid(0, NULL, WUNTRACED | WNOHANG);
		if(pid>0)
			dowork();
	}
	signal(SIGCHLD, will_work_for_food);
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

void usage(char *progname)
{
	printf("Parallel Do\n"
	       "$Id: pdo.c,v 1.1 1998/10/18 09:59:22 dustin Exp $\n"
		   "Usage:  %s [-j n_jobs] jobfile\n", progname);
}

int main(int argc, char **argv)
{
	extern char *optarg;
	extern int optind;
	int c, i, maxkids=1, index=0, size=64;
	char buf[8192];
	FILE *f;

	while( (c=getopt(argc, argv, "j:")) !=-1) {
		switch(c) {
			case 'j':
				maxkids=atoi(optarg);
				break;
			case '?':
				usage(argv[0]);
				exit(1);
		}
	}

	if(optind>=argc) {
		usage(argv[0]);
		exit(1);
	}

#define LAPPEND(a) if(index == size-1) { \
        jobs=realloc(jobs, (size<<=1)*sizeof(char *)); \
            assert(jobs); \
    } \
    jobs[index++]=strdup(a);

	f=fopen(argv[optind], "r");
	if(f==NULL) {
		perror(argv[optind]);
		exit(1);
	}

	jobs = (char **) malloc(size * sizeof(char *));
	assert(jobs);

	for(;;) {
		fgets(buf, 8190, f);
		if(feof(f))
			break;
		assert(strlen(buf)<8190); /* long ass line */
		kw(buf);
		if(buf[0]!='#') {
			LAPPEND(buf);
		}
	}

	signal(SIGCHLD, will_work_for_food);

	/* fork off the initial kids */
	for(i=0; i<maxkids; i++) {
		if(dowork()<0)
			break;
	}

	/* Rest of the program just sits here, letting the signal handler do
	 * the processing.
	 */
	while(jobs[job_index]) {
		pause();
	}
}
