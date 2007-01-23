/*
 * Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

static int childpid=0;

void terminateEarly(int sig)
{
	kill(childpid, SIGTERM);
	exit(1);
}

void usage(char *name) {
	fprintf(stderr, "Usage:  %s time command [args]\n", name);
	exit(1);
}

int main(int argc, char **argv) {
	int t=0;
	int status;

	if(argc < 3) {
		usage(argv[0]);
	}

	t=atoi(argv[1]);
	if(t <= 0) {
		usage(argv[0]);
	}

	signal(SIGALRM, terminateEarly);

	childpid=fork();
	switch(childpid) {
		case -1:
			perror("fork");
			exit(1);
			break;
		case 0:
			execvp(argv[2], argv+2);
			perror("exec");
			exit(1);
			break;
		default:
			alarm(t);
			waitpid(childpid, &status, 0);
			exit(WEXITSTATUS(status));
	}
}
