/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: passwordhander.c,v 1.4 2002/07/09 16:36:20 dustin Exp $
 *
 * Read a file and copy it to a new file descriptor into a pipe so's that a
 * subprocess can read it on a specific file descriptor.  The subprocess
 * will be created by shifting the argv over two positions.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <assert.h>

/* Exit with the child's exit status, if we got one */
static void
sigChildHandler(int sig)
{
	int status=0;
	int myexitcode=0;
	pid_t p=0;

	p=wait(&status);

	if(p>=0) {
		myexitcode=WEXITSTATUS(status);
	}

	exit(myexitcode);
}

/* Copy the contents of the src fd to the dest fd until no more bytes can
 * be read from the src fd */
static void
copyFile(int src, int dest)
{
	char buf[4096];
	size_t bytesW=0, bytesR=1;

	while(bytesR>0) {
		bytesR=read(src, &buf, sizeof(buf));
		if(bytesR>0) {
			bytesW=write(dest, buf, bytesR);
			assert(bytesW == bytesR);
		} else if(bytesR<0) {
			perror("read");
		} else {
			/* Zero means we're done */
		}
	}
}

static void
usage(char *name)
{
	fprintf(stderr, "Usage:  %s filename command [options]\n", name);
	fprintf(stderr, "  The contents of filename will be passed to command "
					"on file descriptor 4.\n");
	exit(1);
}

int main(int argc, char **argv)
{
	pid_t p=0;
	int i=0;
	int pipeparts[2];
	int readFd=-1;

	/* Make sure there are enough arguments */
	if(argc<=2) {
		usage(argv[0]);
	}

	/* Setup signal handlers */
	signal(SIGCHLD, sigChildHandler);

	/* Close all (most) file descriptors above stderr */
	for(i=3; i<getdtablesize(); i++) {
		close(i); /* Ignore the result */
	}

	/* Open the file */
	readFd=open(argv[1], O_RDONLY);
	if(readFd<0) {
		perror(argv[1]);
		exit(1);
	}

	/* create the pipe */
	if(pipe(pipeparts) < 0) {
		perror("pipe");
		exit(1);
	}

	/* Fork and do the stuff */
	p=fork();
	switch(p) {
		case -1:
			perror("fork");
			exit(1);
			break;
		case 0:
			/* Close the write end */
			close(pipeparts[1]);
			/* Make sure we're going to write to descriptor 4 */
			if(pipeparts[0] != 4) {
				if(dup2(pipeparts[0], 4) < 0) {
					perror("dup2");
					exit(1);
				}
			}
			/* Call the real program */
			execvp(argv[2], argv+2);
			/* If it gets this far, it didn't run the command */
			perror(argv[2]);
			break;
		default:
			/* Close the read end */
			close(pipeparts[0]);
			copyFile(readFd, pipeparts[1]);
			/* Wait for a signal */
			pause();
			break;
	}

	/* Unreachable */
	exit(1);
}
