/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 *
 * $Id: readrandom.c,v 1.2 2001/06/30 09:24:08 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>

#define BLOCKSIZE 8192

void usage(const char *name)
{
	fprintf(stderr, "Usage:  %s size indev outfile\n", name);
	exit(1);
}

int main(int argc, char **argv)
{
	int infile=0, outfile;
	int sizetoread=0;
	int amountread=0, readsize=0;
	int writesize=0;
	unsigned char data[BLOCKSIZE];

	if(argc<4) {
		usage(argv[0]);
	}

	infile=open(argv[2], O_RDONLY, 0444);
	if(infile<0) {
		perror(argv[2]);
		exit(1);
	}

	outfile=open(argv[3], O_WRONLY|O_CREAT|O_EXCL, 0400);
	if(outfile<0) {
		perror(argv[3]);
		exit(1);
	}

	sizetoread=atoi(argv[1]);
	assert(sizetoread>0);

	while(amountread<sizetoread) {
		readsize=-1;
		while(readsize<0) {
			readsize=read(infile, data, BLOCKSIZE);
			if(readsize<0) {
				usleep(2600); /* Wait a bit before reading again (on error) */
			}
		}
		amountread+=readsize;
		writesize=write(outfile, data, readsize);
		assert(writesize==readsize);
	}
	close(infile);
	close(outfile);
}
