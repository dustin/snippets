/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 *
 * $Id: readrandom.c,v 1.1 2001/06/30 09:09:06 dustin Exp $
 */

#include <stdio.h>
#include <fcntl.h>
#include <assert.h>

#define BLOCKSIZE 8192

int main(int argc, char **argv)
{
	int infile=0, outfile;
	int sizetoread=0;
	int amountread=0, readsize=0;
	int writesize=0;
	unsigned char data[BLOCKSIZE];

	assert(argc>3);

	infile=open(argv[2], O_RDONLY, 0444);
	assert(infile>=0);

	outfile=open(argv[3], O_WRONLY|O_CREAT|O_EXCL, 0400);
	assert(outfile>=0);

	sizetoread=atoi(argv[1]);
	assert(sizetoread>0);

	while(amountread<sizetoread) {
		readsize=-1;
		while(readsize<0) {
			readsize=read(infile, data, BLOCKSIZE);
		}
		amountread+=readsize;
		writesize=write(outfile, data, readsize);
		assert(writesize==readsize);
	}
	close(infile);
	close(outfile);
}
