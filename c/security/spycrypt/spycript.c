/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 *
 * $Id: spycript.c,v 1.1 2001/06/30 08:35:12 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>

#define BLOCKSIZE 8192

#define EXIT_OK 0
#define EXIT_ERROR 1

struct crypt {
	int fd;
	int size;
	int offset;
	int bufoffset;
	unsigned char buffer[BLOCKSIZE];
};

/*
 * Get a crypt struct from a filename.
 */
struct crypt getCrypt(const char *filename)
{
	struct crypt c;
	struct stat st;
	memset(&c, 0x00, sizeof(c));

	/* The stat may fail, and we don't care, it'll blow up on the open */
	stat(filename, &st);
	c.size=st.st_size;

	c.fd=open(filename, O_RDONLY, 0444);
	if(c.fd<0) {
		perror(filename);
		exit(EXIT_ERROR);
	}

	return(c);
}

/*
 * Close the crypt thing.
 */
void closeCrypt(struct crypt c)
{
	close(c.fd);
}

void getKey(struct crypt *c, unsigned char keyblock[BLOCKSIZE])
{
	int where=0;
	int readsize=0;

	if(c->offset+BLOCKSIZE>c->size) {
		readsize=read(c->fd, keyblock, BLOCKSIZE);
		if(lseek(c->fd, 0, SEEK_SET) < 0) {
			perror("rewind error");
			exit(EXIT_ERROR);
		}
		readsize+=read(c->fd, keyblock+readsize, BLOCKSIZE-readsize);
		c->offset=readsize;
		assert(readsize==BLOCKSIZE);
	} else {
		readsize=read(c->fd, keyblock, BLOCKSIZE);
		c->offset+=readsize;
		assert(readsize==BLOCKSIZE);
	}
}

void decode(struct crypt c, int infile, int outfile)
{
	unsigned char inblock[BLOCKSIZE];
	unsigned char outblock[BLOCKSIZE];
	unsigned char keyblock[BLOCKSIZE];
	int readsize=0;
	int writesize=0;
	int i=0;

	readsize=read(infile, inblock, BLOCKSIZE);
	while(readsize>0) {
		getKey(&c, keyblock);

		for(i=0 ;i<readsize; i++) {
			outblock[i]=inblock[i]^keyblock[i];
		}

		writesize=write(outfile, outblock, readsize);
		assert(writesize==readsize);

		readsize=read(infile, inblock, BLOCKSIZE);
	}
}

void usage(const char *argv0)
{
	printf("Usage:  %s keyfile infile outfile\n", argv0);
	exit(EXIT_ERROR);
}

int main(int argc, char **argv)
{
	struct crypt c;
	int infile=0, outfile=0;

	if(argc<4) {
		usage(argv[0]);
	}

	c=getCrypt(argv[1]);
	infile=open(argv[2], O_RDONLY, 0444);
	if(infile<0) {
		perror(argv[2]);
		exit(EXIT_ERROR);
	}

	outfile=open(argv[3], O_CREAT|O_EXCL|O_WRONLY, 0600);
	if(outfile<0) {
		perror(argv[3]);
		exit(EXIT_ERROR);
	}

	decode(c, infile, outfile);

	close(infile);
	close(outfile);

	return(EXIT_OK);
}
