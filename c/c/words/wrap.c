/*
 *  Copyright (c) 2000 Dustin Sallings
 *
 * $Id: wrap.c,v 1.2 2000/10/30 10:27:49 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <assert.h>

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

int main(int argc, char **argv)
{
	char *ibuffer=NULL;
	char *obuffer=NULL;
	int infile=0, outfile=0;
	int i=0, j=0;
	int linechars=0;
	int current_offset=0;
	int file_len=0;
	int blocksize=1024*1024; /* Give it a meg */
	int wrapmargin=76;
	int outsize=0;
	struct stat st;

	if(argc<3) {
		fprintf(stderr, "Too few arguments.\nUsage:  %s infile outfile",
			argv[0]);
		exit(1);
	}

	/* Find out how big the file is. */
	if( lstat(argv[1], &st) < 0 ) {
		perror(argv[1]);
		exit(1);
	}

	file_len=st.st_size;

	/* Open the input file */
	if( (infile=open(argv[1], O_RDONLY, 0)) < 0 ) {
		perror(argv[1]);
		exit(1);
	}

	/* Open the output file */
	if( (outfile=open(argv[2], O_CREAT | O_RDWR, 0644)) < 0) {
		perror(argv[2]);
		exit(1);
	}

	/* Calculate possible output buffer size, add a K just in case */
	outsize=(blocksize/wrapmargin)+blocksize+1024;
	/* Allocate the output buffer */
	obuffer=calloc(outsize, sizeof(char));
	assert(obuffer);

	while(current_offset<file_len) {

		/* Make sure we don't go past the end of the file */
		if(current_offset+blocksize>file_len) {
			blocksize=file_len-current_offset;
		}

		/* Map it */
		ibuffer=mmap(NULL, blocksize, PROT_READ, MAP_FILE|MAP_SHARED, infile,
			current_offset);

		/* Check it */
		if(ibuffer==NULL) {
			perror("mmap");
			exit(1);
		}

		/* Wrap it, yo */
		for(i=0, j=0; i<blocksize; i++) {
			if(ibuffer[i]=='\n') {
				linechars=0;
			} else {
				if(linechars++>wrapmargin) {
					obuffer[j++]='\n';
					linechars=0;
				}
			}
			obuffer[j++]=ibuffer[i];
		}

		/* Write it */
		if(write(outfile, obuffer, j)<0) {
			perror("write");
			exit(1);
		}

		/* Unmap it */
		munmap(ibuffer, blocksize);

		/* Go to the next offset */
		current_offset+=blocksize;
	}

	exit(0);
}
