/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: isgzip.c,v 1.1 1998/11/18 07:23:05 dustin Exp $
 *
 * How to tell if a file is gzipped or not.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define GZMAGIC "\037\213"

int main(int argc, char **argv)
{
	unsigned char buf[4];
	int fd, bytes, ret=-1;

	if(argc<2) {
		fprintf(stderr, "No filename given\n");
		exit(-1);
	}

	fd=open(argv[1], O_RDONLY, 0444);
	if(fd<0) {
		perror(argv[1]);
		exit(-1);
	}

	bytes=read(fd, &buf, 2);
	if(bytes<2) {
		fprintf(stderr, "Could not read two bytes from %s\n", argv[1]);
		close(fd);
		exit(-1);
	}

	if( (memcmp(buf, GZMAGIC, 2)==0 )) {
		ret=0;
	} else {
		ret=1;
	}

	close(fd);
	return(ret);
}
