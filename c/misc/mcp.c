/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: mcp.c,v 1.1 1999/02/05 05:40:24 dustin Exp $
 */

#include <stdio.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

int main(int argc, char **argv) {
	struct stat st;
	int in, out, len;
	char *cin, *cout;
	char buf;

	if(argc<3) {
		fprintf(stderr, "Too few arguments.\n");
	}

	if( lstat(argv[1], &st) < 0 ) {
		perror(argv[1]);
		exit(1);
	}

	len=st.st_size;

	if( (in=open(argv[1], O_RDONLY, 0)) < 0 ) {
		perror(argv[1]);
		exit(1);
	}

	if( (out=open(argv[2], O_CREAT | O_RDWR, 0644)) < 0) {
		perror(argv[2]);
		exit(1);
	}

	if( lseek(out, (len-1), SEEK_SET) != (len-1)) {
		perror("lseek");
		exit(1);
	}
	
	if( write(out, &buf, 1) < 1 ) {
		perror("write");
		exit(1);
	}

	if( lseek(out, 0, SEEK_SET) != 0) {
		perror("lseek");
		exit(1);
	}

	cin=mmap(NULL, len, PROT_READ, MAP_FILE|MAP_SHARED, in, 0);
	if(cin==NULL) {
		perror("mmap");
		exit(1);
	}

	cout=mmap(NULL, len, PROT_READ|PROT_WRITE, MAP_FILE|MAP_SHARED, out, 0);
	if(cout==NULL) {
		munmap(cin, len);
		perror("mmap");
		exit(1);
	}

	memcpy(cout, cin, len);

	munmap(cin, len);
	munmap(cout, len);
}
