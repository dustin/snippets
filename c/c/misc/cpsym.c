/* Copyright (c) 1999 beyond.com */

/* symlink copier.  Very simple */

#include <stdio.h>
#include <unistd.h>
#include <libgen.h>
#include <sys/errno.h>

extern int errno;

void usage(char *name)
{
	fprintf(stderr, "USAGE:\n%s link destination_dir\n", name);
}

int main(int argc, char **argv)
{
	char buf[8192];
	char out[8192];
	char *base;

	if(argc<3) {
		usage(argv[0]);
		exit(1);
	}

	if(readlink(argv[1], buf, 8190)<0) {
		perror(argv[1]);
		if(errno==EINVAL)
			usage(argv[0]);
		exit(1);
	}

	base=basename(argv[1]);

	if(base==NULL) {
		fprintf(stderr, "Weird, basename failed for %s\n", argv[1]);
		exit(1);
	}

	if(strlen(base)+strlen(argv[2])>8190) {
		fprintf(stderr, "Args too long\n");
		exit(1);
	}

	strcpy(out, argv[2]);
	strcat(out, "/");
	strcat(out, base);

	if( symlink(buf, out) < 0) {
		perror(out);
		exit(1);
	}

	exit(0);
}
