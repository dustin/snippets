#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>

#ifndef MAP_FILE
#define MAP_FILE 0 /* BSD has a useful MAP_FILE */
#endif

/*
 * This makes a simple index of where the lines start.
 */

void main(int argc, char **argv)
{
	char *start, *p, *out;
	struct stat st;
	int fd, data, which;

	assert(argc>2);

	stat(argv[1], &st);
	fd=open(argv[1], O_RDONLY, 0);
	assert(fd);

	start=mmap(NULL, st.st_size, PROT_READ, MAP_FILE|MAP_SHARED, fd, 0);
	assert(start);
	close(fd);

	fd=open(argv[2], O_WRONLY|O_CREAT|O_TRUNC, 0644);
	assert(fd);

	which=0;
	p=start;
	while( p && ((int)p-(int)start)<=st.st_size) {
		p=strchr(p, '\n');
		if(!p)
			break;
		data=(int)p-(int)start;
		lseek(fd, data*sizeof(int), SEEK_SET);
		write(fd, &which, sizeof(int));
		printf("Seeked to %d (%d) and wrote %d\n", data, data*sizeof(int),
		       which);
		p++; /* next line */
		which++;
	}
	close(fd);
	munmap(start, st.st_size);
}
