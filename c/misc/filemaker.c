#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define TVDIFF(tv1, tv2, a, b, c) \
	c=0; \
	a=tv2.tv_sec-tv1.tv_sec; \
	if(tv2.tv_usec<tv1.tv_usec) { \
		a--; \
		c=1000000; \
	} \
	b=(tv2.tv_usec+c)-tv1.tv_usec;



int main(int argc, char **argv)
{
	int i=0, a=0, b=0, c=0, fd=0;
	char fn[64];
	struct timeval timers[2];
	void *tzp=0;

	fprintf(stderr, "Welcome to filemaker!\n");

	for(i=0; i<250000; i++) {
		/* Start timer */
		gettimeofday(&timers[0], tzp);
		sprintf(fn, "fm%020d", i);
		fd=open(fn, O_WRONLY|O_CREAT|O_EXCL, 0600);
		if(fd< 0) {
			perror(fn);
		} else {
			close(fd);
		}
		/* End timer */
		gettimeofday(&timers[1], tzp);

		TVDIFF(timers[0], timers[1], a, b, c);
		printf("%s %d %u.%06u\n", fn, i, a, b);
	}
}
