#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifndef MAX_SIZE
#define MAX_SIZE 1024*1024*1024
#endif /* MAX_SIZE */

#define SUCCESS 2
#define FAIL 3

int allocate(size_t size)
{
	int rv=FAIL;
	char *p=NULL;

	printf("\rAllocating %dk          ", (int)size/1024);
	fflush(stdout);
	p=(char *)malloc(size);
	if(p==NULL) {
		puts("");
		perror("malloc");
		fprintf(stderr, "Failed at %dk\n", (int)size/1024);
	} else {
		rv=SUCCESS;
		memset(p, 0x00, size);
		free(p);
	}
	return(rv);
}

int
main(int argc, char **argv)
{
	int i=0;
	int incramount=128*1024*1024;

	for(i=incramount; incramount>=1 && i<MAX_SIZE; i+=incramount) {
		if(allocate(i) != SUCCESS) {
			i-=incramount;
			if(incramount==1) {
				incramount=0;
			} else {
				incramount/=2;
			}
			printf("Counting up from %dk (by %d)\n", i/1024, incramount);
		}
	}

	return(0);
}
