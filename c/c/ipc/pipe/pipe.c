#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#define START_MAGIC 37259735
#define STOP_MAGIC  83735763

struct data {
	int start_magic;
	int child_id;
	int child_pid;
	int data;
	/*
	char d1[8192];
	char d2[4096];
	*/
	char d3[2048];
	char d4[1024];
	int stop_magic;
};

int data_read[4096];

void write_to(int pipefd[2], struct data d)
{
	int r=0;
	d.start_magic=START_MAGIC;
	d.stop_magic=STOP_MAGIC;

	r=write(pipefd[1], &d, sizeof(d));
	assert(r==sizeof(d));
}

void read_from(int pipefd[2])
{
	int r=0;
	struct data d;

	r=read(pipefd[0], &d, sizeof(d));
	assert(r==sizeof(d));

	assert(d.start_magic==START_MAGIC);
	assert(d.stop_magic==STOP_MAGIC);

	printf("Read %d from child %d (%d)\n", d.data, d.child_id, d.child_pid);

	assert(d.data == data_read[d.child_id]);
	data_read[d.child_id]++;
}

void child(int pipefd[2], int child_id)
{
	int i=0;
	struct data d;

	close(pipefd[0]);
	d.child_pid=getpid();
	d.child_id=child_id;
	sleep(5);

	for(i=0; ; i++) {
		d.data=i;
		write_to(pipefd, d);
	}
}

int main(int argc, char **argv)
{
	int pipefd[2];
	int r, i, nchildren=20;

	memset(&data_read, 0x00, sizeof(data_read));

	if(argc>1) {
		nchildren=atoi(argv[1]);
		assert(nchildren>0);
		assert(nchildren<200);
	}

	r=pipe(pipefd);
	assert(r==0);

	printf("Our data is %d bytes, and the buffer is %d bytes\n",
		sizeof(struct data), PIPE_BUF);
	printf("Starting %d children\n", nchildren);

	for(i=0; i<nchildren; i++) {
		int pid;

		pid=fork();
		switch(pid) {
			case -1:
				perror("fork");
				abort();
				break;
			case 0:
				child(pipefd, i);
				break;
			default:
				printf("Spawned child %d (%d)\n", i+1, pid);
				break;
		}
	}

	for(;;) {
		for(i=0; i<10000; i++) {
			read_from(pipefd);
		}
		printf("Sleeping...\n");
		sleep(1);
	}
}
