#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#define SIZE 4000000

void main(void)
{
int shmid, i;
char *blah;
key_t key=2600;


/*
	shmid=shmget(IPC_PRIVATE, SIZE, IPC_CREAT | IPC_EXCL | 0644);
*/
	shmid=shmget(key, SIZE, IPC_CREAT | IPC_EXCL | 0644);
	if(shmid>=0)
		puts("Yeah, I got your RAM");
	else
		perror("getit");

	if((blah=shmat(shmid, NULL, 0)) == (char *) -1)
		perror("shmat");
	else
		puts("Attached too");

	puts("Initializing");
	for(i=0; i<SIZE; i++)
		blah[i]=13;

	exit(0);
}
