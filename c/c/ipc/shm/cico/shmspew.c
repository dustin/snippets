#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>

int main(int argc, char *argv[])
{
int i, c=0;
char *blah;
key_t key=8649;


	if((blah=shmat(atoi(argv[1]), NULL, 0)) == (char *) -1)
	{
		perror("shmat");
		exit(1);
	}
	else
		puts("Got a hold on it now");

	for(i=0; i<atoi(argv[2]); i++)
	{
		printf("%4d", (int) blah[i]);
		if(++c%15==0)
			putchar('\n');
	}
	putchar('\n');

	exit(0);
}
