#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>

struct ent {
	char filename[40];
	int size;
	char *data;
};

int main(int argc, char *argv[])
{
int shmid;
char *blah;
key_t key=2600;
FILE *in;
struct ent *info;
struct stat stats;

	shmid=shmget(key, 0, 0);
	if(shmid<0)
	{
		perror("Can't find it.");
		exit(1);
	}

	if((blah=shmat(shmid, NULL, 0)) == (char *) -1)
	{
		perror("shmat");
		exit(1);
	}
	else
		puts("Got a hold on it now");

	stat(argv[1], &stats);
	info=(struct ent *)blah;
	info->data=blah+sizeof(struct ent)+4;
	strcpy(info->filename, argv[1]);
	info->size=stats.st_size;

	in=fopen(argv[1], "rb");
	fread(&info->data, info->size, 1, in);
	printf("Wrote %d bytes\n", info->size);
	exit(0);
}
