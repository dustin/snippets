#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>

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
FILE *out;
struct ent *info;

	shmid=shmget(key, 0, 0);
	if(shmid<0)
	{
		perror("Couldn't find it");
		exit(1);
	}

	if((blah=shmat(shmid, NULL, 0)) == (char *) -1)
	{
		perror("shmat");
		exit(1);
	}
	else
		puts("Got a hold on it now");

	info=(struct ent *)blah;

	out=fopen(info->filename, "wb");

	printf("Filename is %s and it's %d bytes long\n", info->filename, info->size);

	fwrite(&info->data, info->size, 1, out);
	exit(0);
}
