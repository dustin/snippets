#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <assert.h>

#define FILENAME "/tmp/semtest.file"

void
makefile()
{
	int fd;

	fd=open(FILENAME, O_WRONLY|O_CREAT|O_EXCL, 0644);
	if(fd<0) {
		perror("open");
		assert(fd>=0);
	}
	close(fd);
}

void
removefile()
{
	int r;
	r=unlink(FILENAME);
	if(r<0) {
		perror("unlink");
		assert(r>=0);
	}
	assert(r>=0);
}

void
printfunc(void)
{
	int             sem;
	struct sembuf   sb[1];

	puts("Getting semaphore...");
	if ((sem = semget(187, 1, 0)) < 0) {
		perror("semget");
		return;
	}

	puts("Obtaining lock...");
	sb[0].sem_num =  0;
	sb[0].sem_op  = -1;
	sb[0].sem_flg =  SEM_UNDO;

	if ((semop(sem, sb, 1)) < 0) {
		perror("semop");
		return;
	}
	puts("Locked...");
	makefile();
	sleep(10);
	removefile();
	puts("Unlocking...");

	sb[0].sem_num = 0;
	sb[0].sem_op  = 1;
	sb[0].sem_flg = SEM_UNDO;
	if ((semop(sem, sb, 1)) < 0) {
		perror("semop");
		return;
	}

}

void
main(void)
{
	printfunc();
}
