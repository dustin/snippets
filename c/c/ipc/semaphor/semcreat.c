#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <assert.h>

void
createSem(void)
{
	int             sem;
	int r;
	union {
		int val;
		struct semid_ds *buf;
		u_short *array;
	} data;

	data.val=1;

	puts("Creating semaphore");
	if ((sem = semget(187, 1, IPC_EXCL | IPC_CREAT | 0644)) < 0) {
		perror("semget");
		return;
	}

	/* Set it (unlocked) */
	if ((semctl(sem, 0, SETVAL, data)) < 0) {
		perror("semctl");
		return;
	}

	r=semctl(sem, 0, GETVAL);
	printf("Sem value is %d\n", r);

	sleep(60);

	if (semctl(sem, 0, IPC_RMID)<0) {
		perror("semctl: IPC_RMID");
		return;
	}
}

int
main(int argc, char **argv)
{
	createSem();
}
