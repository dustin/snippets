#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

void printfunc(void)
{
    int sem;
    struct sembuf sb;

    puts("Getting semaphor...");
    if( (sem=semget(187, 5, 0)) < 0)
    {
        puts("No semaphor, creating...");

        if( (sem=semget(187, 5, IPC_CREAT|0644)) < 0)
        {
	    perror("semget");
	    return;
        }
    }
    else
    {
	puts("Attached to old semaphor");
    }

    printf("semaphor is %d\n", sem);

    puts("Checking lock...");
    sb.sem_num=0;
    sb.sem_op=0;
    sb.sem_flg=0;

    if( (semop(sem, &sb, 1)) < 0 )
    {
	perror("semop");
	return;
    }

    puts("Lock's OK.  Locking the door.");
    sb.sem_num=0;
    sb.sem_op=1;
    sb.sem_flg=0;

    if( (semop(sem, &sb, 1)) < 0 )
    {
	perror("semop");
	return;
    }

    puts("Locked...");
    sleep(15);
    puts("Unlocking...");

    sb.sem_num=0;
    sb.sem_op=-1;
    sb.sem_flg=0;

    if( (semop(sem, &sb, 1)) < 0 )
    {
	perror("semop");
	return;
    }

}

void main(void)
{
    printfunc();
}
