#include <stdio.h>
#include <pthread.h>

#define DBMUTEX 0
#define NMUTEXEN 1

pthread_mutex_t mutexen[NMUTEXEN];

/* Thread simulating database work */
void dodbwork(char *query)
{
    int r;
    printf("Attempting %s\n", query);
    pthread_mutex_lock(&mutexen[DBMUTEX]);
	printf("\t\t\t\t\tLOCKED\n");
    printf("\t\t\t\t\tDoing query:  %s\n", query);
    r=rand()%5;
    sleep(r);
	printf("\t\t\t\t\tUNLOCKING\n");
    pthread_mutex_unlock(&mutexen[DBMUTEX]);
}

/* Thread does this */
int thread_main(int *n)
{
    int me, i;
    char buffer[1024];

    me=*n;
    printf("Thread #%d initializing\n", me);

    for(i=0; i<20; i++)
    {
        sleep(1);
        sprintf(buffer, "I am thread %d", me);
        dodbwork(buffer);
    }
}

int main(int argc, char **argv)
{
    pthread_t threads[8];
    int       thread_n[8];
    int i;

    srand(time(NULL));

    pthread_mutex_init(&mutexen[DBMUTEX], NULL);

	/* Start some threads */
    for(i=0; i<8; i++) {
        thread_n[i]=i;
        pthread_create(&threads[i], NULL, thread_main, &thread_n[i]);
    }
	/* Wait for them to finish
	   If we were not going to wait, we'd use pthread_detach
	 */
    for(i=0; i<8; i++) {
        pthread_join(threads[i], NULL);
	}
}
