#include <stdio.h>
#include <pthread.h>

#define DBMUTEX 0
#define NMUTEXEN 1

pthread_mutex_t mutexen[NMUTEXEN];

void dodbwork(char *query)
{
    int r;
    printf("Attempting %s\n", query);
    pthread_mutex_lock(&mutexen[DBMUTEX]);
    printf("\t\t\t\t\tDoing query:  %s\n", query);
    r=rand()%5;
    sleep(r);
    pthread_mutex_unlock(&mutexen[DBMUTEX]);
}

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

    pthread_mutex_init(&mutexen[0], NULL);

    for(i=0; i<8; i++)
    {
        thread_n[i]=i;
        pthread_create(&threads[i], NULL, thread_main, &thread_n[i]);
    }
    for(i=0; i<8; i++)
        pthread_join(threads[i], NULL);
}
