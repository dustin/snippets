/*
 * Copyright(c) 1997  Dustin Sallings
 *
 * $Id: eatcpu.c,v 1.4 1998/01/14 07:42:59 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

int b, n, *list;

void killthem(int which)
{
    int i;

    for(i=0; i<which; i++)
    {
	if(list[i]>0)
	{
	    printf("Getting %d (%d)\n", i, list[i]);
	    kill(list[i], SIGINT);
	    waitpid(list[i], NULL, 0);
	    list[i]=0;
	}
    }
}

void sigtrap(int sig)
{
    printf("Signal caught, exiting...\n");
    killthem(b);
    free(list);
    exit(0);
}

void alrmtrap(int sig)
{
    printf("Killing off extra kids...\n");
    killthem(b-n);
    alarm(0);
    signal(SIGALRM, sigtrap);
}

void main(int argc, char **argv)
{
    int i, pid, size=4;

    if(argc<2)
    {
	puts("How much do you want to increase the load?");
	exit(0);
    }

    n=atoi(argv[1]);
    b=n+(n/2);

    while(size<b) /* 50% head start */
    {
	size<<=2;
    }

    printf("Getting %d bytes of memory\n", size*sizeof(int));

    list=(int *)malloc(size*sizeof(int));

    signal(SIGTERM, sigtrap);
    signal(SIGKILL, sigtrap);
    signal(SIGINT, sigtrap);
    signal(SIGALRM, alrmtrap);

    for(i=0;i<b;i++)
    {
	if( (pid=fork())==0)
	{
	    signal(SIGINT, SIG_DFL);
	    /* we can reuse i without affecting the parent */
	    for(i=0; i<255; i++)
		close(i);
	    for(;;);
	}
	else
	{
	    printf("Got child %d (%d)\n", i+1, pid);
	    if(pid<0)
		perror("ACK!  fork()");
	    else
	        list[i]=pid;
	}
    }

    alarm(30);

    /* Just to keep the parent busy waiting for signals */
    for(;;)
	pause();
}
