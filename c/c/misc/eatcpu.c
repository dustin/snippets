/*
 * Copyright(c) 1997  Dustin Sallings
 *
 * $Id: eatcpu.c,v 1.1 1997/04/30 04:49:36 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

int n, *list;

void killthem(void)
{
    int i;

    for(i=0; i<n; i++)
    {
	printf("Getting %d (%d)\n", i, list[i]);
	kill(list[i], SIGINT);
	waitpid(list[i], NULL, 0);
    }
}

void sigtrap(int sig)
{
    killthem();
    free(list);
    exit(0);
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

    while(size<n)
    {
	size<<=2;
    }

    printf("Getting %d bytes of memory\n", size*sizeof(int));

    list=(int *)malloc(size*sizeof(int));

    signal(SIGTERM, sigtrap);
    signal(SIGKILL, sigtrap);
    signal(SIGINT, sigtrap);
    signal(SIGALRM, sigtrap);

    for(i=0;i<n;i++)
    {
	if( (pid=fork())==0)
	{
	    signal(SIGINT, SIG_DFL);
	    for(;;);
	}
	else
	{
	    printf("Got child %d (%d)\n", i+1, pid);
	    list[i]=pid;
	}
    }
    pause();
}
