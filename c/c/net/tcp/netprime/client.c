/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: client.c,v 1.2 1997/06/30 00:31:17 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#define _BSD_SIGNALS
#include <signal.h>

#include "netprime.h"

/* no error checking is done in here because I don't care */

void clearsems(void)
{
    int i, sem;
    struct sembuf sb;

    sem=semget(187, 5, 0);

    sb.sem_num=0;
    sb.sem_op=-1;
    sb.sem_flg=IPC_NOWAIT;
    semop(sem, &sb, 1);
}

void dotrap(int sig)
{
    clearsems();
    exit(0);
}

int openhost(char *remhost)
{
struct hostent *hp;
register int s;
int flag;
struct linger l;
struct sockaddr_in sin;

    if((hp=gethostbyname(remhost)) == NULL)
    {
#ifdef HAVE_HERROR
        herror("gethostbyname");
#else
        fprintf(stderr, "Error looking up %s\n", remhost);
#endif
        exit(1);
    }

    if((s=socket(AF_INET, SOCK_STREAM, 0))<0)
    {
        perror("socket");
        exit(1);
    }

    sin.sin_family = AF_INET;
    sin.sin_port=htons(PRIME_PORT);
    bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

    l.l_onoff  = 1;
    l.l_linger = 60;
    setsockopt(s, SOL_SOCKET, SO_LINGER, (char *)&l, sizeof(l));

    flag=1;
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char *)&flag,
        sizeof(int));

    if(connect(s, (struct sockaddr *)&sin, sizeof(sin))<0)
    {
        perror("connect");
        exit(1);
    }

    return(s);
}

char *ckw(char *in)
{
    /* bounds checking */
    if(strlen(in)==0)
        return(in);

    while(isspace(in[strlen(in)-1]))
    {
        /* bounds checking */
        if(strlen(in)==0)
            return(in);

        in[strlen(in)-1]=0x00;
    }

    return(in);
}

void cgettext(char *message, int size)
{
    fgets(message, size, stdin);
    ckw(message);
}

void display(int ints[NGEN])
{
    int i, sem;
    struct sembuf sb;

    if( (sem=semget(187, 5, 0)) < 0)
    {
        if( (sem=semget(187, 5, IPC_CREAT|0644)) < 0)
        {
            perror("semget");
            return;
        }
    }

    sb.sem_num=0;
    sb.sem_op=0;
    sb.sem_flg=0;

    if( (semop(sem, &sb, 1)) < 0 )
    {
        perror("semop");
        return;
    }

    sb.sem_num=0;
    sb.sem_op=1;
    sb.sem_flg=0;

    if( (semop(sem, &sb, 1)) < 0 )
    {
        perror("semop");
        return;
    }

    for(i=0; i<NGEN; i++)
    {
        printf("%d\n", ints[i]);
        usleep(500000);
    }

    sb.sem_num=0;
    sb.sem_op=-1;
    sb.sem_flg=0;
    if( (semop(sem, &sb, 1)) < 0 )
    {
        perror("semop");
        return;
    }
}

void moreargs(char *name)
{
    printf("Usage:  %s hostname\nwhere hostname is your data server\n",
        name);
    exit(0);
}

void main(int argc, char **argv)
{
    int s, i, loc;
    char buf[BUFLEN];
    int ints[NGEN];

    signal(SIGINT, dotrap);
    signal(SIGQUIT, dotrap);
    signal(SIGTERM, dotrap);
    signal(SIGHUP, dotrap);
    signal(SIGALRM, dotrap);

    if(argc>1)
        s=openhost(argv[1]);
    else
	moreargs(argv[0]);

    loc=0;

    for(;;)
    {
        sprintf(buf, "generate %d\n", loc);

        send(s, buf, strlen(buf), 0);

        for(i=0; i<NGEN; i++)
        {
            gettextcr(s, buf);
            ints[i]=atoi(buf);
        }

        display(ints);
	loc+=NGEN;
    }
}
