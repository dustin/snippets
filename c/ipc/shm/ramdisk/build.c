/*
 * Copyright (c) 1997  SPY Internetworking
 *
 * $Id: build.c,v 1.1 1997/08/26 06:00:10 dustin Exp $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>

char *attach(int id)
{
    int shmid;
    char *shm;

    shmid=shmget( (key_t)id, 0, 0);

    if(shmid<0)
	return(NULL);

    if((shm=shmat(shmid, NULL, 0)) == (char *) -1)
	return(NULL);

    return(shm);
}
