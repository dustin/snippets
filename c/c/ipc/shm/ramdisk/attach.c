/*
 * Copyright (c) 1997  SPY Internetworking
 *
 * $Id: attach.c,v 1.1 1997/08/26 06:00:10 dustin Exp $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#include "ramdisk.h"

char *buildit(int id, int size)
{
    int shmid, i;
    char *shm;

    shmid=shmget(id, size, IPC_CREAT | IPC_EXCL | 0644);

    if(shmid<0)
    {
	return(NULL);
    }

    if((shm=shmat(shmid, NULL, 0)) == (char *) -1)
	return(NULL);

    memset(shm, 0x00, size);
}
