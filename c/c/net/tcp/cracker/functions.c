/*
 * Copyright 1997 SPY Internetworking
 *
 * $Id: functions.c,v 1.2 1997/01/05 08:41:00 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include "cracker.h"

extern struct global *glob;

void getsolution(struct command com)
{
    printf("The password is %s\n", com.passwd);
}

void inituser(int s)
{
    struct init ini;

    strcpy(ini.password, glob->encrypted);
    send(s, (char *)&ini, sizeof(ini), 0);
}

void sendpasswords(int s)
{
    struct retpack r;
    int i;

    memset(&r, 0x00, sizeof(r));

    r.info=htons(0);

    for(i=0; i<MAXPASS; i++)
    {
	strcpy(r.pswds[i], newpasswd());
	glob->acons[s]->tries++;
	glob->numtries++;
    }

    send(s, (char *)&r, sizeof(r), 0);
}

void sendstatus(int s)
{
    struct info i;
    int j;

    memset(&i, 0x00, sizeof(i));

    i.cons=htonl(glob->numusers);
    i.tries=htonl(glob->numtries);
    i.starttime=htonl(glob->starttime);
    i.curtime=htonl(time(0));
    strcpy(i.curpass, glob->password);
    strcpy(i.encrypted, glob->encrypted);

    for(j=0; j<MAXCONS; j++)
    {
	if(glob->acons[j]!=NULL)
	{
	    i.acons[j].socket=htonl(glob->acons[j]->socket);
	    i.acons[j].contime=htonl(glob->acons[j]->contime);
	    i.acons[j].tries=htonl(glob->acons[j]->tries);
	}
    }

    send(s, (char *)&i, sizeof(i), 0);
}
