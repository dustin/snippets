/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: mqrecv.c,v 1.1 2000/10/05 21:02:15 dustin Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/errno.h>
#include <assert.h>

#include "mq.h"

extern int errno;

int
getMqKey(void)
{
	int mqid=0;

	mqid=msgget(MQ_KEY, IPC_CREAT|0666);
	assert(mqid>=0);

	return(mqid);
}

int
recv_msg(int userid, struct mqtest_msgbuf *mb)
{
	int r;

	r=msgrcv(getMqKey(), mb, sizeof(struct mqtest_msgbuf),
		(long)userid, IPC_NOWAIT);
	if(r<=0) {
		if(errno==ENOMSG) {
			printf("No message in the queue!\n");
		} else {
			perror("Error reading message");
		}
	}

	return(r);
}

int
main(int argc, char **argv)
{
	int r;
	struct mqtest_msgbuf mb;

	r=recv_msg(1, &mb);
	if(r>=0) {
		printf("Message for %s:\n%s\n", mb.username, mb.message);
	}
}
