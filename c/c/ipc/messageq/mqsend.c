/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: mqsend.c,v 1.1 2000/10/05 21:02:15 dustin Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <assert.h>

#include "mq.h"

int
getMqKey(int userid)
{
	int mqid=0;

	mqid=msgget(MQ_KEY, IPC_CREAT|0666);
	assert(mqid>=0);

	return(mqid);
}

int
send_message(int userid, char *username, char *msg)
{
	struct mqtest_msgbuf mb;
	int r;

	/* Check the pointers */
	assert(username);
	assert(msg);
	/* Check the lengths */
	assert(strlen(username) < MQTEST_USER_LENGTH);
	assert(strlen(msg)      < MQTEST_MSG_LENGTH);

	mb.mtype=(long)userid;
	strcpy(mb.username, username);
	strcpy(mb.message,  msg);

	r=msgsnd(getMqKey(userid), &mb, sizeof(mb), 0);
	assert(r>=0);
}

int
main(int argc, char **argv)
{
	send_message(1, "dustin", "helloder");
}
