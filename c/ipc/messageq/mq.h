/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: mq.h,v 1.1 2000/10/05 21:02:14 dustin Exp $
 */

#ifndef MQ_H
#define MQ_H 1

#define MQ_KEY 8353243

#define MQTEST_USER_LENGTH 16
#define MQTEST_MSG_LENGTH  (80*5)

#define MQTEST_TYPE_SIMPLE 1

struct mqtest_msgbuf {
	long mtype; /* long representation of the userid */
	char username[MQTEST_USER_LENGTH];
	char message[MQTEST_MSG_LENGTH];
};

#endif /* MQ_H */
