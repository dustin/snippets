/*
 * Copyright (c) 2001  Dustin Sallings
 *
 * $Id: dThread.h,v 1.1 2002/01/21 02:31:52 dustin Exp $
 */

#ifndef _DTHREAD_H
#define _DTHREAD_H 1

#include <dObject.h>
#include <pthread.h>

@interface dThread : Object
{
@private
	pthread_t thread; /* The thread itself */
}

/* Tell it to go */
-run;

/* The client will run start (not run) and it will call the run method. */
-start;

@end

#endif /* _DTHREAD_H */
