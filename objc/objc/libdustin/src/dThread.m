/*
 * Copyright (c) 2001  Dustin Sallings
 *
 * $Id: dThread.m,v 1.1 2002/01/21 02:31:54 dustin Exp $
 */

#include <dThread.h>
#include <pthread.h>
#include <assert.h>

@implementation dThread

-init
{
	[super init];
	return self;
}

static void run(id *self)
{
	[*self run];
}

-run
{
	fprintf(stderr, "Starting....\n");
	sleep(1);
	fprintf(stderr, "Run not implemented.\n");
}

-start
{
	pthread_create(&thread, NULL, run, &self);
}

@end

