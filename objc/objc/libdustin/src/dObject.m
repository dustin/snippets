/*
 * Copyright (c) 2001  Dustin Sallings
 *
 * $Id: dObject.m,v 1.1 2002/01/21 02:31:53 dustin Exp $
 */

#include <dObject.h>
#include <assert.h>

@implementation dObject

-init
{
	[super init];
	return self;
}

- (const char *)getRCSId
{
	static char buf[8192];
	assert(strlen([self name])<1024);
	strcpy(buf, [self name]);
	strcat(buf, " did not override getRCSId");
	return(buf);
}

@end
