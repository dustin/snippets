/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dSocket.h,v 1.1 1997/04/15 06:11:23 dustin Exp $
 */

#ifndef _DSOCKET_H
#define _DSOCKET_H 1

#include <objc/Object.h>
#include <dString.h>

@interface dSocket : Object
{
@private
    int s;
    int listening;
    int port;
}
-init;
- (int) islistening;
- listento: (int) p;
- (int) getsocket;
- (int) write: (char *)buf;
- accept;
- setsocket: (int)so;
- read;
@end

#endif
