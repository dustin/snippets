/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dCSocket.h,v 1.1 1998/03/17 04:04:58 dustin Exp $
 */

#ifndef _DCSOCKET_H
#define _DCSOCKET_H 1

#include <objc/Object.h>
#include <dString.h>

@interface dCSocket : Object
{
@private
    int s;
    int port;
}
-init;
-clear;
- (int) isconnected;
- connectTo: (char *)host :(int)port;
- (int) getsocket;
- (int) write: (char *)buf;
- setsocket: (int)so;
- readline;
- getStatus: string;
@end

#endif
