/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dSSocket.h,v 1.1 1998/03/17 04:04:58 dustin Exp $
 */

#ifndef _DSSOCKET_H
#define _DSSOCKET_H 1

#include <objc/Object.h>
#include <dString.h>

@interface dSSocket : Object
{
@private
    int s;
    int listening;
    int port;
}
-init;
-clear;
- (int) islistening;
- listento: (int) p;
- (int) getsocket;
- (int) write: (char *)buf;
- accept;
- setsocket: (int)so;
- readline;
@end

#endif
