/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: dSNPP.h,v 1.1 1998/03/17 04:04:57 dustin Exp $
 */

#ifndef _DSNPP_H
#define _DSNPP_H 1

#include <objc/Object.h>
#include <dString.h>
#include <dSocket.h>

@interface dSNPP :Object
{
@private
    id sock;
    int status;
}
-init;
-clear;
-(int)connectTo: (char *)host :(int)port;
-(int)status;
-(int)sendAPage :(char *)to thatsays:(char *)message;
-protoSend :(char *)cmd :(char *)message;
-protoSend :(char *)cmd;
-quit;
@end

#endif
