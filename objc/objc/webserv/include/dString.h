/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dString.h,v 1.2 1997/04/15 06:11:25 dustin Exp $
 */

#ifndef _DSTRING_H
#define _DSTRING_H 1

#include <objc/Object.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

@interface dString : Object
{
@private
    char *string;   /* pointer to the string itself */
    int size;       /* Amount of allocated memory   */
    int inc;        /* Amount to increment on grow  */
}
- init;
- init:(int)s;
- append:(const char *)astring;
- setto:(const char *)astring;
- (int) length;
- lowercase;
- uppercase;
- free;
- clear;
- print;
- (int) readfile  :(const char *)filename;
- (int) scompare  :(const char *)nother;
- (int) compare   :nother;
- (int) ncompare  :nother len:(int)length;
- (int) sncompare :(const char *)nother len:(int)length;
- (char *) dup;
-kw;
@end

#endif
