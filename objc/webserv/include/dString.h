/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dString.h,v 1.3 1997/04/15 21:49:44 dustin Exp $
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
- oappend: astring;
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
- (int) compare  :nother len:(int)length;
- (int) scompare :(const char *)nother len:(int)length;
- (char *) dup;
-kw;
@end

#endif
