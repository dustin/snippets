/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dString.h,v 1.1 2002/01/21 02:31:51 dustin Exp $
 */

#ifndef _DSTRING_H
#define _DSTRING_H 1

#include <dObject.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

@interface dString : dObject
{
@private
    char *string;   /* pointer to the string itself */
    int size;       /* Amount of allocated memory   */
    int inc;        /* Amount to increment on grow  */
}
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
