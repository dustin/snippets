/*
 * Copyright (c) 2001  Dustin Sallings
 *
 * $Id: dObject.h,v 1.1 2002/01/21 02:31:51 dustin Exp $
 */

#ifndef _DOBJECT_H
#define _DOBJECT_H 1

#include <objc/Object.h>
#include <stdio.h>

@interface dObject : Object

-init;

- (const char *) getRCSId; /* Get the RCS ID for an object. */

@end /* dObject : Object */

#endif /* _DOBJECT_H */
