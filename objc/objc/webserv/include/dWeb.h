/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dWeb.h,v 1.1 1997/04/15 06:11:26 dustin Exp $
 */

#ifndef _DWEB_H
#define _DWEB_H 1

#include <objc/Object.h>
#include <dString.h>

#ifdef IWANTMETHODNAMES
static char *methodnames[]={
    "GET",
    "PUT",
    "POST",
    "HEAD",
    "DELETE",
    "TRACE",
    "OPTIONS",
    NULL
};
#endif

#define HTTP_GET     0
#define HTTP_PUT     1
#define HTTP_POST    2
#define HTTP_HEAD    3
#define HTTP_DELETE  4
#define HTTP_TRACE   5
#define HTTP_OPTIONS 6

@interface dRequest : Object
{
@private
    id request;    // This should be a dString
    int version;
    int docnum;
    int special;
    int method;
}
- init;
- setto:(const char *)astring;
- (int) length;
- clear;
- (int) parse :string;
- print;
@end

#endif
