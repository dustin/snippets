/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: redirect.h,v 1.1 1998/01/02 02:49:51 dustin Exp $
 */

#ifndef REDIRECT_H
#define REDIRECT_H

#define CONFFILE "redir.conf"

#define MAPSIZE 1024
#define BUFLEN 1024

/* Undef this if you don't have herror, I'll switch to autoconf RSN */
#define HAVE_HERROR

/* Debug stuff */
#ifndef PDEBUG
#define PDEBUG 1
#endif

#if (PDEBUG>0)
# ifndef _ndebug
#  define _ndebug(a, b) if(PDEBUG > a ) printf b;
# endif
#endif

/* In case it didn't make it */
#ifndef _ndebug
#define _ndebug(a, b)
#endif

int getclientsocket(char *host, int port);
int getservsocket(char *host, int port);

#endif
