/*
 *
 *
 *
 */

#ifndef SPLAT_H
#define SPLAT_H

/* Debug stuff */
#ifndef PDEBUG
#define PDEBUG 1
#endif

#if (PDEBUG>0)
# ifndef _ndebug
#  define _ndebug(a, b) if(_debug > a ) printf b;
# endif
#endif

/* In case it didn't make it */
#ifndef _ndebug
#define _ndebug(a, b)
#endif

/* Socket options */
#define DO_NAGLE 1
#define NON_BLOCKING 2

#ifndef MAXINT
#define MAXINT 0x7fffffff
#endif

int getclientsocket(char *host, int port, int sock_opts);

#endif
