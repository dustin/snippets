/*
 * Copyright 1998 Dustin Sallings
 *
 * $Id: mbkd.h,v 1.4 1998/10/02 07:02:29 dustin Exp $
 */

#ifndef MBKD_H
#define MBKD_H 1

/* debug stuff */
#ifndef PDEBUG
#define PDEBUG 1
#endif

#if (PDEBUG>0)
# ifndef _ndebug
#  define _ndebug(a, b) if(conf.debug > a ) printf b;
# endif
#endif

/* In case it didn't make it */
#ifndef _ndebug
#define _ndebug(a, b)
#endif

#include <config.h>
#include <definitions.h>
#include <readconfig.h>
#include <hash.h>
#include <mymalloc.h>

/* stuff */
#if !defined(HAVE_VSNPRINTF)
# if defined(HAVE_VSPRINTF)
#  define vsnprintf(a, b, c, d) vsprintf(a, c, d)
# else
#  error "No vsnprintf *OR* vsprintf?  Call your vendor."
# endif
#endif

#if !defined(HAVE_SNPRINTF)
# if ! defined(HAVE_SPRINTF)
#  error "No snprintf or sprintf, this is not C."
# endif
#endif

#define MAXPACKETLEN 1024

struct config {
	int debug;  /* debug level */
    int log;    /* Logging facility */
	char *pidfile; /* pid file */

	struct confType *cf;
};

/* instance */
extern struct config conf;

struct namedfunc {
    char *name;
    void (*func)(void);
};

struct mbk {
    int  len;                /* Length of the packet from length to end of
	                          * useful data.
							  */
	char data[MAXPACKETLEN]; /* The actual data */
	struct hashtable *hash;  /* The hash table we put all the data in after
	                          * we get it.
							  */
};

char **split(char c, char *string);
char *hexprint(int size, char *buf);
char *kw(char *in);
char *unhexprint(int size, char *buf);
void _do_log(int level, char *msg);
void freeptrlist(char **list);
void log_debug(char *format,...);
void log_misc(int level, char *format, ...);
void log_msg(char *format,...);

#endif /* MBKD_H */
