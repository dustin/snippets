/*
 * Copyright 1998 Dustin Sallings
 *
 * $Id: mbkd.h,v 1.2 1998/10/01 16:44:41 dustin Exp $
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

struct config {
	int debug;  /* debug level */
    int log;    /* Logging facility */
	char *pidfile; /* pid file */

	struct confType *cf;
};

struct namedfunc {
    char *name;
    void (*func)(void);
};

struct mbk {
    int len;
	int auth;
	char data[1024];
};

char *kw(char *in);

#endif /* MBKD_H */
