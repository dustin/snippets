/*
 * Copyright 1998 Dustin Sallings
 *
 * $Id: mbkd.h,v 1.9 1998/10/03 08:20:23 dustin Exp $
 */

#ifndef MBKD_H
#define MBKD_H 1

/* debug stuff */
#ifndef PDEBUG
#define PDEBUG 1
#endif

#if (PDEBUG>0)
#ifndef _ndebug
#define _ndebug(a, b) if(conf.debug > a ) printf b;
#endif
#endif

/* In case it didn't make it */
#ifndef _ndebug
#define _ndebug(a, b)
#endif

#include <config.h>
#include <definitions.h>
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

struct namedfunc {
	char   *name;
	void    (*func) (void);
};

typedef struct __mbk MBK;

struct __mbk {
	struct {
		int     len;
		char    data[MAXPACKETLEN];
	} pkt;

	struct hashtable *hash;

	char   *host;
	int     port;
	char   *auth;

	int     (*append) (MBK * mbk, char *key, char *value);
	int     (*send) (MBK * mbk);
	int     (*verify) (MBK * mbk);
	struct hashtable *(*parse) (MBK * mbk);
	void    (*destroy) (MBK * mbk);
	void    (*sign) (MBK * mbk);

};

MBK    *mbk_new(char *host, int port, char *auth);
char  **split(char c, char *string);
char   *hexprint(int size, char *buf);
char   *kw(char *in);
char   *unhexprint(int size, char *buf);
int     checkpidfile(char *pidfile);
int     getservsocket_udp(int);
void    _do_log(int level, char *msg);
void    freeptrlist(char **list);
void    log_debug(char *format,...);
void    log_misc(int level, char *format,...);
void    log_msg(char *format,...);

#endif /* MBKD_H */
