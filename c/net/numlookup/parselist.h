/*
 * Copyright (c) 1999  beyond.com (dustin@beyond.com)
 *
 * $Id: parselist.h,v 1.9 1999/05/12 18:07:43 dustin Exp $
 */

#include <syslog.h>

#include "hash.h"

#if !defined(HAVE_VSNPRINTF)
#if defined(HAVE_VSPRINTF)
#define vsnprintf(a, b, c, d) vsprintf(a, c, d)
#else
#error No vsnprintf *OR* vsprintf?  Call your vendor.
#endif
#endif

/* the size of an IP address */
#define IP_SIZE 32

/* Length of a line */
#define LINELEN 90

/* Amount of time (in seconds) to run library loop before checking the
 * library */
#ifndef LIFETIME
#define LIFETIME 60
#endif

/* How long to run the emergency function (in seconds) if the library
 * doesn't load */
#define EMERGENCY_TIME 30

/* Path to the config file */
#ifndef CONFIGFILE
#define CONFIGFILE "/usr/local/apache_mod/conf/rewrite.conf"
#endif

/* The library to load */
#ifndef THELIB
#define THELIB "/usr/local/apache_mod/bin/libparselist.so"
#endif

/* The function to run in the library */
#define THEFUNC "main"

/* The name to report as the log source */
#define LOG_NAME "testip"

/* The facility to log to */
#define LOG_FACILITY LOG_LOCAL0

/* Output if we can't find anything better */
#define DEFAULT_OUTPUT ""

/* The config structure */
struct config_t {
	struct hashtable *hash[IP_SIZE+1]; /* one for each bit (including 0) */
	unsigned int masks[IP_SIZE+1];
};

void _log(const char *format, ...);
