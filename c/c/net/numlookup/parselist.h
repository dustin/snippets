/*
 * Copyright (c) 1999  beyond.com (dustin@beyond.com)
 *
 * $Id: parselist.h,v 1.6 1999/05/10 22:53:11 dustin Exp $
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

/* Length of a line */
#define LINELEN 90

/* Amount of time (in seconds) to run library loop before checking the
 * library */
#define LIFETIME 60

/* How long to run the emergency function (in seconds) if the library
 * doesn't load */
#define EMERGENCY_TIME 30

/* Path to the config file */
#define CONFIGFILE "/usr/local/apache_mod/conf/rewrite.conf"

/* The library to load */
#define THELIB "/usr/local/apache_mod/bin/libparselist.so"

/* The function to run in the library */
#define THEFUNC "main"

/* The name to report as the log source */
#define LOG_NAME "testip"

/* The facility to log to */
#define LOG_FACILITY LOG_LOCAL0

/* The config structure */
struct config_t {
	struct hashtable *hash[33]; /* 0-32, one for each bit */
	unsigned int masks[33];
};

void _log(const char *format, ...);
