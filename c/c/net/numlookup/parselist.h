/*
 * Copyright (c) 1999  beyond.com (dustin@beyond.com)
 *
 * $Id: parselist.h,v 1.4 1999/05/08 22:39:56 dustin Exp $
 */

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

/* Path to the config file */
#define CONFIGFILE "list"

/* Amount of time (in seconds) to run library loop before checking the
 * library */
#define LIFETIME 11

/* How long to run the emergency function (in seconds) if the library
 * doesn't load */
#define EMERGENCY_TIME 30

/* The library to load */
#define THELIB "./libparselist.so"

/* The function to run in the library */
#define THEFUNC "main"

/* The config structure */
struct config_t {
	struct hashtable *hash[33]; /* 0-32, one for each bit */
	unsigned int masks[33];
};
