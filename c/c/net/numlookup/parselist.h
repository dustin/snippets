/*
 * Copyright (c) 1999  beyond.com (dustin@beyond.com)
 *
 * $Id: parselist.h,v 1.2 1999/05/08 21:17:54 dustin Exp $
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
#define CONFIGFILE "list"
#define LIFETIME 10

/* The config structure */
struct config_t {
	struct hashtable *hash[33]; /* 0-32, one for each bit */
	unsigned int masks[33];
};
