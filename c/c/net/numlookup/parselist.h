/*
 * Copyright (c) 1999  beyond.com (dustin@beyond.com)
 *
 * $Id: parselist.h,v 1.1 1999/05/08 09:01:50 dustin Exp $
 */

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

/* The address list */
struct addr {
	unsigned int addr;
	int     mask;
	unsigned int intmask;
	char   *data;
};

/* The config structure */
struct config_t {
	int     size;
	int     index;
	struct addr **addr;
};
