/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: log.c,v 1.1 1999/05/10 21:19:37 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <assert.h>

#include "mymalloc.h"
#include "parselist.h"

/* Logging code */
#if !defined(HAVE_SNPRINTF)
/*
 * snprintf for those that don't have it. * More that likely, it'll overrun
 * buffers, because they
 * probably don't have vsnprintf either.
 */
int
snprintf(char *s, size_t n, const char *format,...)
{
	int r;
	va_list ap;

	va_start(ap, format);
	r=vsnprintf(s, n - 1, format, ap);
	va_end(ap);
	return(r);
}

#endif

static void
_do_log(int level, char *msg)
{
	static int open=0;

	if(open==0) {
		open=1;
		openlog(LOG_NAME, LOG_PID | LOG_NDELAY, LOG_FACILITY);
	}
	syslog(LOG_FACILITY | level, msg);
}

void
_log(const char *format,...)
{
	va_list ap;
	char    buf[LINELEN];

	va_start(ap, format);
	(void)vsnprintf(buf, LINELEN - 1, format, ap);
	va_end(ap);
	_do_log(LOG_INFO, buf);
}
