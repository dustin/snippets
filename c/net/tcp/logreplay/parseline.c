/*
 * Copyright (c) 1998  Dustin Sallings <dustin@spy.net>
 *
 * $Id: parseline.c,v 1.3 1998/12/07 20:03:20 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "replay.h"

time_t
parsedate(char *date)
{
	struct tm t;
	char   *p, *p2, *tmp, *in;
	char   *datemap = "JanFebMarAprMayJunJulAugSepOctNovDec";

	assert(date);
	in = strdup(date);
	assert(in);

	t.tm_wday = 0;
	t.tm_yday = 0;
	t.tm_isdst = 0;

	t.tm_mday = atoi(in);
	p = strchr(in, '/');
	assert(p);
	p++;

	p2 = strchr(p, '/');
	assert(p2);
	*p2 = 0x00;
	tmp = strstr(datemap, p);
	t.tm_mon = ((int) tmp - (int) datemap) / 3;

	p = p2 + 1;
	t.tm_year = atoi(p) - 1900;

	p = strchr(p, ':');
	assert(p);
	p++;
	t.tm_hour = atoi(p);

	p = strchr(p, ':');
	assert(p);
	p++;
	t.tm_min = atoi(p);

	p = strchr(p, ':');
	assert(p);
	p++;
	t.tm_sec = atoi(p);

	free(in);

	return (mktime(&t));
}

int
getlog(FILE * f, struct log_entry *log)
{
	char   *p, *p2;
	char    buf[64 * 1024];
	time_t  timestamp;
	static time_t basetime = 0;

	if (fgets(buf, (64 * 1024) - 1, f) == NULL) {
		return (-1);
	}
	p = strchr(buf, ' ');
	assert(p);

	*p = 0x00;
	log->IP = strdup(buf);
	p++;

	p = strchr(p, '[');
	assert(p);
	p++;
	p2 = strchr(p, ']');
	assert(p2);
	*p2 = NULL;

	timestamp = parsedate(p);
	assert(timestamp > 0);
	if (basetime == 0)
		basetime = timestamp;

	log->timeoffset = (timestamp - basetime);

	p = strchr(p2 + 1, '"');
	assert(p);
	p++;
	p2 = strchr(p, '"');
	assert(p2);
	*p2 = NULL;

	log->request = strdup(p);

	return (0);
}
