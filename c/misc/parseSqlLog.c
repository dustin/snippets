/*
 * SQL Log parser.
 *
 * $Id: parseSqlLog.c,v 1.1 2002/09/23 18:20:43 dustin Exp $
 *
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 */

#include <stdio.h>
#include <stdlib.h>
#define _XOPEN_SOURCE
#include <time.h>
#include <string.h>
#include <assert.h>

#define TIMEFMT "%Y-%m-%d %H:%M:%S"

#define strdupa(d, s) { \
	d=alloca(strlen(s)+1);\
	assert(d!=NULL); \
	memcpy(d, s, strlen(s)+1); \
}


struct logEntry {
	time_t time;
	int calls;
	int calltime;
};

static struct logEntry *parseLogEntry(const char *line)
{
	struct logEntry *rv=NULL;

	/* Make sure it's there */
	assert(line);

	/* Only search through things that contain this string */
	if(strstr(line, "database.DBManager.sql") != NULL) {
		char *tmp;
		char *timestampStr=NULL;
		struct tm tm;
		int i;
		char *timingsStr;
		char *callsStr;
		time_t timestamp;
		int calls;
		int calltime;

		/* Make sure it's long enough to have a timestamp */
		assert(strlen(line) > 20);

		timestampStr=alloca(20);
		assert(timestampStr);
		strncpy(timestampStr, line, 20);
		timestampStr[19]=0x00;

		tmp=strptime(timestampStr, TIMEFMT, &tm);
		assert(tmp);

		/* Get the timestamp */
		timestamp=mktime(&tm);

		/* Make a copy of the line so we can strtok it*/
		strdupa(tmp, line);

		strtok(tmp, " \t");
		for(i=0; i<10; i++) {
			timingsStr=strtok(NULL, " \t");
		}

		tmp=strchr(timingsStr, '/');
		assert(tmp);
		callsStr=tmp+1;
		tmp=NULL;

		/* OK, get the parts...make sure there was at least one call */
		calls=atoi(callsStr);
		if(calls>0) {
			calltime=atoi(timingsStr)/calls;

			rv=calloc(1, sizeof(struct logEntry));
			assert(rv);

			rv->time=timestamp;
			rv->calls=calls;
			rv->calltime=calltime;
		}
	}

	return(rv);
}

time_t nearest(time_t t, int accuracy)
{
	time_t rv=t;

	rv=(t/accuracy) * accuracy;

	return(rv);
}

int main(int argc, char **argv)
{
	time_t lastTime=0;
	int totalCalls=0;
	int totalTime=0;
	char linebuff[8192];
	struct logEntry *le=NULL;

	while(fgets(linebuff, sizeof(linebuff), stdin) != NULL) {
		le=parseLogEntry(linebuff);
		if(le!=NULL) {
			/* Process */

			time_t t=nearest(le->time, 60);

			if(t!=lastTime && totalCalls > 0) {
				printf("update %s %d:%d:%d\n",
					argv[1], lastTime, totalCalls, totalTime);

				totalCalls=0;
				totalTime=0;
			}

			lastTime=t;
			totalCalls++;
			totalTime+=le->calltime;

			free(le);
		}
	}
}
