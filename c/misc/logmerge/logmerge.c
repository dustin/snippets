/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: logmerge.c,v 1.10 2003/10/06 19:03:44 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>

#ifdef USE_ASSERT
# include <assert.h>
#else
# define assert(a)
#endif

#include <zlib.h>

#include "logmerge.h"
#include "mymalloc.h"

#define strdupa(d, s) { \
	d=alloca(strlen(s)+1);\
	assert(d!=NULL); \
	memcpy(d, s, strlen(s)+1); \
}

/**
 * Open a logfile.
 */
static int openLogfile(struct logfile *lf)
{
	int rv=ERROR;
	assert(lf != NULL);

	assert(lf->isOpen==0);

	fprintf(stderr, "*** Opening %s\n", lf->filename);

	lf->input=gzopen(lf->filename, "r");

	if(lf->input != NULL) {
		lf->isOpen=1;
		rv=OK;
	}

	return(rv);
}

/* Convert a three character month to the numeric value */
static int parseMonth(char *s)
{
	int rv=-1;

	if(strncmp(s, "Jan", 3)==0) {
		rv=0;
	} else if(strncmp(s, "Feb", 3)==0) {
		rv=1;
	} else if(strncmp(s, "Mar", 3)==0) {
		rv=2;
	} else if(strncmp(s, "Apr", 3)==0) {
		rv=3;
	} else if(strncmp(s, "May", 3)==0) {
		rv=4;
	} else if(strncmp(s, "Jun", 3)==0) {
		rv=5;
	} else if(strncmp(s, "Jul", 3)==0) {
		rv=6;
	} else if(strncmp(s, "Aug", 3)==0) {
		rv=7;
	} else if(strncmp(s, "Sep", 3)==0) {
		rv=8;
	} else if(strncmp(s, "Oct", 3)==0) {
		rv=9;
	} else if(strncmp(s, "Nov", 3)==0) {
		rv=10;
	} else if(strncmp(s, "Dec", 3)==0) {
		rv=11;
	}

	return(rv);
}

/* Convert a line from the old format to CLF */
static int unfsckLine(struct logfile *lf)
{
	char *start=NULL, *end=NULL;
	char *line=NULL;
	char tsbuf[32];
	char *timestamp=NULL, *ip=NULL, *request=NULL, *status=NULL,
		*size=NULL, *refer=NULL, *ua=NULL;
	char *format=NULL;
	int rv=ERROR;

	assert(lf != NULL);
	assert(lf->line != NULL);

	strdupa(line, lf->line);
	assert(line!=NULL);

	/* Start at the timestamp */
	start=line;
	end=index(start, ' ');
	assert(end != NULL);
	timestamp=NULL;
	/* XXX:  Hack, should deal with other timezones */
	if(lf->tm.tm_isdst) {
		format="%d/%h/%Y:%T -0700";
	} else {
		format="%d/%h/%Y:%T -0800";
	}
	/* OK, just going to use the existing timestamp and format it properly */
	strftime(tsbuf, sizeof(tsbuf), format, &lf->tm);
	/*
	fprintf(stderr, "** Timestamp is ``[%s]''\n", tsbuf);
	*/

	/* Seek to the IP address */
	start=end+1;
	end=index(start, ' ');
	assert(end != NULL);
	assert(*end == ' ');
	*end=0x00;
	strdupa(ip, start);
	/*
	fprintf(stderr, "** IP is ``%s''\n", ip);
	*/

	/* Seek to the - */
	start=end+1;
	assert(start[0] == '-');
	end=index(start, ' ');
	assert(end != NULL);

	/* Seek to the request */
	start=end+1;
	assert(start[0] == '\"');
	end=index(start+1, '\"');
	assert(end != NULL);
	/* If there's an embedded quote, keep seeking */
	while( !(end[1]==' ' && isdigit(end[2])) ) {
		end=index(end+1, '\"');
		assert(end);
	}
	end++; /* The quote is part of what we want */
	assert(*end == ' ');
	*end=0x00;
	strdupa(request, start);
	/*
	fprintf(stderr, "** Request is ``%s''\n", request);
	*/

	/* Seek to the status */
	start=end+1;
	end=index(start, ' ');
	assert(end != NULL);
	assert(*end == ' ');
	*end=0x00;
	strdupa(status, start);
	/*
	fprintf(stderr, "** Status is ``%s''\n", status);
	*/

	/* Seek to the size */
	start=end+1;
	end=index(start, ' ');
	assert(end != NULL);
	assert(*end == ' ');
	*end=0x00;
	strdupa(size, start);
	/*
	fprintf(stderr, "** Size is ``%s''\n", size);
	*/

	/* Seek to the referer */
	start=end+1;
	assert(start[0] == '\"');
	end=index(start+1, '\"');
	assert(end != NULL);
	/* If there's an embedded quote, keep seeking */
	while( !(end[1]==' ' && end[2]=='\"') ) {
		end=index(end+1, '\"');
		assert(end);
	}
	end++; /* The quote is part of what we want */
	assert(*end == ' ');
	*end=0x00;
	strdupa(refer, start);
	/*
	fprintf(stderr, "** Referer is ``%s''\n", refer);
	*/

	/* Seek to the user agent */
	start=end+1;
	assert(start[0] == '\"');
	end=index(start+1, '\n');
	assert(end != NULL);
	assert(*end == '\n');
	*end=0x00;
	strdupa(ua, start);
	/*
	fprintf(stderr, "** User Agent is ``%s''\n", ua);
	*/

	/* Put it back */
	snprintf(lf->line, LINE_BUFFER-1, "%s - - [%s] %s %s %s %s %s\n",
		ip, tsbuf, request, status, size, refer, ua);

	return(rv);
}

static time_t parseTimestamp(struct logfile *lf)
{
	char *p;

	assert(lf != NULL);
	assert(lf->line != NULL);

	lf->timestamp=-1;

	memset(&lf->tm, 0x00, sizeof(lf->tm));

	p=lf->line;

	/* The shortest line I can parse is about 32 characters. */
	if(strlen(p) < 32) {
		/* This is a broken entry */
		fprintf(stderr, "Broken log entry (too short):  %s\n", p);
	} else if(p[10]=='T') {
		/* fprintf("**** Parsing %s\n", p); */

		lf->tm.tm_year=atoi(p);
		p+=5;
		lf->tm.tm_mon=atoi(p);
		p+=3;
		lf->tm.tm_mday=atoi(p);
		p+=3;
		lf->tm.tm_hour=atoi(p);
		p+=3;
		lf->tm.tm_min=atoi(p);
		p+=3;
		lf->tm.tm_sec=atoi(p);

		lf->tm.tm_year-=1900;
		lf->tm.tm_mon--;

		lf->timestamp=mktime(&lf->tm);

		unfsckLine(lf);
	} else if(index(p, '[') != NULL) {

		p=index(p, '[');
		assert(p != NULL);
		/* Verify it's long enough to parse */
		assert(strlen(p) > 32);

		/* fprintf(stderr, "**** Parsing %s\n", p); */
		p++;
		lf->tm.tm_mday=atoi(p);
		p+=3;
		lf->tm.tm_mon=parseMonth(p);
		p+=4;
		lf->tm.tm_year=atoi(p);
		p+=5;
		lf->tm.tm_hour=atoi(p);
		p+=3;
		lf->tm.tm_min=atoi(p);
		p+=3;
		lf->tm.tm_sec=atoi(p);

		/* Make sure it still looks like CLF */
		assert(p[2]==' ');

		lf->tm.tm_year-=1900;

		/* XXX  Hack for figuring out DST */
		assert(p[5] == '7' || p[5] == '8');
		if( p[5] == '7' ) {
			lf->tm.tm_isdst=1;
		}

		lf->timestamp=mktime(&lf->tm);

	} else {
		fprintf(stderr, "Unknown log format:  %s\n", p);
	}

	if(lf->timestamp < 0) {
		fprintf(stderr, "* Error parsing timestamp from %s", lf->line);
	}

	return(lf->timestamp);
}

/**
 * Get the next line from a log file.
 */
static char *nextLine(struct logfile *lf)
{
	char *p=NULL;

	assert(lf != NULL);

	if(lf->isOpen == 0) {
		int logfileOpened=openLogfile(lf);
		/* This looks a little awkward, but it's the only way I can both
		 * avoid the side effect of having assert perform the task and
		 * not leave the variable unreferenced when assertions are off.
		 */
		if(logfileOpened != OK) {
			assert(logfileOpened == OK);
		}
		/* Recurse to skip a line */
		p=nextLine(lf);
		assert(p!=NULL);
	}

	p=gzgets(lf->input, lf->line, LINE_BUFFER-1);
	if(p!=Z_NULL) {
		/* Make sure the line is short enough */
		assert(strlen(lf->line) < LINE_BUFFER);
		/* Make sure we read a line */
		if(p[strlen(p)-1] != '\n') {
			fprintf(stderr, "*** BROKEN LOG ENTRY IN %s (no newline)\n",
				lf->filename);
			p=NULL;
		} else if(parseTimestamp(lf) == -1) {
			/* If we can't parse the timestamp, give up */
			p=NULL;
		}
	}

	return(p);
}

static void closeLogfile(struct logfile *lf)
{
	int gzerrno=0;

	assert(lf != NULL);
	assert(lf->input != NULL);
	assert(lf->filename != NULL);

	fprintf(stderr, "*** Closing %s\n", lf->filename);

	gzerrno=gzclose(lf->input);
	if(gzerrno!=0) {
		gzerror(lf->input, &gzerrno);
	}
	lf->isOpen=0;
}

/**
 * Get rid of a logfile that's no longer needed.
 */
static void destroyLogfile(struct logfile *lf)
{
	assert(lf != NULL);

	fprintf(stderr, "** Destroying %s\n", lf->filename);

	if(lf->isOpen==1) {
		closeLogfile(lf);
	}

	/* Free the parts */
	if(lf->filename!=NULL) {
		free(lf->filename);
	}
	if(lf->line != NULL) {
		free(lf->line);
	}

	/* Lastly, free the container itself. */
	free(lf);
}

/**
 * Create a new logfile.
 */
struct logfile *createLogfile(const char *filename)
{
	struct logfile *rv=NULL;
	char *p=NULL;

	rv=calloc(1, sizeof(struct logfile));
	assert(rv != NULL);

	rv->filename=(char *)strdup(filename);
	assert(rv->filename != NULL);

	/* Make room for lines */
	rv->line=calloc(1, LINE_BUFFER);
	assert(rv->line != NULL);

	/* Try to open the logfile */
	if(openLogfile(rv) != OK) {
		destroyLogfile(rv);
		rv=NULL;
	} else {
		/* If it's opened succesfully, read the next (first) line */
		p=nextLine(rv);
		/* Now close the logfile */
		closeLogfile(rv);
		/* If nextLine didn't return a record, this entry is invalid. */
		if(p == NULL) {
			destroyLogfile(rv);
			rv=NULL;
		}
	}

	return(rv);
}

/**
 * Add a logfile to the given linked list.  If the list is NULL, create a
 * new one.
 */
struct linked_list *addToList(struct linked_list *list, struct logfile *r)
{
	struct linked_list *tmp;
	struct linked_list *rv;

	assert(r != NULL);

	tmp=calloc(1, sizeof(struct linked_list));

	tmp->logfile=r;

	if(list == NULL) {
		rv=tmp;
	} else {

		assert(list->logfile != NULL);

		if(tmp->logfile->timestamp < list->logfile->timestamp) {
			/* Special case where it goes at the head. */
			rv=tmp;
			tmp->next=list;
		} else {
			/* Regular case where it goes somewhere in the list. */
			struct linked_list *p;
			int placed=0;
			
			rv=list;
			p=list;
			while(placed==0 && p->next != NULL) {

				if(tmp->logfile->timestamp < p->next->logfile->timestamp) {
					tmp->next=p->next;
					p->next=tmp;
					placed=1;
				}

				p=p->next;
			}

			/* If it's still not placed, it goes at the end */
			if(placed==0) {
				p->next=tmp;
			}
		}
	}


	return(rv);
}

/**
 * Get the current record from the first entry in the linked list.
 */
struct logfile *currentRecord(struct linked_list *list)
{
	struct logfile *rv=NULL;

	if(list!=NULL && list->logfile != NULL) {
		rv=list->logfile;
	}

	return(rv);
}

/**
 * Get rid of the first entry in the log list, and reinsert it somewhere
 * that makes sense, or throw it away if it's no longer necessary.
 */
struct linked_list *skipRecord(struct linked_list *list)
{
	struct linked_list *rv=NULL;
	struct logfile *oldEntry=NULL;
	char *p;

	if(list!=NULL) {
		rv=list->next;

		oldEntry=list->logfile;

		p=nextLine(oldEntry);
		/* If stuff comes back, reinsert the old entry */
		if(p!=NULL) {
			rv=addToList(rv, oldEntry);
		} else {
			destroyLogfile(oldEntry);
		}

		/* No longer need this record, a new one will be created when it's
		 * reinserted (if it's reinserted)
		 */
		free(list);
	}

	return(rv);
}

static void dumpList(struct linked_list *list)
{
	struct linked_list *p;

	if(list==NULL) {
		fprintf(stderr, "*** dumpList: NULL list\n");
	} else {
		p=list;

		while(p!=NULL) {
			fprintf(stderr, "*** dumpList: %s (%d)\n", p->logfile->filename,
				(int)p->logfile->timestamp);
			p=p->next;
		}
	}
}

/**
 * The main.
 */
int main(int argc, char **argv)
{
	struct linked_list *list=NULL;
	struct logfile *lf=NULL;
	int i=0;
	int entries=0;

	if(argc>1) {
		for(i=1; i<argc; i++) {
			lf=createLogfile(argv[i]);
			if(lf!=NULL) {
				list=addToList(list, lf);
			} else {
				fprintf(stderr, "Error opening logfile ``%s''\n", argv[i]);
			}
		}
	} else {
		char buf[8192];
		fprintf(stderr, "No logfiles given, accepting list from stdin\n");
		while(fgets((char*)&buf, sizeof(buf)-1, stdin)) {
			buf[strlen(buf)-1]=0x00;
			lf=createLogfile(buf);
			if(lf!=NULL) {
				list=addToList(list, lf);
			} else {
				fprintf(stderr, "Error opening logfile ``%s''\n", buf);
			}
		}
	}

	dumpList(list);

	while(list!=NULL) {
		entries++;

		lf=currentRecord(list);
		mymalloc_assert(lf);
		mymalloc_assert(lf->line);
		mymalloc_assert(lf->filename);

		assert(lf!=NULL);
		if(lf->line) {
			printf("%s", lf->line);
		}
		list=skipRecord(list);
	}

	fprintf(stderr, "Read %d entries.\n", entries);

#ifdef MYMALLOC
	_mdebug_dump();
#endif /* MYMALLOC */

	return(0);
}
