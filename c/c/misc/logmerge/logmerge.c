/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: logmerge.c,v 1.4 2002/06/05 19:37:06 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>

#include <zlib.h>

#include "logmerge.h"
#include "mymalloc.h"

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

static time_t parseTimestamp(struct logfile *lf)
{
	char *p, *tmp;
	struct tm tm;

	assert(lf != NULL);
	assert(lf->line != NULL);

	lf->timestamp=-1;

	memset(&tm, 0x00, sizeof(tm));

	tmp=strdup(lf->line);
	assert(tmp!=NULL);
	p=tmp;

	if(p[10]=='T') {
		/* fprintf("**** Parsing %s\n", p); */

		tm.tm_year=atoi(p);
		p+=5;
		tm.tm_mon=atoi(p);
		p+=3;
		tm.tm_mday=atoi(p);
		p+=3;
		tm.tm_hour=atoi(p);
		p+=3;
		tm.tm_min=atoi(p);
		p+=3;
		tm.tm_sec=atoi(p);

		tm.tm_year-=1900;
		tm.tm_mon--;

		lf->timestamp=mktime(&tm);
	} else if(index(p, '[') != NULL) {

		p=index(p, '[');
		assert(p != NULL);

		/* fprintf(stderr, "**** Parsing %s\n", p); */
		p++;
		tm.tm_mday=atoi(p);
		p+=3;
		tm.tm_mon=parseMonth(p);
		p+=4;
		tm.tm_year=atoi(p);
		p+=5;
		tm.tm_hour=atoi(p);
		p+=3;
		tm.tm_min=atoi(p);
		p+=3;
		tm.tm_sec=atoi(p);

		/* Make sure it still looks like CLF */
		assert(p[2]==' ');

		tm.tm_year-=1900;

		/* XXX  Hack for figuring out DST */
		assert(p[5] == '7' || p[5] == '8');
		if( p[5] == '7' ) {
			tm.tm_isdst=1;
		}

		lf->timestamp=mktime(&tm);

	} else {
		fprintf(stderr, "Unknown log format:  %s\n", p);
	}
	/*
	fprintf(stderr, "**** Got %d:  %s\n", (int)lf->timestamp,
		ctime(&lf->timestamp));
	*/

	if(lf->timestamp < 0) {
		fprintf(stderr, "* Error parsing timestamp from %s", tmp);
	}

	free(tmp);

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
		assert(logfileOpened == OK);
		/* Recurse to skip a line */
		p=nextLine(lf);
		assert(p!=NULL);
	}

	p=gzgets(lf->input, lf->line, LINE_BUFFER-1);
	if(p!=Z_NULL) {
		/* Make sure the line is short enough */
		assert(strlen(lf->line) < LINE_BUFFER);
		/* Make sure we read a line */
		assert(p[strlen(p)-1] == '\n');

		/* Parse it */
		if(parseTimestamp(lf) == -1) {
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
	struct logfile *rv;

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
		nextLine(rv);
		/* Now close the logfile */
		closeLogfile(rv);
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

	assert(argc>1);

	for(i=1; i<argc; i++) {
		lf=createLogfile(argv[i]);
		if(lf!=NULL) {
			list=addToList(list, lf);
		} else {
			fprintf(stderr, "Error opening logfile ``%s''\n", argv[i]);
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
