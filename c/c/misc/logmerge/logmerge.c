/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: logmerge.c,v 1.1 2002/06/05 16:51:55 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <zlib.h>

#include "logmerge.h"

/**
 * Open a logfile.
 */
static int openLogfile(struct logfile *lf)
{
	int rv=ERROR;
	assert(lf);

	assert(lf->isOpen==0);

	printf("*** Opening %s\n", lf->filename);

	lf->input=gzopen(lf->filename, "r");

	if(lf->input != NULL) {
		lf->isOpen=1;
		rv=OK;
	}

	return(rv);
}

/**
 * Get the next line from a log file.
 */
static char *nextLine(struct logfile *lf)
{
	char *p=NULL;

	assert(lf);

	if(lf->isOpen == 0) {
		int logfileOpened=openLogfile(lf);
		assert(logfileOpened == OK);
		/* Recurse to skip a line */
		p=nextLine(lf);
		assert(p);
	}

	p=gzgets(lf->input, lf->line, LINE_BUFFER-1);
	if(p!=Z_NULL) {
		/* Make sure we read a line */
		assert(p[strlen(p)-1] == '\n');
	}

	return(p);
}

static void closeLogfile(struct logfile *lf)
{
	assert(lf);
	assert(lf->input != NULL);

	gzclose(lf->input);
	lf->isOpen=0;
}

/**
 * Get rid of a logfile that's no longer needed.
 */
static void destroyLogfile(struct logfile *lf)
{
	assert(lf);

	/* Free the parts */
	if(lf->filename!=NULL) {
		free(lf->filename);
	}
	if(lf->line != NULL) {
		free(lf->line);
	}

	if(lf->isOpen==1) {
		gzclose(lf->input);
		lf->isOpen=0;
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
	assert(rv);

	rv->filename=(char *)strdup(filename);
	assert(rv->filename);

	/* Make room for lines */
	rv->line=calloc(1, LINE_BUFFER);

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
 * The main.
 */
int main(int argc, char **argv)
{
	struct logfile *lf;
	char *p;

	assert(argc>1);

	lf=createLogfile(argv[1]);
	printf("Log line:  ``%s''\n", lf->line);
	p=nextLine(lf);
	assert(p);
	printf("Next log line:  ``%s''\n", lf->line);
}
