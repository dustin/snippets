/*
 * Copyright (c) 2002  Dustin Sallings
 *
 * $Id: logmerge.h,v 1.2 2002/06/06 04:01:27 dustin Exp $
 */

#ifndef LOGMERGE_H
#define LOGMERGE_H 1

#include <sys/time.h>
#include <sys/types.h>
#include <zlib.h>

#define OK 1
#define ERROR -1

/* The size of a line buffer */
#define LINE_BUFFER 16384

/* The logfile itself */
struct logfile {
	/* The filename of this log entry */
	char *filename;
	/* The current record */
	char *line;
	/* The timestamp of the current record */
	time_t timestamp;
	/* The timestamp as a struct tm */
	struct tm tm;
	/* 1 if it's open, 0 otherwise */
	int isOpen;
	/* The actual input stream being read */
	gzFile input;
};

/* Linked list to hold the queue */
struct linked_list {
	struct logfile *logfile;
	struct linked_list *next;
};

/* Get a new logfile */
struct logfile *createLogfile(const char *filename);
/* Add a new record to the correct location in the given linked list. */
struct linked_list *addToList(struct linked_list *list, struct logfile *r);
/* Get the current record from the list */
struct logfile *currentRecord(struct linked_list *list);
/* Skip to the next record in the list */
struct linked_list *skipRecord(struct linked_list *list);

#endif /* LOGMERGE_H */
