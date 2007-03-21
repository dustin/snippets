/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: tablecounter.c,v 1.14 2002/03/15 09:52:46 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <libpq-fe.h>

#ifdef HAVE_RRD_H
#include <rrd.h>
#endif /* HAVE_RRD_H */

#include "array.h"

#define DBSERVER "db"
#define DBUSER "dustin"
#define DBPASS "blahblah"

#define MAKEDBSPEC4(dbname, table, tscolumn, query) \
	{ DBSERVER, DBUSER, DBPASS, NULL, dbname, NULL, table, tscolumn, query }
#define MAKEDBSPEC3(dbname, table, tscolumn) \
	MAKEDBSPEC4(dbname, table, tscolumn, "select count(*) from " #table)
#define MAKEDBSPEC(dbname, table) MAKEDBSPEC3(dbname, table, "ts")

#define TIGERDBOLD(table) \
	{ "disk", DBUSER, DBPASS, NULL, "tiger", "2345", table, "ts", \
		"select count(*) from %t" }

#define TIGERDB(table) \
	{ "disk", DBUSER, DBPASS, NULL, "tiger", "2345", table, "ts", \
		"select rows from counts where tablename='%t'" }

#define INCREMENT 3600

struct checkspec {
	char *server;
	char *user;
	char *pass;
	char *options;
	char *dbname;
	char *port;

	char *table;
	char *ts;
	
	char *query;
} checkspec;

static PGconn *getConn(struct checkspec query)
{
	PGconn *dbConn=NULL;
	dbConn=PQsetdbLogin(query.server, query.port, query.options, NULL,
		query.dbname, query.user, query.pass);
	if (PQstatus(dbConn) == CONNECTION_BAD) {
		fprintf(stderr, "Error connecting to %s on %s:  %s\n",
			query.dbname, query.server, PQerrorMessage(dbConn));
	}
	return(dbConn);
}

static void cleanConn(PGconn *dbConn) {
	if(dbConn!=NULL) {
		PQfinish(dbConn);
	}
}

void printResults(struct checkspec query, time_t t, int nrows)
{
#ifdef HAVE_RRD_H
	char **args;
	extern int optind;
	int rv=0;
	char buf[8192];

	sprintf(buf, "update %s.%s.rrd %d:%d\n",
		query.dbname, query.table, (int)t, nrows);
	assert(strlen(buf) < sizeof(buf));

	args=split(buf, " ");
	assert(args);

	optind=0;
	rv=rrd_update(listLength(args), args);
	if(rv<0 || rrd_test_error()) {
		fprintf(stderr, "ERROR on %s:  %s\n", buf, rrd_get_error());
		rrd_clear_error();
	}
	freeList(args);
#else /* !HAVE_RRD_H */
	printf("update %s.%s.rrd %d:%d\n",
		query.dbname, query.table, (int)t, nrows);
	fflush(stdout);
#endif /* HAVE_RRD_H */
}

void process(struct checkspec query)
{
	PGconn *dbConn=NULL;
	PGresult *res=NULL;
	char querystr[8192];
	int rv=0, i=0, j=0;

	dbConn=getConn(query);
	if(dbConn==NULL) {
		goto finished;
	}

	/* Build the query */
	memset(querystr, 0x00, sizeof(querystr));
	for(i=0; i<strlen(query.query); i++) {
		switch(query.query[i]) {
			case '%':
				/* Skip the percent */
				i++;
				switch(query.query[i]) {
					case 't':
						strcat(querystr, query.table);
						j=strlen(querystr);
						break;
					case '%':
						querystr[j++]='%';
						break;
					default:
						fprintf(stderr,
							"WARNING:  Unknown character ``%c'' in %s\n",
							query.query[i], query.query);

				} /* inner switch */
				break;
			default:
				querystr[j++]=query.query[i];
		} /* Outer switch */
	}
	assert(strlen(querystr) < sizeof(querystr));

	res=PQexec(dbConn, querystr);
	if(PQresultStatus(res) != PGRES_TUPLES_OK) {
		fprintf(stderr, "Query failed:  %s\n%s\n",
			PQerrorMessage(dbConn), querystr);
		goto finished;
	}

	if(PQntuples(res) != 1) {
		fprintf(stderr, "Returned %d rows from the count query:  %s\n",
			PQntuples(res), querystr);
		goto finished;
	}

	rv=atoi(PQgetvalue(res, 0, 0));

	printResults(query, time(NULL), rv);

	finished:

	if(res!=NULL) {
		PQclear(res);
	}
	cleanConn(dbConn);

}

void backfill(time_t sincewhen, struct checkspec query)
{
	PGconn *dbConn=NULL;
	PGresult *res=NULL;
	char querystr[8192];
	int count=0, rows=0;
	time_t now=0, ts=0;

	dbConn=getConn(query);
	if(dbConn==NULL) {
		goto finished;
	}

	now=time(NULL);

	sprintf(querystr, "select date_part('epoch', %s) from %s order by %s",
		query.ts, query.table, query.ts);

	res=PQexec(dbConn, querystr);
	if(PQresultStatus(res) != PGRES_TUPLES_OK) {
		fprintf(stderr, "Query failed:  %s\n%s\n",
			PQerrorMessage(dbConn), querystr);
		goto finished;
	}

	rows=PQntuples(res);

	for(count=0; count < rows && sincewhen<now; count++) {
		ts=atoi(PQgetvalue(res, count, 0));

		if(ts>sincewhen) {
			printResults(query, sincewhen, count);
			sincewhen+=INCREMENT;
		}
	}

	finished:

	if(res!=NULL) {
		PQclear(res);
	}
	cleanConn(dbConn);

}

void realmain(time_t backfill_time)
{
	int i=0;
	struct checkspec queries[]={
		MAKEDBSPEC4("photo", "photo_logs", "ts",
			"select last_value from photo_logs_log_id_seq"),
		MAKEDBSPEC4("music", "music_download_log", "timestamp",
			"select last_value from music_download_log_log_id_seq"),
		MAKEDBSPEC4("music", "music_mp3_downloads", "ts",
			"select last_value from music_mp3_downl_download_id_seq"),
		MAKEDBSPEC("music", "music_subscribers"),
		MAKEDBSPEC("temperature", "samples"),
		TIGERDBOLD("loaded_files"),
		TIGERDB("type_1"),
		TIGERDB("type_2"),
		TIGERDB("type_3"),
		TIGERDB("type_4"),
		TIGERDB("type_5"),
		TIGERDB("type_6"),
		TIGERDB("type_7"),
		TIGERDB("type_8"),
		TIGERDB("type_9"),
		TIGERDB("type_a"),
		TIGERDB("type_c"),
		TIGERDB("type_h"),
		TIGERDB("type_i"),
		TIGERDB("type_p"),
		TIGERDB("type_r"),
		TIGERDB("type_s"),
		TIGERDB("type_z"),
		MAKEDBSPEC(NULL, NULL),
		};

	if(backfill_time>0) {
		for(i=0; queries[i].table!=NULL; i++) {
			backfill(backfill_time, queries[i]);
		}
	} else {
		for(;;) {
			int started=0, stopped=0, naptime=0;
			started=time(NULL);
			for(i=0; queries[i].table!=NULL; i++) {
				process(queries[i]);
			}
			stopped=time(NULL);
			/* I want to run every hour, so take into consideration the amount
			   of time it took to query all those tables. */
			naptime=INCREMENT - (stopped-started);
			if(naptime < 1) {
				fprintf(stderr, "Took longer than an hour to run (leftover is "
					"%d), finding next run.\n", naptime);
				while(naptime < 1) {
					naptime+=3600;
				}
			}
			fprintf(stderr, "Sleeping %d seconds\n", naptime);
			sleep(naptime);
		}
	}
}

int main(int argc, char **argv)
{
	time_t backfill_time=0;

	if(argc>1) {
		backfill_time=atoi(argv[1]);
		backfill_time/=3600;
		backfill_time*=3600;
	}

	realmain(backfill_time);
	return(0);
}
