/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: tablecounter.c,v 1.8 2002/02/28 20:14:05 dustin Exp $
 */

#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <libpq-fe.h>

#define DBSERVER "db"
#define DBUSER "dustin"
#define DBPASS "blahblah"

#define MAKEDBSPEC3(a, b, c) { DBSERVER, DBUSER, DBPASS, NULL, a, NULL, b, c}
#define MAKEDBSPEC(a, b) MAKEDBSPEC3(a, b, "ts")

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
/*
	printf("%lu %s.%s: %d\n", (int)t, query.dbname, query.table, nrows);
*/
	printf("update %s.%s.rrd %d:%d\n",
		query.dbname, query.table, (int)t, nrows);
	fflush(stdout);
}

void process(struct checkspec query)
{
	PGconn *dbConn=NULL;
	PGresult *res=NULL;
	char querystr[8192];
	int rv=0;

	dbConn=getConn(query);
	if(dbConn==NULL) {
		goto finished;
	}

	sprintf(querystr, "select count(*) from %s", query.table);

	res=PQexec(dbConn, querystr);
	if(PQresultStatus(res) != PGRES_TUPLES_OK) {
		fprintf(stderr, "Query failed:  %s\n%s\n",
			PQerrorMessage(dbConn), querystr);
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

	sprintf(querystr, "select date_part('epoch', %s) from %s order by ts",
		query.ts, query.table);

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
		MAKEDBSPEC("photo", "photo_logs"),
		MAKEDBSPEC3("music", "music_download_log", "timestamp"),
		MAKEDBSPEC("music", "music_mp3_downloads"),
		MAKEDBSPEC("music", "music_subscribers"),
		MAKEDBSPEC("temperature", "samples"),
		MAKEDBSPEC(NULL, NULL),
		};

	if(backfill_time>0) {
		for(i=0; queries[i].table!=NULL; i++) {
			backfill(backfill_time, queries[i]);
		}
	} else {
		for(;;) {
			for(i=0; queries[i].table!=NULL; i++) {
				process(queries[i]);
			}
			sleep(INCREMENT);
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
}
