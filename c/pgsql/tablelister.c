/*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <libpq-fe.h>

#include "array.h"

#define DBSERVER "db"
#define DBUSER "dustin"
#define DBPASS "blahblah"

#define QUERY "select n.nspname as schema, c.relname as name\n" \
	"  from pg_catalog.pg_class c\n" \
	"    left join pg_catalog.pg_namespace n on n.oid = c.relnamespace\n" \
	"  where c.relkind = 'r'\n" \
	"    and n.nspname not in ('pg_catalog', 'pg_toast', 'information_schema')"

static PGconn *getConn(char *server, char *user, char *pass, char *db)
{
	PGconn *dbConn=NULL;
	dbConn=PQsetdbLogin(server, NULL, NULL, NULL, db, user, pass);
	if (PQstatus(dbConn) == CONNECTION_BAD) {
		fprintf(stderr, "Error connecting to %s on %s:  %s\n",
			db, server, PQerrorMessage(dbConn));
	}
	return(dbConn);
}

static void cleanConn(PGconn *dbConn) {
	if(dbConn!=NULL) {
		PQfinish(dbConn);
	}
}

void process(char *server, char *user, char *pass, char *db)
{
	PGconn *dbConn=NULL;
	PGresult *res=NULL;
	int i=0;

	dbConn=getConn(server, user, pass, db);
	if(dbConn==NULL) {
		goto finished;
	}

	/* Build the query */
	res=PQexec(dbConn, QUERY);
	if(PQresultStatus(res) != PGRES_TUPLES_OK) {
		fprintf(stderr, "Query failed:  %s\n%s\n",
			PQerrorMessage(dbConn), QUERY);
		goto finished;
	}

	for(i=0; i<PQntuples(res); i++) {
		char *schema=NULL, *table=NULL;
		schema=PQgetvalue(res, i, 0);
		table=PQgetvalue(res, i, 1);

		printf("%s\t%s\n", schema, table);
	}

	finished:

	if(res!=NULL) {
		PQclear(res);
	}
	cleanConn(dbConn);

}

int main(int argc, char **argv)
{
	if(argc<5) {
		fprintf(stderr, "Usage:  %s server user pass db\n", argv[0]);
		exit(1);
	}
	process(argv[1], argv[2], argv[3], argv[4]);
	return(0);
}
