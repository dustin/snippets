/*
 * Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
 */

#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <libpq-fe.h>
#include <assert.h>

static int linenum=0;

static void dbCopy(const char *query, FILE *from, PGconn *conn) {
	PGresult *res=NULL;
	char buf[8192];
	int rv=0;

	assert(query);
	assert(from);
	assert(conn);

	/* Execute the query */
	res=PQexec(conn, query);

	/* Validate the copy in is ready */
	if(PQresultStatus(res) != PGRES_COPY_IN) {
		fprintf(stderr, "Copy preparation failed:  %s\n%s\n",
			PQerrorMessage(conn), query);
		goto theend;
	}

	/* Start putting lines */
	while(fgets(buf, sizeof(buf), from) != NULL) {
		/* Make sure we read a line */
		assert(buf[strlen(buf)-1] == '\n');
		/* Get the current line number */
		linenum++;
		rv=PQputline(conn, buf);
		if(rv != 0) {
			fprintf(stderr, "Error sending line %d because %s\n",
				linenum, PQerrorMessage(conn));
		}
		if(linenum % 10000 == 0) {
			time_t t=time(NULL);
			fprintf(stderr, "Copied line %d at %s", linenum, ctime(&t));
		}
	}

	/* End the copy */
	PQputline(conn, "\\.\n");
	PQendcopy(conn);

	theend:

	return;
}

static PGconn *getConn(char *server, char *user, char *pass, char *db)
{
	PGconn *dbConn=NULL;
	dbConn=PQsetdbLogin(server, NULL, NULL, NULL, db, user, pass);
	if (PQstatus(dbConn) == CONNECTION_BAD) {
		fprintf(stderr, "Error connecting to %s on %s:  %s\n",
			db, server, PQerrorMessage(dbConn));
		exit(2);
	}
	return(dbConn);
}

int main(int argc, char **argv) {
	PGconn *db=NULL;
	char *query=NULL;
	time_t start=0, finish=0;
	if(argc < 6) {
		fprintf(stderr, "Usage:  %s server user pass db query\n"
			"Copy data will be expected to arrive via stdin.\n"
			"Example:\n\tgzip -dc dump.gz | %s db user pass log "
				"'copy tblx from STDIN'\n", argv[0], argv[0]);
		exit(1);
	}
	db=getConn(argv[1], argv[2], argv[3], argv[4]);
	query=argv[5];

	/* Perform the copy */
	time(&start);
	dbCopy(query, stdin, db);
	time(&finish);

	/* Close the connection */
	PQfinish(db);

	fprintf(stderr, "Copied %d lines in %d seconds as of %s",
		linenum, ((int)finish - (int)start), ctime(&finish));

	return(0);
}
