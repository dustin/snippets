/*
 * Copyright (c) 1999  Dustin Sallings
 *
 * $Id
 */

#include <stdio.h>
#include <libpq-fe.h>

PGconn *
pg_connect(char *host, char *db)
{
	PGconn *conn;

	conn = PQsetdb(host, NULL, NULL, NULL, db);

	if (PQstatus(conn) == CONNECTION_BAD) {
		conn=NULL;
	}

	return (conn);
}

PGresult *
pg_query(PGconn * conn, char *query)
{
	PGresult       *res;
	char           *tmp;
	int             i, j, ntuples, nfields;

	res = PQexec(conn, query);

	if (PQresultStatus(res) != PGRES_TUPLES_OK) {
		puts(PQerrorMessage(conn));
		return (NULL);
	}

	return (res);
}
