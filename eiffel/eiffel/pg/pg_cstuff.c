/*
 * Copyright (c) 1999  Dustin Sallings
 *
 * Licensed as according to the Eiffel Forum Freeware License, version 1
 * See forum.txt for more information.
 *
 * $Id: pg_cstuff.c,v 1.4 1999/06/03 07:39:48 dustin Exp $
 */

#include <stdio.h>
#include <libpq-fe.h>

PGconn *
pg_connect(char *host, char *port, char *options, char *tty, char *db,
	char *user, char *pass)
{
	PGconn *conn;

	if(user!=NULL || pass != NULL) {
		conn = PQsetdbLogin(host, port, options, tty, db, user, pass);
	} else {
		conn = PQsetdb(host, port, options, tty, db);
	}

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
		/* puts(PQerrorMessage(conn)); */
		return (NULL);
	}

	return (res);
}
