/*
 * Copyright (c) 1999  Dustin Sallings
 *
 * Licensed as according to the Eiffel Forum Freeware License, version 1
 * See forum.txt for more information.
 *
 * $Id: pg_cstuff.c,v 1.5 1999/06/03 22:16:34 dustin Exp $
 */

#include <stdio.h>
#include <libpq-fe.h>

int
pg_connection_ok(PGconn *conn)
{
	return(PQstatus(conn) == CONNECTION_OK);
}

int
pg_connection_bad(PGconn *conn)
{
	return(PQstatus(conn) == CONNECTION_BAD);
}

int
pg_empty_query(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_EMPTY_QUERY);
}

int
pg_command_ok(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_COMMAND_OK);
}

int
pg_tuples_ok(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_TUPLES_OK);
}

int
pg_copy_out(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_COPY_OUT);
}

int
pg_copy_in(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_COPY_IN);
}

int
pg_bad_response(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_BAD_RESPONSE);
}

int
pg_nonfatal_error(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_NONFATAL_ERROR);
}

int
pg_fatal_error(PGresult *res)
{
	return(PQresultStatus(res) == PGRES_FATAL_ERROR);
}
