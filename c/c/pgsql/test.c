/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: test.c,v 1.1 1997/10/01 07:06:29 dustin Exp $
 *
 * This is a practical example of the postgres C API
 */


#include <stdio.h>
#include <libpq-fe.h>

void main(void)
{
    PGconn *conn;
    PGresult *res;
    char *tmp;
    int i, j, ntuples, nfields;

    conn=PQsetdb(NULL, NULL, NULL, NULL, "machine");

    if(PQstatus(conn)==CONNECTION_BAD)
    {
	puts(PQerrorMessage(conn));
	exit(0);
    }

    res=PQexec(conn, "select * from oems order by name;");

    if(PQresultStatus(res)!=PGRES_TUPLES_OK)
    {
	puts(PQerrorMessage(conn));
	exit(0);
    }

    ntuples=PQntuples(res);
    nfields=PQnfields(res);

    printf("Found %d tuples of %d fields each.\n", ntuples, nfields);

    for(i=0; i<nfields; i++)
	printf("%%%s%%:", PQfname(res, i));

    puts("");

    for(i=0; i<ntuples; i++)
    {
	for(j=0; j<nfields; j++)
	    printf("%s:", PQgetvalue(res, i, j));
	puts("");
    }

    PQfinish(conn);
}
