/*
 * Copyright(c) 1997  Dustin Sallings
 *
 * $Id: database.c,v 1.3 1997/10/01 07:07:21 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <libpq-fe.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
/*
#include <Xm/TextF.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
*/

#include "phonebook.h"
#include "fields.h"

#define DBHOST NULL
#define DBNAME "phone"

extern progdata globl;

void dDie(char *what)
{
    puts(what);
    PQfinish(globl.dbh);
    exit(1);
}

void dbConnect(void)
{
    globl.dbh=(void *)PQsetdb(DBHOST, NULL, NULL, NULL, DBNAME);

    if(PQstatus(globl.dbh)==CONNECTION_BAD)
	dDie(PQerrorMessage(globl.dbh));
}

void doQuery(char *query)
{
    PGresult *res;
    int i, j, ntuples, nfields;
    char *name;

    res=PQexec(globl.dbh, query);

    if(PQresultStatus(res)!=PGRES_TUPLES_OK)
	dDie(PQerrorMessage(globl.dbh));

    ntuples=PQntuples(res);
    nfields=PQnfields(res);

    for(i=0; i<nfields; i++)
	printf("%%%s%%:", PQfname(res, i));

    puts("");

    for(i=0; i<ntuples; i++)
    {
	for(j=0; j<nfields; j++)
	{
	    printf("%s:", PQgetvalue(res, i, j));
	}
	puts("");
    }
}
