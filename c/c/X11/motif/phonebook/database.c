/*
 * Copyright(c) 1997  Dustin Sallings
 *
 * $Id: database.c,v 1.2 1997/07/15 13:52:04 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <time.h>
#include <msql.h>

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
    exit(1);
}

void dbConnect(void)
{
    if( (globl.dbh=msqlConnect(DBHOST)) < 0)
	dDie(msqlErrMsg);

    if( (msqlSelectDB(globl.dbh, DBNAME)) <0)
	dDie(msqlErrMsg);
}

void doQuery(char *query)
{
    int i, j, dodis;
    m_result *tmp;
    m_row res;

    if(msqlQuery(globl.dbh, query)<0)
	dDie(msqlErrMsg);

    tmp=msqlStoreResult();

    switch(msqlNumRows(tmp))
    {
	case 0:
	    CreateTrans("No such user", "Find message");
	    break;
	case 1:
	    res=msqlFetchRow(tmp);
	    for(i=0; i<NFIELDS; i++) puts(res[i]);
	default:
	    CreateTrans("Found user", "Find message");
    }

    msqlFreeResult(tmp);
}
