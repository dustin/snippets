/*
Property of Dustin Sallings and anyone he lets use it.
$Id: rmauthdb.c,v 1.1 1997/08/26 06:00:50 dustin Exp $
*/
#include <stdio.h>
#include <msql.h>
#include <sys/types.h>
#include <pwd.h>

#define DBHOST "irony.ipa.net"
#define DBNAME "auth"

int debugvar=0;

void debug(char *what)
{
	if(debugvar==1)
	{
		puts(what);
	}
}

void die(char *what)
{
	puts(what);
	exit(1);
}

int doconnect(void)
{
int dbh;

	if( (dbh=msqlConnect(DBHOST)) < 0)
		die(msqlErrMsg);

	if(msqlSelectDB(dbh, DBNAME)<0)
		die(msqlErrMsg);

	/* delete anything that might be in there */
	if( msqlQuery(dbh, "delete from auth")<0)
		die(msqlErrMsg);

	return(dbh);
}

void main(int argc, char *argv[])
{
int dbh;
struct passwd *p;

	if(argc>1)
	{
		if(strcmp(argv[1], "-d")==0)
		{
			debugvar=1;
		}
	}

	dbh=doconnect();
}
