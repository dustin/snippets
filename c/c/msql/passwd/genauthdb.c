/*
Property of Dustin Sallings and anyone he lets use it.
$Id: genauthdb.c,v 1.1 1997/08/26 06:00:49 dustin Exp $
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
	/*
	if( msqlQuery(dbh, "delete from auth")<0)
		die(msqlErrMsg);
	*/

	return(dbh);
}

void doit(struct passwd *p, int dbh)
{
char query[200], name[31];
int i,j,dodis;
m_result *tmp;
m_row blah;

	for(i=j=0; i<30; i++)
	{
		if(p->pw_gecos[i]=='\'')
			name[j++]='\\';
		if(p->pw_gecos[i]==',')
		{
			name[j]=0;
			break;
		}
		name[j++]=p->pw_gecos[i];
	}

	dodis=0;

	sprintf(query, "select passwd from auth where user_name='%s'",
		p->pw_name);

	debug(p->pw_name);

	if( msqlQuery(dbh, query)<0)
		die(msqlErrMsg);

	tmp=msqlStoreResult();

	if( msqlNumRows(tmp)==0)
	{
	    debug("    User doesn't exist, adding.\n");
	    sprintf(query, "insert into auth\n\
		values('%s','%s',%d,%d,'%s','%s','%s')",
		p->pw_name,p->pw_passwd,p->pw_uid,p->pw_gid,
		name,p->pw_dir,p->pw_shell);
	    dodis=1;
	}
	else
	{
	   blah=msqlFetchRow(tmp);
	   if( (strcmp(p->pw_passwd, blah[0]) != 0) )
	   {
	      if(debugvar==1)
	      {
		 printf("%s != %s, need to update\n", p->pw_passwd, blah[0]);
	      }
	      sprintf(query, "update auth set passwd='%s' where user_name='%s'",
		p->pw_passwd, p->pw_name);
              dodis=1;
	   }
	}

	if(dodis==1)
	{
	    debug(query);

	    if( msqlQuery(dbh, query)<0)
		die(msqlErrMsg);
	}

	msqlFreeResult(tmp);
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

	while(p=getpwent())
	{
		if( (p->pw_gid==100) || (p->pw_gid==20) )
			doit(p, dbh);
	}
}
