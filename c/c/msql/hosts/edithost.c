#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <msql.h>
#include <cgi.h>

void die(char *why)
{
	puts(why);
	exit(1);
}

void main(void)
{
int dbh, i;
time_t clock;
char query[500];
char qtmp[100];
struct cgiform *form;

char *charthings[]={
    "hostname", "admin", "user", "group", NULL
} ;

char *intthings[]={
    "trans_log", "err_log", "cgi_bin", "active", NULL
} ;

	puts("Content-type: text/html\n");
	puts("<html><head><title>Added Host</title></head>\n");
	puts("<body bgcolor=\"ffffff\">\n");

	if( (dbh=msqlConnect(NULL))<0)
		die(msqlErrMsg);

	if( msqlSelectDB(dbh, "web") < 0)
		die(msqlErrMsg);

        form=cgiinit();

	strcpy(query, "update virtuals set ");

	for(i=0; charthings[i]!=NULL; i++)
	{
	    sprintf(qtmp, "%s = '%s',", charthings[i],
		cgigetdata(form, charthings[i]));
	    strcat(query, qtmp);
	}

	for(i=0; intthings[i]!=NULL; i++)
	{
	    sprintf(qtmp, "%s = %s,",  intthings[i],
		cgigetdata(form, intthings[i]));
	    strcat(query, qtmp);
	}

	sprintf(qtmp, "who_updated='%s', date_updated= %d
		where hostname='%s'", getenv("REMOTE_USER"),
		(int)time(&clock), cgigetdata(form, charthings[0]));
	strcat(query, qtmp);

        puts(query);

	if( msqlQuery(dbh, query) < 0)
	{
	    printf("<br>Failed, that bastard:  %s<br>\n", msqlErrMsg);
	}
	else
	{
	    printf("<br>Success!!!, WOO!!!<br>\n");
            sprintf(query,"update info set value='%d' where prop='lastchange'",
		(int)time(&clock));
            msqlQuery(dbh, query);
	}

	puts("<hr align=\"left\" width=\"50%\"><font size=\"-2\">");
	puts("Copyright &copy 1996 <a href=\"http://www.ipa.net/\">Internet Partners of America</a>");
	puts("</font></body></html>");
	msqlClose(dbh);
}
