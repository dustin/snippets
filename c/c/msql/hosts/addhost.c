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
int dbh, i, count;
time_t clock;
char query[500];
char qtmp[100];
struct cgiform *form;
char *flags[8];

char *charthings[]={
    "hostname", "admin", "user", "group", NULL
} ;

char *intthings[]={
    "trans_log", "err_log", "cgi_bin", "active", NULL
} ;

        count=0;

	memset(query, 0x00, 500);
	puts("Content-type: text/html\n");
	puts("<html><head><title>Added Host</title></head>\n");
	puts("<body bgcolor=\"ffffff\">\n");

	if( (dbh=msqlConnect(NULL))<0)
		die(msqlErrMsg);

	if( msqlSelectDB(dbh, "web") < 0)
		die(msqlErrMsg);

        form=cgiinit();

	cgiprintdata(form);

	strcpy(query, "insert into virtuals  (hostname, admin, user, \
		group, trans_log, err_log, cgi_bin, active, who_updated, \
		date_updated) values(");

	for(i=0; charthings[i]!=NULL; i++)
	{
	    memset(qtmp, 0x00, 100);
	    sprintf(qtmp, "'%s',", cgigetdata(form, charthings[i]));
#ifdef DEBUG
	    printf("DEBUG: Got '%s' for '%s'<br>\n",
		cgigetdata(form, charthings[i]), charthings[i]);
#endif
	    strcat(query, qtmp);
	    if(strcmp(qtmp, "'',")==0)
	    {
		flags[count++]=charthings[i];
	    }
	}

	for(i=0; intthings[i]!=NULL; i++)
	{
	    memset(qtmp, 0x00, 100);
	    sprintf(qtmp, "%s,", cgigetdata(form, intthings[i]));
	    strcat(query, qtmp);
	    if(strcmp(qtmp, ",")==0)
	    {
		flags[count++]=charthings[i];
	    }
	}

	sprintf(qtmp, "'%s', %d)", getenv("REMOTE_USER"), (int)time(&clock));
	strcat(query, qtmp);

        puts(query);

        if( flags[0]==NULL)
	{
	    if( msqlQuery(dbh, query) < 0)
	    {
	        printf("<br>Failed, that bastard:  %s<br>\n", msqlErrMsg);
	    }
	    else
	    {
	        printf("<br>Success!!!, WOO!!!<br>\n");
	        sprintf(query,
		    "update info set value='%d' where prop='lastchange'",
		    (int)time(&clock));
	        msqlQuery(dbh, query);
	    }
        }
	else
	{
	    puts("<br>ERROR:  The following fields were not filled out:<br>");
	    for(i=0; flags[i]!=NULL; i++)
	    {
		printf("%s<br>\n", flags[i]);
	    }
	}

	puts("<hr align=\"left\" width=\"50%\"><font size=\"-2\">");
	puts("Copyright &copy 1996 <a href=\"http://www.ipa.net/\">Internet Partners of America</a>");
	puts("</font></body></html>");
	msqlClose(dbh);
}
