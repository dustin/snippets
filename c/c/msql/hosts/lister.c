#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <msql.h>

void die(char *why)
{
	puts(why);
	exit(1);
}

void main(void)
{
int dbh;
m_result *tmp;
m_row res;
time_t clock;

	if( (dbh=msqlConnect(NULL))<0)
		die(msqlErrMsg);

	if( msqlSelectDB(dbh, "web") < 0)
		die(msqlErrMsg);

	if( msqlQuery(dbh, "select hostname, who_updated, date_updated from virtuals") < 0)
		die(msqlErrMsg);

	tmp=msqlStoreResult();

	puts("Content-type: text/html\n");

	puts("<html><head><title>Virtual Hosts</title></head>\n");
	puts("<body bgcolor=\"ffffff\">\n");
	puts("<h2>Virtual Hosts on Heavy</h2>\n<table>\n");
	puts("<tr><td>Hostname</td><td>Who updated</td><td>Last Updated</td></tr>");

	while( (res=msqlFetchRow(tmp)) !=NULL)
	{
		clock=atol(res[2]);

		printf("<tr><td color=\"eeeeff\"><a \
		href=\"/cgi-bin/tools/hosts/gethostinfo.cgi?%s\">%s</a></td> \
		<td>%s</td><td>%s</td></tr>\n",
		res[0], res[0], res[1], ctime(&clock) );
	}

	puts("</table><hr align=\"left\" width=\"50%\">");
	puts("<font size=\"-2\">");
	puts("Copyright &copy 1996 <a href=\"http://www.ipa.net/\">Internet Partners of America</a>");
	puts("</font></body></html>");
	msqlFreeResult(tmp);
	msqlClose(dbh);
}
