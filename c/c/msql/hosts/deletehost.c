#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <msql.h>

void die(char *why)
{
	puts(why);
	exit(1);
}

void main(int argc, char *argv[])
{
int dbh;
char query[120], query2[120];
time_t c;

	puts("Content-type: text/html\n");

	if( (dbh=msqlConnect(NULL))<0)
		die(msqlErrMsg);

	if( msqlSelectDB(dbh, "web") < 0)
		die(msqlErrMsg);

        sprintf(query, "delete from virtuals where hostname='%s'", argv[1]);
	if( msqlQuery(dbh, query) < 0)
		die(msqlErrMsg);

	sprintf(query2, "insert into deletions values('%s', '%s', %d)",
		argv[1], getenv("REMOTE_USER"), (int)time(&c));
	msqlQuery(dbh, query2);

	printf("<html><head><title>Deleted %s</title></head>\n", argv[1]);
	puts("<body bgcolor=\"ffffff\">\n");
	printf("<h2>Deleted %s</h2>\n", argv[1]);
	printf("Deleted with query:<br>\n%s\n", query);

	puts("<hr width=\"50%\" align=\"left\"><font size=\"-2\">");
	puts("Copyright &copy 1996 <a href=\"http://www.ipa.net/\">Internet Partners of America</a>");
	puts("</font></body></html>");
	msqlClose(dbh);
}
