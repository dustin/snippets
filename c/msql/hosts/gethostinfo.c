#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <cgi.h>
#include <msql.h>

void die(char *why)
{
	puts(why);
	exit(1);
}

void main(int argc, char *argv[])
{
int dbh, tmpint;
m_result *tmp;
m_row res;
char query[500];
static char *selected="SELECTED";
char *c[2]={"", ""}, *t[2]={"", ""}, *e[2]={"", ""}, *a[2]={"", ""};
struct cgiform *form;
char *hostname;

    puts("Content-type: text/html\n");
    puts("<html><head><title>Info</title></head>\n");
    puts("<body bgcolor=\"ffffff\">\n");

    if( (dbh=msqlConnect(NULL))<0)
	die(msqlErrMsg);

    if( msqlSelectDB(dbh, "web") < 0)
	die(msqlErrMsg);

    if(argc>1)
    {
	hostname=argv[1];
    }
    else
    {
        form=cgiinit();
	hostname=cgigetdata(form, "hostname");
    }

    sprintf(query, "select * from virtuals where hostname='%s'", hostname);

    if( msqlQuery(dbh, query) < 0)
    {
	printf("Oops, didn't like that:  %s<br>\n", msqlErrMsg);
	puts("</body></html>");
	exit(0);
    }

    tmp=msqlStoreResult();

    puts("<h2>Edit a virtual host</h2>");
    puts("<form action=\"/cgi-bin/tools/hosts/edithost.cgi\" \
        method=\"POST\">");

    res=msqlFetchRow(tmp);

    if(res==NULL)
    {
	printf("Problem getting the data<br>\n");
	puts("</body></html>");
	exit(0);
    }

    tmpint=atoi(res[6]);
    c[tmpint]=selected;

    tmpint=atoi(res[4]);
    t[tmpint]=selected;

    tmpint=atoi(res[5]);
    e[tmpint]=selected;

    tmpint=atoi(res[9]);
    a[tmpint]=selected;

    puts("<table>\n");
    printf("<tr><td>Hostname:</td><td><input name=\"hostname\" \
	value=\"%s\"></td></tr>\n", res[0]);

    printf("<tr><td>Admin:</td><td><input name=\"admin\" \
	value=\"%s\"></td></tr>\n", res[1]);

    printf("<tr><td>User:</td><td><input name=\"user\" \
	value=\"%s\"></td></tr>\n", res[2]);

    printf("<tr><td>Group:</td><td><input name=\"group\" \
	value=\"%s\"></td></tr>\n", res[3]);


    printf("<tr><td>Keep Transfer log:</td><td><select name=\"trans_log\">\
	<option value=\"0\" %s>No\n<option value=\"1\" %s>Yes</select>\n",
	t[0], t[1]);

    printf("<tr><td>Keep Error log:</td><td><select name=\"err_log\">\
	<option value=\"0\" %s>No\n<option value=\"1\" %s>Yes</select>\n",
	e[0], e[1]);

    printf("<tr><td>Create CGI bin:</td><td><select name=\"cgi_bin\">\
	<option value=\"0\" %s>No\n<option value=\"1\" %s>Yes</select>\n",
	c[0], c[1]);

    printf("<tr><td>Active:</td><td><select name=\"active\">\
	<option value=\"0\" %s>No\n<option value=\"1\" %s>Yes</select>\n",
	a[0], a[1]);

    puts("</table><input type=\"submit\" value=\"Save it\"></form><br>");
    printf("<a href=\"/cgi-bin/tools/hosts/deletehost.cgi?%s\">Delete %s</a>",
	res[0], res[0]);
    puts("<hr align=\"left\" width=\"50%\">");
    puts("<font size=\"-2\">");
    puts("Copyright &copy 1996 <a href=\"/\">SPY Internetworking</a>");
    puts("</font></body></html>");
    msqlFreeResult(tmp);
    msqlClose(dbh);
}
