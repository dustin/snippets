/* Copyright 1996 SPY Internetworking */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <time.h>

#include <msql.h>

#include "protocol.h"


extern int errno;

char *rcsinfo="$Id: bigbro.c,v 1.1 1998/01/15 09:17:57 dustin Exp $";

char * printip(long ip)
{
static char ret[20];

	ret[0]=0;
	sprintf(ret, "%u.%u.%u.%u", ((ip & 0xff000000) / 0x1000000),
		((ip & 0x00FF0000) / 0x10000), ((ip & 0x0000FF00) / 0x100),
		(ip & 0x000000FF));
	return(ret);
}

char * printtm(time_t time)
{
static char ret[40];
char format[20];
int t[3];

	ret[0]=0;
	format[0]=0;

	t[0]=time/86400;
	time%=86400;
	t[1]=time/3600;
	time%=3600;
	t[2]=time/60;
	time%=60;

	if(time<0)
		time=0;

	if(t[0]>0)
		strcat(format, "%dd ");

	if(t[1]>9)
		strcat(format, "%d");
	else
		strcat(format, "0%d");

	if(t[2]>9)
		strcat(format, ":%d");
	else
		strcat(format, ":0%d");

	if(time>9)
		strcat(format, ":%d");
	else
		strcat(format, ":0%d");

#ifdef DEBUG
	printf("\nformat string: %s\n", format);
#endif

	if(t[0]>0)
		sprintf(ret, format, t[0], t[1], t[2], time);
	else
		sprintf(ret, format, t[1], t[2], time);

	return ret;
}

void printpacket(struct sysinfo p, long add)
{
struct hostent *h;
int a;
char *addr;
char hostname[64];
char query[200];
float load[3];
int dbh;
time_t now;

#define DOLOAD(x) (float)( (float)(ntohl(x))/(float)100)

	dbh=msqlConnect(NULL);
	if(dbh>=0)
		msqlSelectDB(dbh, "spy");

	h=gethostbyaddr( (char *)&add, sizeof(add), AF_INET);

	if(h==NULL)
	{
		addr=printip(ntohl(add));
		strcpy(hostname, addr);
	}
	else
	{
		strcpy(hostname, h->h_name);
	}

	p.version=ntohl(p.version);
	if(p.version!=PROTVER)
	{
		printf("Received an wrong protocol version from %s, %d\n",
			hostname, p.version);
		return;
	}

	p.users=ntohl(p.users);
	load[0]=DOLOAD(p.load[0]);
	load[1]=DOLOAD(p.load[1]);
	load[2]=DOLOAD(p.load[2]);

#ifdef DEBUG
	printf("%s\n\t%d users\n\t%.2f\n\t%.2f\n\t%.2f\n",
		hostname,p.users, load[0], load[1], load[2]);
#endif

	if(dbh>=0)
	{
		query[0]=0;
		sprintf(query, "delete from host where hostname='%s'",
			hostname);

		if(dbh>=0)
			msqlQuery(dbh, query);

		query[0]=0;
		sprintf(query, "delete from user where hostname='%s'",
			hostname);

		if(dbh>=0)
			msqlQuery(dbh, query);

		query[0]=0;
		now=time(NULL);
		sprintf(query,
			"insert into host\
			(hostname,users,load1,load2,load3,lastupdate)\n\
			values('%s', %d, %.2f, %.2f, %.2f, %u)",
			hostname, p.users, load[0], load[1], load[2], now);
#ifdef DEBUG
		puts(query);
#endif
		if(dbh>=0)
		    if(msqlQuery(dbh, query)<0)
		    {
			puts(query);
			perror(msqlErrMsg);
		    }
	}


	for(a=0; (a<p.users && a<NUSERS); a++)
	{
		query[0]=0;
		p.user[a].time=ntohl(p.user[a].time);
		p.user[a].idle=ntohl(p.user[a].idle);

#ifdef DEBUG
		printf("\t%s\t%s\t%s\t%s\t%s", p.user[a].login, p.user[a].tty,
			p.user[a].host, printtm(p.user[a].idle),
			ctime(&p.user[a].time));
#endif

		sprintf(query, "insert into user values('%s','%s','%s','%s',\
				%d, %d)", hostname, p.user[a].login,
				p.user[a].tty, p.user[a].host, p.user[a].idle,
				p.user[a].time);

		if(dbh>=0)
		    if(msqlQuery(dbh, query)<0)
		    {
			puts(query);
			perror(msqlErrMsg);

		    }
	}
	if(dbh>=0)
	{
		msqlClose(dbh);
	}
}

int
main(int argc, char *argv[])
{
  char hostname[64];
  struct hostent *hp;
  register int s, i;
  struct sockaddr_in sin;

  i=0;

  if (argc < 2)
    gethostname(hostname, sizeof(hostname));
  else
    strcpy(hostname, argv[1]);

  fprintf(stderr, "Using %s as the host to connect to.\n",
	  hostname);

  if(fork())
    exit(0);
  if ((hp = gethostbyname(hostname)) == NULL)
    {
      herror("gethostbyname");
      exit(1);
    }

  if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      perror("server: socket");
      exit(1);
    }

  memset(&sin, 0, sizeof(sin));
  sin.sin_family = AF_INET;
  sin.sin_port = htons(1234);
  bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

  if (bind(s, (struct sockaddr *)&sin, sizeof(sin)) < 0)
  {
      perror("server: bind");
      exit(1);
  }

  while(1)
  {
	struct sysinfo p;
	struct sockaddr_in from;
	int len=1500;
	int stat;

	stat=recvfrom(s, (char *)&p, INFOSIZE, 0, (struct sockaddr *)&from,
		&len);
	if(stat<0)
	{
		perror("recvfrom");
	}

#ifdef DEBUG
	printf("Got %d bytes\nAddress really is %u\n",
		stat, ntohl(from.sin_addr.s_addr));
#endif
	printpacket(p, from.sin_addr.s_addr);
  }

  close(s);
  exit(0);
}
