/* Copyright 1996 SPY Internetworking */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <utmp.h>
#include "protocol.h"

#define DOLOAD(x) htonl((int)(x*100))

extern int errno;

char *rcsinfo="$Id: spynetd.c,v 1.1 1998/01/15 09:17:57 dustin Exp $";

#ifdef linux
/* Linux ain't got naw getloadavg, so I got to make one */
void getloadavg(double load[3], int i)
{
FILE *l;
float fload[3];
	if( (l=fopen("/proc/loadavg", "r")) == NULL)
		return;
	fscanf(l, "%f %f %f", &fload[0], &fload[1], &fload[2]);
	fclose(l);

	for(i--; i>=0; i--)
		load[i]=fload[i];
}
#endif /* Linux */

int numusers(struct people p[])
{
char dev[20];
int i, ret;
FILE *utmp;
struct utmp ut;
struct stat st;
time_t now;

  ret=0;

  utmp=fopen(UTMP_FILE, "r");

  if(utmp!=NULL)
  {
    now=time(NULL);
    fseek(utmp, 0, SEEK_END);
    i=ftell(utmp);
    fseek(utmp, 0, 0);
    while(ftell(utmp)<i)
    {
      fread(&ut, sizeof(struct utmp), 1, utmp);
#ifdef USER_PROCESS
      if(ut.ut_type==USER_PROCESS)
#else
      if(ut.ut_name[0]!=0)
#endif /* USER_PROCESS */
      {
#ifdef DEBUG
	printf("%s gets counted\n", ut.ut_name);
#endif
	if(ret<NUSERS) /* Don't blow it up */
	{
	    strncpy(p[ret].login, ut.ut_name, 7);
	    strncpy(p[ret].tty, ut.ut_line, 7);
#ifndef __svr4__
	    strncpy(p[ret].host, ut.ut_host, 15);
#endif
	    p[ret].time=htonl(ut.ut_time);
	    strcpy(dev, "/dev/");
	    strcat(dev, p[ret].tty);
	    if(stat(dev, &st)>=0)
		p[ret].idle=htonl(now - st.st_atime);
	}
	ret++;
      }
#ifdef DEBUG
      else
      {
	printf("%s doesn't get counted\n", ut.ut_name);
      }
#endif
    }
#ifdef DEBUG
    printf("utmp file seeked to %d, utmp struct is %d, answer is %d\n",
		i, sizeof(struct utmp), ret);
#endif
    fclose(utmp);
  }

  return(ret);
}

void dosend(char *towhom)
{
  struct sysinfo p;
  double load[3];
  struct hostent *hp;
  struct sockaddr_in sin;
  register int s;
  char hostname[64];

  if(towhom==NULL)
  {
    gethostname(hostname, sizeof(hostname));
  }
  else
  {
    strcpy(hostname, towhom);
  }

#ifdef DEBUG
  fprintf(stderr, "Using %s as the host to connect to.\n",
	  hostname);
#endif

  if ((hp = gethostbyname(hostname)) == NULL)
    {
      herror("gethostbyname");
      exit(1);
    }

  if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      perror("client: socket");
      exit(1);
    }
  sin.sin_family = AF_INET;
  sin.sin_port = htons(1234);
  bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

  getloadavg(load, 3);

  memset(&p, 0, sizeof(p));
  p.version=htonl(PROTVER);
  p.users=numusers(p.user);
  p.users=htonl(p.users);
  p.load[0]=DOLOAD(load[0]);
  p.load[1]=DOLOAD(load[1]);
  p.load[2]=DOLOAD(load[2]);
  if(sendto(s, &p, INFOSIZE, 0, (struct sockaddr *)&sin, sizeof(sin)) <0)
  {
	perror("sendto");
  }
  close(s);
}

int
main(int argc, char *argv[])
{
#ifndef DEBUG
  if(fork()) exit(0);
#endif
  while(1)
  {
    dosend(BIGBROTHER);
    sleep(SLEEPTIME);
  }
}
