/*
 * Copyright 1997 SPY Internetworking
 *
 * $Id: getstatus.c,v 1.5 1997/01/08 06:46:21 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "cracker.h"

int
openhost(char *host)
{
  struct hostent *hp;
  register int s;
  struct sockaddr_in sin;

  if ((hp = gethostbyname(host)) == NULL)
    {
      printf("ERR: gethostbyname\n");
      exit(1);
    }

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
      perror("socket");
      exit(1);
    }

  sin.sin_family = AF_INET;
  sin.sin_port = htons(PORT);
  bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

  if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0)
    {
      perror("connect");
      exit(1);
    }

  return (s);
}

void
showconn(struct dllist thing, int curtime)
{
  time_t t;
  int runtime;

  t = ntohl(thing.contime);

  runtime = (curtime - ntohl(thing.contime));

  printf(" Socket:     %d\n", thing.socket);
  printf(" Connected:  %s", ctime(&t));
  printf(" Runtime:    %d\n", runtime);
  printf(" Tries:      %d\n", ntohl(thing.tries));
  printf(" Tries/s:    %d\n", ntohl(thing.tries) / runtime);
  puts(" ----------");
}

void
main(int argc, char **argv)
{
  int s, i;
  struct command c;
  struct info stat;
  time_t t;
  char *p;

  s = openhost(argv[1]);

  c.command = htonl(COM_STAT);

  send(s, (char *) &c, sizeof(c), 0);

  i = recv(s, (char *) &stat, sizeof(stat), 0);

  while (i < sizeof(stat))
    {
      printf("Got %d bytes, needed %d  Will wait for %d more\n", i,
	     sizeof(stat), sizeof(stat) - i);
      p = (char *) &stat + i;
      i += recv(s, p, sizeof(stat) - i, 0);
    }

  t = ntohl(stat.starttime);
  printf("Num cons:    %d\n", (ntohl(stat.cons) - 1));
  printf("Attemps:     %d\n", ntohl(stat.tries));
  printf("Encrypted:   %s\n", stat.encrypted);
  printf("Last try:    %s\n", stat.curpass);
  printf("Start time:  %s", ctime(&t));
  printf("Runtime:     %d\n", ntohl(stat.curtime) - ntohl(stat.starttime));
  printf("Average:     %.2f\n\n", ((float)ntohl(stat.tries)/
	(ntohl(stat.curtime) - ntohl(stat.starttime))));

  for (i = 0; i < MAXCONS; i++)
    {
      if (ntohl(stat.acons[i].socket) > 0 && ntohl(stat.acons[i].tries) > 0)
	{
	  showconn(stat.acons[i], ntohl(stat.curtime));
	}
    }
}
