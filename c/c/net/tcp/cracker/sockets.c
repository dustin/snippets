/*
 * Copyright 1997 SPY Internetworking
 *
 * $Id: sockets.c,v 1.3 1997/01/07 08:07:32 dustin Exp $
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>

#include "cracker.h"

extern struct global *glob;

int
initialize(void)
{
  int reuse = 1;
  int s;
  struct sockaddr_in sin;

  signal(SIGPIPE, SIG_IGN);

  glob = (struct global *) malloc(sizeof(struct global));
  memset(glob, 0x00, sizeof(struct global));

  glob->acons = malloc((sizeof(glob->acons)) * MAXCONS);

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
      perror("server: socket");
      exit(1);
    }

  if ((fcntl(s, F_SETFL, O_NONBLOCK)) < 0)
    {
      perror("fcntl");
      exit(1);
    }

  memset((char *) &sin, 0x00, sizeof(sin));
  sin.sin_family = AF_INET;
  sin.sin_port = htons(PORT);
  sin.sin_addr.s_addr = htonl(INADDR_ANY);

  setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
	     (char *) &reuse, sizeof(int));

  if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0)
    {
      perror("server: bind");
      exit(1);
    }

  if (listen(s, 5) < 0)
    {
      perror("server: listen");
      exit(1);
    }

  return (s);
}

void
con_add(int new)
{
  struct dllist *n;

  n = (struct dllist *) malloc(sizeof(struct dllist));
  memset(n, 0x00, sizeof(struct dllist));

  n->socket = new;
  n->contime = time(0);

  glob->numusers++;
  glob->acons[new] = n;
  FD_SET(new, &glob->fdset);

  printf("Added socket %d\n", n->socket);
}

void
pipe_del(int n)
{
  printf("Closing socket %d -- broken pipe...", n);
  close(n);

  FD_CLR(n, &glob->fdset);
  free(glob->acons[n]);
  glob->acons[n] = NULL;
  glob->numusers--;

  puts("done");
}

void
con_del(int n)
{
  printf("Closing socket %d...", n);
  close(n);

  FD_CLR(n, &glob->fdset);
  free(glob->acons[n]);
  glob->acons[n] = NULL;
  glob->numusers--;

  puts("done");
}
