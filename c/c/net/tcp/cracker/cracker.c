/*
 * Copyright 1997 SPY Internetworking
 *
 * $Id: cracker.c,v 1.5 2000/02/24 00:48:43 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <time.h>

#include "cracker.h"

struct global *glob;

void
docommand(struct command com, int s)
{
  switch (ntohl(com.command))
    {
    case (COM_STOP):
      getsolution(com);
      break;
    case (COM_INIT):
      inituser(s);
      break;
    case (COM_GET):
      sendpasswords(s);
      break;
    case (COM_STAT):
      sendstatus(s);
      break;
    }
}

void
main(int argc, char **argv)
{
  struct sockaddr_in fsin;
  int s, ns, fromlen, i, hits, size, done;
  struct command com;
  fd_set fdset;
  struct timeval t;
  time_t tdone;

  if (argc < 2)
    {
      printf("Need some args:\n%s password\n", argv[0]);
      exit(0);
    }

  s = initialize();

  done = 0;

  strcpy(glob->encrypted, argv[1]);

  if(argc >2)
  {
      printf("Setting initial password to %s\n", argv[2]);
      set_pass(argv[2], glob->password, glob->passmap);
  }

  printf("Listening socket is %d\n", s);

  glob->starttime = time(NULL);

  t.tv_sec = 1;
  t.tv_usec = 2600;

  fromlen = sizeof(fsin);

  FD_SET(s, &glob->fdset);

  while (done == 0)
    {
      fdset = glob->fdset;
      if ((hits = select(MAXCONS, &fdset, NULL, NULL, NULL)) > 0)
	{
	  if (FD_ISSET(s, &fdset) != 0)
	    {
	      puts("Got a connection");
	      if ((ns = accept(s, (struct sockaddr *) &fsin,
			       &fromlen)) >= 0)
		{
		  hits--;
		  con_add(ns);
		}
	      else
		{
		  perror("accept");
		}
	    }
	  for (i = s + 1; (i < MAXCONS && hits > 0); i++)
	    {
	      if (FD_ISSET(i, &fdset) != 0)
		{
		  hits--;
		  if ((size = recv(i, (char *) &com, sizeof(com), 0)) > 1)
		    {
		      docommand(com, i);
		      if (ntohl(com.command) == COM_STOP)
			done = 1;
		    }
		  else
		    {
		      pipe_del(i);
		    }
		}
	    }			/* for thingy */
	}
    }

  tdone = time(NULL);

  printf("Took %d seconds, %d tries\nAveraged %.2f tries/s\n",
	 (int) (tdone - glob->starttime), glob->numtries,
	 ((float) glob->numtries / (float) (tdone - glob->starttime)));
}
