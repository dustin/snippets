/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.1 1998/01/02 02:49:48 dustin Exp $
 */

#include <redirect.h>
#include <readconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <assert.h>

struct confType *cf;

int mapcon(char *p)
{
     char key[80];
     char *host;
     int port;

     _ndebug(2, ("mapcon(%s)\n", p));

     sprintf(key, "ports.%s.remote_port", p);
     _ndebug(5, ("Looking up ``%s''\n", key));
     port=rcfg_lookupInt(cf, key);
     sprintf(key, "ports.%s.remote_addr", p);
     _ndebug(5, ("Looking up ``%s''\n", key));
     host=rcfg_lookup(cf, key);

     assert(host && port);
     return(getclientsocket( rcfg_lookup(cf, key), port));
}

int listento(char *gimme)
{
     char *host, *tmp, *ptr;
     int port=-1, s;

     /* as not to destroy it */
     ptr=strdup(gimme);

     host=ptr;
     for(tmp=ptr; *tmp; tmp++)
     {
	 if(*tmp==':')
	 {
	     *tmp=NULL;
	     tmp++;
	     port=atoi(tmp);
	 }
     }
     if(port==-1)
     {
	 host=NULL;
	 port=atoi(ptr);
     }

     s=getservsocket(host, port);
     free(ptr);
     return(s);
}

void _main(void)
{
    char **ports;
    struct sockaddr_in fsin;
    int i, s, cs, os, fromlen, upper, size, selected;
    fd_set fdset, tfdset;
    struct timeval t;
    int map[MAPSIZE];
    char *portmap[MAPSIZE];
    char buf[BUFLEN];

    /* Initialize maps */
    for(i=0; i<MAPSIZE; i++)
    {
	map[i]=-1;
	portmap[i]=NULL;
    }

    FD_ZERO(&tfdset);
    upper=0;

    ports=rcfg_getSection(cf, "ports");
    for(i=0; ports[i]; i++)
    {
	printf("Initialize port %s\n", ports[i]);
	s=listento(ports[i]);
	if(s>=0)
	{
	     if(s>upper)
		 upper=s;

	     _ndebug(3, ("Listening on, and Fdsetting %d\n", s));
	     FD_SET(s, &tfdset);
	     portmap[s]=strdup(ports[i]);
	}
    }
    rcfg_freeSectionList(ports);
    upper++;

    for(;;)
    {
	 fdset=tfdset;
	 t.tv_sec=0;
	 t.tv_usec=0;
	 fromlen=sizeof(fsin);

	 _ndebug(2, ("Selecting...\n"));
#if(PDEBUG>5)
         for(i=0; i<MAPSIZE; i++)
	 {
	     if(FD_ISSET(i, &fdset))
		 printf("    ...on %d\n", i);
	 }
#endif
	 if( (selected=select(MAPSIZE, &fdset, NULL, NULL, NULL)) > 0)
	 {
	     for(i=0; i<MAPSIZE; i++)
	     {
		 _ndebug(4, ("Testing %d for fdset\n", i));
		 if(FD_ISSET(i, &fdset))
		 {
		     _ndebug(2, ("Select found %d (portmaps to %s)\n",
				 i, portmap[i]));
		     if(portmap[i]!=NULL)
		     {
			  _ndebug(2, ("Got a connection for port %s\n",
				      portmap[i]));
			  if( (cs=accept(i, (struct sockaddr *)&fsin,
			       &fromlen)) >= 0)
                          {
			     if( (os=mapcon(portmap[i])) >= 0)
			     {
				 _ndebug(2, ("Got new connection, %d<->%d\n",
					     os, cs));
				 /* Select on both directions */
				 FD_SET(cs, &tfdset);
				 FD_SET(os, &tfdset);

				 /* Map both directions */
				 map[cs]=os;
				 map[os]=cs;
			     }
			     else
			     {
				 close(cs);
			     }
			  }
		     }
		     else /* We've got data, need to send it */
		     {
			 _ndebug(2, ("That's a listener\n"));
			 if(map[i]>0)
			 {
			     size=recv(i, buf, BUFLEN, 0);
			     if(size==0)
			     {
				 close(i);
				 close(map[i]);
				 FD_CLR(i, &tfdset);
				 FD_CLR(map[i], &tfdset);
				 map[map[i]]=-1;
				 map[i]=-1;
			     }
			     else
			     {
				 _ndebug(3, ("Sending %d bytes from %d "
					     "to %d\n", size, i, map[i]));
			         send(map[i], buf, size, 0);
			     }
			 }
		     }
		     if(--selected==0)
		     {
			 _ndebug(4, ("Select done\n"));
		         break;
		     }
		 }
	     }
	 }
	 else
	 {
	     perror("Select");
	     exit(0);
	 }
    } /* Infinite loop */
}

void main(void)
{
    cf=rcfg_readconfig(CONFFILE);
    _main();
}
