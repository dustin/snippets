/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: cluster.c,v 1.5 1998/01/06 08:03:27 dustin Exp $
 */

#include <config.h>
#include <redirect.h>
#include <readconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <assert.h>

/* A macro to append a new entry to a cluster list. */
#define lAPPEND(a) if(current==size-1) \
    { \
        _ndebug(4, ("Reallocating, now need %d bytes for %d\n", \
                    size*sizeof(struct cluster **), size)); \
        cluster=realloc(cluster, (size<<=1)*sizeof(struct cluster **)); \
        assert(cluster); \
    } \
    cluster[current++]=clusterDup(a);

extern struct confType *cf;
extern int _debug;

/*
 * Duplicate a cluster and everything in it and return a pointer to
 * the new cluster.
 */
struct cluster *clusterDup(struct cluster a)
{
    struct cluster *c;

    c=(struct cluster *)malloc(sizeof(struct cluster));
    memcpy(c, &a, sizeof(struct cluster));
    c->hostname=strdup(a.hostname);
    c->port=a.port;
    c->tcptimeout=a.tcptimeout;
    return(c);
}

/*
 * Free memory associated with a cluster.
 */
void freeCluster(struct cluster **c)
{
    int i;

    if(!c)
        return;

    for(i=0; c[i]; i++)
    {
        if(c[i]->hostname)
	{
            _ndebug(4, ("Freeing %s\n", c[i]->hostname));
            free(c[i]->hostname);
	}
        free(c[i]);
    }
    free(c);
}

/*
 * Look up an entry from the config file.
 * p1 is the cluster name, p2 is the element name.
 * defalrm is the default tcp timeout alarm time
 */
struct cluster lookupClusterEnt(char *p1, char *p2, int defalrm)
{
    char key[80];
    struct cluster element;

    element.hostname=NULL;
    element.port=0;
    element.tcptimeout=0;

    sprintf(key, "ports.%s.cluster.%s.remote_port", p1, p2);
    _ndebug(5, ("Looking up ``%s''\n", key));
    element.port=rcfg_lookupInt(cf, key);

    sprintf(key, "ports.%s.cluster.%s.remote_addr", p1, p2);
    _ndebug(5, ("Looking up ``%s''\n", key));
    element.hostname=rcfg_lookup(cf, key);

    _ndebug(5, ("Found ``%s''\n", element.hostname));

    sprintf(key, "ports.%s.cluster.%s.tcptimeout", p1, p2);
    _ndebug(5, ("Looking up ``%s''\n", key));
    element.tcptimeout=rcfg_lookupInt(cf, key);
    if(element.tcptimeout<1)
        element.tcptimeout=defalrm;
    return(element);
}

/*
 * Round robin clustering.
 */
struct cluster **clustRoundRobin(char *p, int stats)
{
    char **list=NULL;
    char key[80];
    struct cluster **cluster=NULL;
    int i, index, defalrm, current=0, size=4;

    cluster=(struct cluster**)malloc(size*sizeof(struct cluster **));
    assert(cluster);
    cluster[0]=NULL;

    _ndebug(2, ("cluster(\"%s\", %d)\n", p, stats));

    sprintf(key, "ports.%s.tcptimeout", p);
    defalrm=rcfg_lookupInt(cf, key);
    if(defalrm<1)
        defalrm=DEFCONTIME;

    sprintf(key, "ports.%s.cluster", p);
    list=rcfg_getSection(cf, key);
    for(i=0; list[i]!=NULL; i++);
    index=stats%i;
    _ndebug(2, ("Index got %d\n", index));

    /* Do keen round-robin thing in two for loops */
    for(i=index; list[i]; i++)
    {
        lAPPEND(lookupClusterEnt(p, list[i], defalrm));
    }

    /* Second loop isn't necessary if the index above was zero */
    if(index>0)
    {
	for(i=0; i<index; i++)
	{
            lAPPEND(lookupClusterEnt(p, list[i], defalrm));
	}
    }

    if(list)
        rcfg_freeSectionList(list);

    cluster[current]=NULL;
    return(cluster);
}

struct cluster **clustTopDown(char *p, int stats)
{
    char **list=NULL;
    char key[80];
    struct cluster **cluster=NULL;
    int i, index, defalrm, current=0, size=4;

    cluster=(struct cluster**)malloc(size*sizeof(struct cluster **));
    assert(cluster);
    cluster[0]=NULL;

    sprintf(key, "ports.%s.tcptimeout", p);
    defalrm=rcfg_lookupInt(cf, key);
    if(defalrm<1)
        defalrm=DEFCONTIME;

    sprintf(key, "ports.%s.cluster", p);
    list=rcfg_getSection(cf, key);
    if(list)
	rcfg_freeSectionList(list);
    freeCluster(cluster);
    return(NULL);
}

/*
 * Get the cluster algorithm, make it go, return the cluster.
 */
struct cluster **getcluster(char *p, int stats)
{
    char key[80];
    int i;
    char *al;
    void *(*f)(char *p, int stats);
    struct namedfunc funclist[]={
	{ "roundrobin", clustRoundRobin },
	{ "topdown", clustTopDown },
	{ NULL, NULL }
    };

    sprintf(key, "ports.%s.algorithm", p);
    al=rcfg_lookup(cf, key);
    if(al==NULL)
        al=funclist[0].cmd;

    for(i=0; funclist[i].cmd!=NULL; i++)
    {
	if(strncmp(funclist[i].cmd, al, strlen(al))==0)
	    break;
    }

    if(funclist[i].func==NULL)
    {
	_ndebug(2, ("Warning:  No clustering algorithm known named %s\n", al));
	f=(void *)funclist[0].func;
    }
    else
    {
	_ndebug(2, ("Using a %s algorithm for clustering.\n", al));
	f=(void *)funclist[i].func;
    }

    return((struct cluster **)f(p, stats));
}
