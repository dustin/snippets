/* Copyright (c) 1997  SPY Internetworking */
/* $Id: dslookup.c,v 1.1 1997/12/22 08:55:11 dustin Exp $ */
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

unsigned int lookup(char *host)
{
    struct hostent *hp;
    unsigned int blah;

    if((hp=gethostbyname(host))==NULL) return(0);
    memcpy(&blah, hp->h_addr, hp->h_length);
    return(ntohl(blah));
}

void main(int argc, char **argv)
{
    if(argc>1)   printf("%s is 0x%x\n", argv[1], lookup(argv[1]));
    else         printf("Usage:  %s <hostname>\n", argv[0]);
}
