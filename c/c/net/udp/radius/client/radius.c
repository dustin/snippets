/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: radius.c,v 1.1 1998/06/21 08:33:30 dustin Exp $
 */

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

#include "md5.h"
#include "radius.h"

int getudpsocket(void)
{
    int s;

    if( (s=socket(AF_INET, SOCK_DGRAM, 0)) < 0 ) {
	perror("client: socket");
	return(-1);
    }

    return(s);
}

int radius_send(int s, char *server, int port, radius_packet *r)
{
    struct hostent *hp;
    struct sockaddr_in sin;

    if( (hp=gethostbyname(server)) == NULL) {
	printf("Couldn't resolve %s\n", server);
	return(-1);
    }

    sin.sin_family=AF_INET;
    sin.sin_port=port;
    memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);

    if( sendto(s, r, r->length, 0, (struct sockaddr *)&sin, sizeof(sin)) <0) {
	perror("sendto");
	return(-1);
    }

    return(0);
}

int radius_recv(int s, radius_packet *r)
{
    struct sockaddr sa;
    int salen;
    struct timeval t;
    fd_set fdset;

    salen=sizeof(sa);

    t.tv_sec=3;
    t.tv_usec=0;
    FD_ZERO(&fdset);
    FD_SET(s, &fdset);

    if( (select(s+1, &fdset, NULL, NULL, &t)) <= 0) {
	return(-1);
    }

    recvfrom(s, (char *)r, RADIUS_PACKET_SIZE, 0, &sa, &salen);
    return(0);
}

/*
 * This routine was basically stolen from mod_auth_radius
 */

static void add_attribute(radius_packet *rad, int type,
			  unsigned char *data, int length)
{
    attribute_t *p;

    p=(attribute_t *) ((unsigned char *)rad + rad->length);
    p->attribute=type;
    p->length=length+2;
    rad->length+=p->length;
    memcpy(p->data, data, length);
}

static unsigned char *xor(unsigned char *p, unsigned char *q, int len)
{
    for(;len>=0; len--) *(p++)^=*(q++);
    return(p);
}

int addpassword(radius_packet *rad, char *password)
{
    MD5_CTX md5, tmpmd5;
    unsigned char misc[1024];
    char pass[RADIUS_PASS_LENGTH];
    int i, len;

    /* Password */

    i=strlen(password);
    len=(i+0x0f)&0xfffffff0; /* round off to 16 */
    if(len==0) {
	len=16;
    } else if(len>128) {
	return(-1);
    }

    memset(pass, 0x00, len);
    memcpy(pass, password, i);

    MD5Init(&md5);
    MD5Update(&md5, "go", 2);
    tmpmd5=md5;
    MD5Update(&tmpmd5, rad->vector, RADIUS_VECTOR_LEN);
    MD5Final(misc, &tmpmd5);

    xor(pass, misc, RADIUS_PASS_LENGTH);

    for(i=1; i<(len>>4); i++) {
	tmpmd5=md5;
	MD5Update(&tmpmd5, &pass[(i-1)*RADIUS_PASS_LENGTH],RADIUS_PASS_LENGTH);
	MD5Final(misc, &tmpmd5);
	xor(&pass[i*RADIUS_PASS_LENGTH], misc, RADIUS_PASS_LENGTH);
    }

    add_attribute(rad, 2, pass, 16);

    return(0);
}

int simpleauth(char *username, char *password)
{
    MD5_CTX md5;
    int s, i, service, r;
    unsigned char send_buffer[RADIUS_PACKET_SIZE];
    radius_packet *rad;

    rad=(radius_packet *)send_buffer;

    rad->code=1;
    rad->id=getpid()&0xff;
    rad->length=RADIUS_HEADER_LENGTH;

    srand(time(NULL));
    i=rand()*getpid();

    MD5Init(&md5);
    MD5Update(&md5, &i, 4);
    MD5Final(&rad->vector,&md5);

    /* Username */
    add_attribute(rad, 1, username, strlen(username));
    if(addpassword(rad, password)<0)
	return(-1);

    service=8;
    add_attribute(rad, 6, &service, 4);

    s=getudpsocket();
    radius_send(s, "bleu", 1645, rad);

    r=radius_recv(s, rad);

    if(r<0)
       return(r);
    else
        return(rad->code);
}

int main(int argc, char **argv)
{
    int r;
    char *codes[]={
	0,
	"Request",
	"Accept",
	"Reject",
	0,0,0,0,0,0,0,
	"Challenge",
    };

    r=simpleauth(argv[1], argv[2]);

    if(r<0) {
        printf("Timeout\n");
    } else {
        printf("Server returned:  ``%s''\n", codes[r]);
    }
}
