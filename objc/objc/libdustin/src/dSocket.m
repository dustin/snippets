//
//  rawsocket.m
//  Test
//
//  Created by Dustin Sallings on Sat Jan 12 2002.
//  Copyright (c) 2001 __MyCompanyName__. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <netinet/tcp.h>

#import <dSocket.h>

@implementation dSocket

-init
{
	s=-1;
	return self;
}

-(int)connectTo :(dString *)host port:(int)port
{
	char *hostname;
	struct hostent *hp;
	int success=1, i, flag;
	struct linger l;
	struct sockaddr_in sin;

	hostname=[host dup];

	if ((hp = gethostbyname(hostname)) == NULL) {
		// herror("gethostbyname");
		success = 0;
	}
	/* Don't need this anymore */
	free(hostname);
	/* If we didn't succeed, return failure */
	if(success==0) {
		return(-1);
	}

	/* of course, replace that 1 with the max number of con attempts */
	for (i = 0; i < 1; i++) {
		if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			return(-1);
		}
		sin.sin_family = AF_INET;
		sin.sin_port = htons(port);
		memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);

		l.l_onoff = 1;
		l.l_linger = 60;
		setsockopt(s, SOL_SOCKET, SO_LINGER, (char *) &l, sizeof(l));

		flag = 1;
		if (setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char *) &flag,
				 sizeof(int)) < 0) {
			puts("Nagle algorithm not dislabled.");
		}
		if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
			sleep(1);
			success=0;
		} else {
			success = 1;
			break;
		}
	}

	if (!success) {
		s=-1;
	}

	return(s);
}

-(int)send :(char *)msg size:(int)msgSize
{
	return(send(s, msg, msgSize, 0));
}

-(int)recv :(char *)buf size:(int)msgSize
{
	return(recv(s, buf, msgSize, 0));
}

-(int)sendString :(dString *)msg
{
	char *data=NULL;
	int len=0;

	data=[msg dup];
	len=[self send :data size:strlen(data)];
	return(len);
}

-readLine
{
	id rv=[[dString alloc] init :80];
	int len=0;
	char buf[]={0,0};

	len=recv(s,buf,1,0);
	if(buf[0]=='\r' || buf[0]=='\n') {
		len=0;
	}

	if(len!=0) {
		[rv append :buf];
	}

	while( (len=recv(s, buf, 1, 0)) >0 ) {
		buf[1]=0x00;
		if(buf[0]=='\r' || buf[0]=='\n') {
			break;
		}
		[rv append :buf];
	}

	return(rv);
}


@end
