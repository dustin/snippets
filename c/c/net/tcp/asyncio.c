#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <syslog.h>
#include <netinet/tcp.h>
#include <assert.h>

fd_set rfdset, wfdset, efdset;
int fds=0;

int getConnectingSocket()
{
	int s;
	int r;
	int flags;
	struct hostent *hp=NULL;
	char *host;
	int port;
	struct sockaddr_in sin;

	host="www.beyond.com";
	port=23;

	printf("Doing DNS lookup...");
	fflush(stdout);
	hp = gethostbyname(host);
	assert(hp);
	printf("done!\n");

	s=socket(AF_INET, SOCK_STREAM, 0);
	assert(s>=0);

	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);
	memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);

	flags = fcntl(s, F_GETFL);
	r=fcntl(s, F_SETFL, flags | O_NONBLOCK);
	assert( r >= 0);

	printf("Calling connect...");
	fflush(stdout);
	r=connect(s, (struct sockaddr *)&sin, sizeof(sin));
	printf("returned %d\n", r);
	perror("connect");

	FD_SET(s, &efdset);
	FD_SET(s, &rfdset);
	FD_SET(s, &wfdset);

	if(s>=fds) {
		fds=s+1;
	}

	return(r);
}

int writeTo(int s)
{
	char *data="GET / HTTP/1.0\r\n\r\n";
	static int r=0;

	if(r==0) {
		r=write(s, data, strlen(data));
		printf("Wrote %d bytes of %d\n", r, strlen(data));
	}
	return(r);
}

int readFrom(int s)
{
	int r;
	char data[8192];

	memset(&data, 0x00, sizeof(data));
	r=read(s, data, sizeof(data));
	printf("read %d bytes:\n%s\n", r, data);
	if(r==0) {
		printf("Closing connection.\n");
		close(s);
		FD_CLR(s, &rfdset);
		FD_CLR(s, &wfdset);
		FD_CLR(s, &efdset);
	}
	return(r);
}

int handleException(int s)
{
	int r=0, v=0;

	r=getsockopt(s, SO_ERROR, 0, (void *)&v, NULL);
	printf("Exception:  %d (%d)\n", r, v);
	perror("getsockopt");
	return(r);
}

int main(int argc, char **argv)
{
	int r, i;
	fd_set trfdset, twfdset, tefdset;

	/* Zero my FD sets */
	FD_ZERO(&rfdset);
	FD_ZERO(&wfdset);
	FD_ZERO(&efdset);

	getConnectingSocket();

	for(;;) {
		trfdset=rfdset;
		twfdset=wfdset;
		tefdset=efdset;

		r=select(fds, &trfdset, &twfdset, &tefdset, NULL);
		printf("select returned %d\n", r);
		for(i=0; i<fds; i++) {
			if(FD_ISSET(i, &trfdset)) {
				printf("%d is ready for reading...\n", i);
				readFrom(i);
			}
			if(FD_ISSET(i, &twfdset)) {
				printf("%d is ready for writing...\n", i);
				writeTo(i);
			}
			if(FD_ISSET(i, &tefdset)) {
				printf("%d had an exception...\n", i);
				handleException(i);
			}
		}

	}
}
