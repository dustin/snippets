#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>

#include "pageserv.h"

/* create a listening socket and break out */

int initialize(void)
{
    int reuse=1;
    int s;
    struct sockaddr_in sin;

    printf("Starting up on port %d\n", PORT);

    signal(SIGPIPE, SIG_IGN);

    if((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
	perror("server: socket");
	exit(1);
    }

    memset((char *) &sin, 0x00, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = htons(PORT);
    sin.sin_addr.s_addr = htonl(INADDR_ANY);

    setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
	(char *)&reuse, sizeof(int));

    if( bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0)
    {
	perror("server: bind");
	exit(1);
    }

    if(listen(s, 5) < 0)
    {
	perror("server: listen");
	exit(1);
    }

    return(s);
}
