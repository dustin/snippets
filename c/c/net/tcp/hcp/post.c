/*
 * Copyright (c) 1998 beyond.com
 * Written by Dustin Sallings
 *
 * $Id: post.c,v 1.1 1998/11/11 00:59:53 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <assert.h>

#include "http.h"

#define VERSION "0.1"
#define USERAGENT "Mozilla/4.04 [en] (X11; U; IRIX 6.2 IP22; Nav)"

void
_gendelimit(char *d, size_t len)
{
	snprintf(d, len-1, "---------------------------d%d",
		time(NULL), getpid());
}

struct status
postfile(char *url, char *path)
{
	struct status st;
	struct url u;
	struct host_ret conn;
	char delimit[80], line[1024];
	FILE *f, *tmp;
	int len;

	st.status=-1;
	st.message=NULL;
	st.bytesread=0;

	u = parseurl(url);
	if (u.port == -1) {
		st.message = strdup("Invalid url request format");
		return (st);
	}
	conn = openhost(u.host, u.port, u.ssl);
	if (conn.s < 0) {
		st.message = strdup("Could not connect to host");
		return (st);
	}

	f=fopen(path, "r");
	if(f==NULL) {
		perror(path);
		st.message=strdup("Error opening file for posting");
		return(st);
	}

	/* We have to build our post data into a tmp file so we can know how
	 * big the whole thing's going to be */
	tmp=tmpfile();
	if(tmp==NULL) {
		perror("tmpfile");
		st.message=strdup("Couldn't create a tmp file");
		return(st);
	}

	_gendelimit(delimit, 80);

	/* Tell it what we're doing */
	send_data(conn, u, "POST ");
	send_data(conn, u, u.req);
	send_data(conn, u, " HTTP/1.0\r\n");
	send_data(conn, u, "User-Agent: " USERAGENT "\r\n");
	snprintf(line, 1024, "Content-type: multipart/form-data; boundary=%s\r\n",
			delimit);
	send_data(conn, u, line);

	/* Write out tmp file */
	fprintf(tmp, "%s\r\n", delimit);
	fprintf(tmp, "Content-Disposition: form-data; "
		"name=\"file\"; filename=\"%s\"\r\n\r\n", path);

	/* Read in the file, and send it along */
	while( (len=fread(line, 1, 1024, f)) > 0 ) {
		fwrite(line, 1, len, tmp);
	}

	fprintf(tmp, "%s--\r\n", delimit);

	/* tell how long tmp is */
	snprintf(line, 1024, "Content-Length: %d\r\n\r\n", ftell(tmp));
	send_data(conn, u, line);

	/* rewind the file, we're going to send it now */
	rewind(tmp);

	/* Read in the file, and send it along */
	while( (len=fread(line, 1, 1024, tmp)) > 0 ) {
		send_ndata(conn, u, len, line);
	}

	fclose(f);
	fclose(tmp);

	st=getstatus(u, conn);

#ifdef USE_SSLEAY
    if (u.ssl) {
        if (conn.ssl)
            SSL_free(conn.ssl);
        if (conn.ctx)
            SSL_CTX_free(conn.ctx);
    }
#endif
    close(conn.s);
    freeurl(u);
    return (st);
}

int main(int argc, char **argv)
{
	struct status st;

	signal(SIGPIPE, SIG_IGN);
	signal(SIGALRM, timeout);

	if(argc<3) {
		printf("Usage:  %s url filename\n", argv[0]);
		exit(1);
	}
	st=postfile(argv[1], argv[2]);
	printf("Status was %d (%s)\n", st.status, st.message);
}
