/*
 * Copyright (c) 1998 beyond.com
 * Written by Dustin Sallings
 *
 * $Id: post.c,v 1.13 1998/11/17 01:03:40 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <string.h>
#include <assert.h>

#include "http.h"

#define USERAGENT "DUpload/$Revision: 1.13 $"

/* Generate a MIME delimiter */
static void
_gendelimit(char *d, size_t len)
{
	snprintf(d, len - 1, "---------------------------%d%d",
	    (int) time(NULL), getpid());
}

/* Post a file to a url */
struct status
postfile(char *url, char *path)
{
	struct status st;
	struct url u;
	struct host_ret conn;
	char    delimit[80], line[1024];
	FILE   *f, *tmp;
	int     len;

	st.status = -1;
	st.message = NULL;
	st.bytesread = 0;

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
	f = fopen(path, "r");
	if (f == NULL) {
		perror(path);
		st.message = strdup("Error opening file for posting");
		return (st);
	}
	/* We have to build our post data into a tmp file so we can know how
	 * big the whole thing's going to be */
	tmp = tmpfile();
	if (tmp == NULL) {
		perror("tmpfile");
		st.message = strdup("Couldn't create a tmp file");
		return (st);
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
	if (u.port == 80) {
		snprintf(line, 1024, "Host: %s\r\n", u.host);
	} else {
		snprintf(line, 1024, "Host: %s:%d\r\n", u.host, u.port);
	}
	send_data(conn, u, line);

	/* Write out tmp file */
	fprintf(tmp, "--%s\r\n", delimit);
	fprintf(tmp, "Content-Disposition: form-data; "
	    "name=\"file\"; filename=\"%s\"\r\n\r\n", path);

	while ((len = fread(line, 1, 1024, f)) > 0) {
		len = fwrite(line, 1, len, tmp);
	}

	fprintf(tmp, "\r\n--%s--\r\n", delimit);

	/* tell how long tmp is */
	snprintf(line, 1024, "Content-Length: %ld\r\n\r\n", ftell(tmp));
	send_data(conn, u, line);

	/* rewind the file, we're going to send it now */
	rewind(tmp);

	/* Read in the file, and send it along */
	while ((len = fread(line, 1, 1024, tmp)) > 0) {
		send_ndata(conn, u, len, line);
	}

	fclose(f);
	fclose(tmp);

	st = getstatus(u, conn);

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

static void
usage(char *progname)
{
	printf("Usage:  %s filename [filename...] desturl\n", progname);
	printf("hcp $Revision: 1.13 $\n"
	       "This build supports the following protocols:  http "
#ifdef USE_SSLEAY
		"https "
#endif
		"\n"
	);
}

int
main(int argc, char **argv)
{
	struct status st;
	int     i, ret = 0;

	signal(SIGPIPE, SIG_IGN);
	signal(SIGALRM, timeout);

	if (argc < 3) {
		usage(argv[0]);
		exit(1);
	}
	for (i = 1; i < (argc - 1); i++) {
		/* printf("Sending %s\n", argv[i]); */
		st = postfile(argv[argc - 1], argv[i]);
		printf("Status for %s was %d (%s)\n", argv[i], st.status, st.message);
		if (st.status != 200)
			ret++;
		freestatus(st);
	}
	return (ret);
}
