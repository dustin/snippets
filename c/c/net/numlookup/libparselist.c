#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <stdarg.h>
#include <assert.h>

#include "mymalloc.h"
#include "hash.h"
#include "parselist.h"

/* Logging code */
#if !defined(HAVE_SNPRINTF)
/*
 * snprintf for those that don't have it. * More that likely, it'll overrun
 * buffers, because they
 * probably don't have vsnprintf either.
 */
int
snprintf(char *s, size_t n, const char *format,...)
{
	va_list ap;
	va_start(ap, format);
	vsnprintf(s, n - 1, format, ap);
	va_end(ap);
}

#endif

void
_do_log(int level, char *msg)
{
	openlog("testip", LOG_PID | LOG_NDELAY, LOG_LOCAL0);
	syslog(LOG_LOCAL0 | level, msg);
	closelog();
}

void
_log(char *format,...)
{
	va_list ap;
	char    buf[LINELEN];
	va_start(ap, format);
	vsnprintf(buf, LINELEN - 1, format, ap);
	va_end(ap);
	_do_log(LOG_INFO, buf);
}

/* Initialize a config structure */
struct config_t
initConfig(void)
{
	struct config_t config;
	int i;

	for(i=0; i<33; i++) {
		config.hash[i] = hash_init(HASHSIZE);

		/* Canned masks */
		if(i==0) {
			config.masks[0]=0;
		} else {
			config.masks[i]=(0xffffffff << (32-i));
		}
	}

	return (config);
}

/* This code is kinda duplicated from below, but we can deal with that */
int
parseIP(const char *ip)
{
	int     a[4];
	unsigned int ret;

	sscanf(ip, "%d.%d.%d.%d", &a[0], &a[1], &a[2], &a[3]);
	ret = (a[0] << 24) | (a[1] << 16) | (a[2] << 8) | a[3];
	return (ret);
}

/*
 * I usually call this routine ``killwhitey,'' but I stole it from
 * Nathan, so I'll keep the name.
 */
char *
getCleanLine(char *data, int size, FILE * infile)
{
	char *first_char, *comment, *last_char;

	if (fgets(data, size, infile) == NULL)
		return(NULL);

	comment = strchr(data, '#');

	if (comment != NULL)
		comment[0]=0x00;

	first_char = data + strspn(data, " \t");

	last_char = first_char + strlen(first_char) - 1;
	while (last_char >= first_char && index(" \t\n", *last_char)) {
		*last_char = 0x00;
		last_char--;
	}
	strcpy(data, first_char);
	return(data);
}

/* Read the config */
struct config_t
readconfig(void)
{
	char    line[LINELEN];
	struct config_t config;
	FILE   *f;

	/* allocate a new config */
	config = initConfig();

	f = fopen(CONFIGFILE, "r");
	if (f == NULL) {
		perror(CONFIGFILE);
		return (config);
	}
	/* We've got a macro for appending to the address list */

	while (getCleanLine(line, LINELEN - 1, f)) {
		int     a[4], mask, n, addr;
		char    data[LINELEN];

		n = sscanf(line, "%d.%d.%d.%d/%d:%s",
			&a[0], &a[1], &a[2], &a[3], &mask, data);
		if (n != 6) {
			_log("Error in config file [%s]\n", line);
			continue;
		}

		if (mask < 0 || mask > 32) {
			fprintf(stderr, "ERROR, netmask is invalid:  %s\n", line);
			continue;
		}

		/* The IP address */
		addr = (a[0] << 24) | (a[1] << 16) | (a[2] << 8) | a[3];

		/* apply the netmask to the address */
		addr&=config.masks[mask];

		/* store it in our hash */
		hash_store(config.hash[mask], addr, data);
	}
	fclose(f);
	return (config);
}

void
destroyConfig(struct config_t config)
{
	int     i;

	for (i = 0; i < 33; i++) {
		if (config.hash[i]) {
			hash_destroy(config.hash[i]);
		}
	}
}

char   *
search(struct config_t config, unsigned int ip)
{
	int     i, addr;
	struct	hash_container *h;

	/* The list is sorted by specifics of netmask.  We start with the
	 * most specific netmask, and go down to 0. */
	for (i = 32; i >= 0; i--) {
		addr=ip&config.masks[i];
		h=hash_find(config.hash[i], addr);
		if(h) {
			return(h->value);
		}
	}
	return (NULL);
}

int
main(int argc, char **argv)
{
	struct config_t config;
	int     i;
	char   *val, *tmp;
	char    buf[LINELEN];

	config = readconfig();

	for (i = 0; i < LIFETIME; i++) {
		tmp = fgets(buf, LINELEN - 1, stdin);
		if (tmp == NULL) {
			sleep(1);
			break;
		}
		buf[strlen(buf) - 1] = 0x00;
		val = search(config, parseIP(buf));
		if (val) {
			_log("Received [%s] Sent [%s]", buf, val);
			puts(val);
		} else {
			_log("AHH!!!!  Nothing found for %s", buf);
		}
	}

	destroyConfig(config);

	return(0);

#ifdef MYMALLOC
	_mdebug_dump();
#endif
}
