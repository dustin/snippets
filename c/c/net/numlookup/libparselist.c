#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <assert.h>

#include "mymalloc.h"

#define HAVE_VSPRINTF 1

#if !defined(HAVE_VSNPRINTF)
# if defined(HAVE_VSPRINTF)
#  define vsnprintf(a, b, c, d) vsprintf(a, c, d)
# else
#  error No vsnprintf *OR* vsprintf?  Call your vendor.
# endif
#endif

/* Length of a line */
#define LINELEN 90
#define CONFIGFILE "list"
#define LIFETIME 10

/* The address list */
struct addr {
	unsigned int addr;
	int mask;
	unsigned int intmask;
	char *data;
};

/* The config structure */
struct config_t {
	int size;
	int index;
	struct addr **addr;
};

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
	vsnprintf(s, n-1, format, ap);
	va_end(ap);
}
#endif

void
_do_log(int level, char *msg)
{
	openlog("testip", LOG_PID|LOG_NDELAY, LOG_LOCAL0);
	syslog(LOG_LOCAL0|level, msg);
	closelog();
}

void
_log(char *format, ...)
{
	va_list ap;
	char buf[LINELEN];
	va_start(ap, format);
	vsnprintf(buf, LINELEN-1, format, ap);
	va_end(ap);
	_do_log(LOG_INFO, buf);
}

/* Initialize a config structure */
struct config_t initConfig(void)
{
	struct config_t config;
	config.index=0;
	config.size=64;

	config.addr=malloc(config.size*sizeof(struct addr *));

	return(config);
}

/* I'm implementing my own quicksort because I can never get qsort to work */
static void quicksort(struct addr **a, int l, int r)
{
	int i, j;
	struct addr *v, *t;

	if(r>l) {
		v=a[r]; i=l-1; j=r;

		do {
			while( (a[++i]->mask > v->mask) && i<r );
			while( (a[--j]->mask < v->mask) && j>0 );
			t=a[i]; a[i]=a[j]; a[j]=t;
		} while(j>i);

		a[j]=a[i]; a[i]=a[r]; a[r]=t;
		quicksort(a, l, i-1);
		quicksort(a, i+1, r);
	}
}

/* This code is kinda duplicated from below, but we can deal with that */
int parseIP(const char *ip)
{
	int a[4];
	unsigned int ret;

	sscanf(ip, "%d.%d.%d.%d", &a[0],&a[1],&a[2],&a[3]);
	ret=(a[0]<<24)|(a[1]<<16)|(a[2]<<8)|a[3];
	return(ret);
}

/* A wrapper to quicksort */
static void addrsort(struct config_t config)
{
	quicksort(config.addr, 0, config.index-1);
}

/* Read the config */
struct config_t readconfig(void)
{
	char line[LINELEN];
	struct config_t config;
	FILE *f;

	/* allocate a new config */
	config=initConfig();

	f=fopen(CONFIGFILE, "r");
	if(f==NULL) {
		perror(CONFIGFILE);
		return(config);
	}

	/* We've got a macro for appending to the address list */

	#define LAPPEND(a) if(config.index == config.size-1) \
	{ \
		config.addr=realloc(config.addr, \
			(config.size<<=1)*sizeof(struct addr *)); \
		assert(config.addr); \
	} \
	config.addr[config.index++]=a;

	while(fgets(line, LINELEN-1, f)) {
		int a[4], mask, n;
		char data[LINELEN];
		struct addr *addr;

		line[strlen(line)-1]=0x00;

		n=sscanf(line, "%d.%d.%d.%d/%d:%s",&a[0],&a[1],&a[2],&a[3],&mask,data);
		if(n!=6) {
			_log("Error in config file [%s]\n", line);
			continue;
		}

		addr=malloc(sizeof(struct addr));
		assert(addr);

		/* Put data in data, we only need the network address and netmask */
		addr->mask=mask;
		if(addr->mask<0||addr->mask>32) {
			fprintf(stderr, "ERROR, netmask is invalid:  %s\n", line);
			free(addr);
			continue;
		}

		/* The IP address */
		addr->addr=(a[0]<<24)|(a[1]<<16)|(a[2]<<8)|a[3];
		/* The integer netmask */
		if(mask==0) {
			addr->intmask=0;
		} else {
			addr->intmask=(0xFFFFFFFF << (32 - mask));
		}
		/* apply the netmask to the address */
		addr->addr=addr->addr & addr->intmask;
		/* Copy the string in */
		addr->data=strdup(data);

		/* And, append it */
		LAPPEND(addr);
	}
	fclose(f);
	addrsort(config);
	return(config);
}

void destroyConfig(struct config_t config)
{
	int i;

	for(i=0; i<config.index; i++) {
		if(config.addr[i]) {
			if(config.addr[i]->data)
				free(config.addr[i]->data);
			free(config.addr[i]);
		}
	}
	free(config.addr);
}

char *search(struct config_t config, unsigned int ip)
{
	int i;

	for(i=0; i<config.index; i++) {
		/*
		printf("Testing %X/%d for %X (%X)\n", ip, config.addr[i]->mask,
			config.addr[i]->addr, config.addr[i]->intmask);
		*/
		if(config.addr[i]->addr == (ip&config.addr[i]->intmask) ) {
			return(config.addr[i]->data);
		}
	}
	return(NULL);
}

int main(int argc, char **argv)
{
	struct config_t config;
	int i;
	char *val, *tmp;
	char buf[LINELEN];

	config=readconfig();

	for(i=0; i<LIFETIME; i++) {
		tmp=fgets(buf, LINELEN-1, stdin);
		if(tmp==NULL) {
			break;
		}
		buf[strlen(buf)-1]=0x00;
		val=search(config, parseIP(buf));
		if(val) {
			_log("Received [%s] Sent [%s]", buf, val);
			puts(val);
		} else {
			_log("AHH!!!!  Nothing found for %s", buf);
		}
	}

	destroyConfig(config);

#ifdef MYMALLOC
	_mdebug_dump();
#endif
}
