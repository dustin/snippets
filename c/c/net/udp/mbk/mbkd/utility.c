/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: utility.c,v 1.4 1998/10/02 07:02:32 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <assert.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include <mbkd.h>

/* Static declarations */
static int set_bit(int map, int bit);

#if !defined(HAVE_SNPRINTF)

/*
 * snprintf for those that don't have it.
 * More that likely, it'll overrun buffers, because they
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
    openlog("mbkd", LOG_PID | LOG_NDELAY, conf.log);
    syslog(conf.log | level, msg);
    closelog();
}

void
log_msg(char *format,...)
{
	va_list ap;
	char    buf[BUFLEN];
	va_start(ap, format);
	vsnprintf(buf, BUFLEN - 1, format, ap);
	va_end(ap);
    _do_log(LOG_INFO, buf);
}

void
log_debug(char *format,...)
{
	va_list ap;
	char    buf[BUFLEN];
	va_start(ap, format);
	vsnprintf(buf, BUFLEN - 1, format, ap);
	va_end(ap);
    _do_log(LOG_DEBUG, buf);
}

void
log_misc(int level, char *format, ...)
{
	va_list ap;
	char    buf[BUFLEN];
	va_start(ap, format);
	vsnprintf(buf, BUFLEN - 1, format, ap);
	va_end(ap);
    _do_log(level, buf);
}

int
checkpidfile(char *filename)
{
	int     pid, ret;
	FILE   *f;

	if ((f = fopen(filename, "r")) == NULL) {
		return (PID_NOFILE);
	} else {
		fscanf(f, "%d", &pid);

		_ndebug(2, ("Checking pid %d for life\n", pid));

		if (kill(pid, 0) == 0) {
			ret = PID_ACTIVE;
		} else {
			ret = PID_STALE;
		}
	}

	return (ret);
}

static void
quicksort(char **a, int l, int r)
{
	int     i, j;
	char   *v, *t;

	if (r > l) {
		v = a[r];
		i = l - 1;
		j = r;

		do {
			while ((strcmp(a[++i], v) < 0) && i < r);
			while ((strcmp(a[--j], v) > 0) && j > 0);

			t = a[i];
			a[i] = a[j];
			a[j] = t;
		} while (j > i);

		a[j] = a[i];
		a[i] = a[r];
		a[r] = t;

		quicksort(a, l, i - 1);
		quicksort(a, i + 1, r);
	}
}

void
stringListSort(char **list)
{
	int     i;

	for (i = 0; list[i] != NULL; i++);

	if (i > 0) {
		_ndebug(2, ("Calling quicksort(list, %d, %d)\n", 0, i - 1));
		quicksort(list, 0, i - 1);
	}
}

int
execnamedfunc(char *name, struct namedfunc *f)
{
	int     i;

	for (i = 0; f[i].name != NULL; i++) {
		if (strcmp(f[i].name, name) == 0)
			break;
	}

	if (f[i].name == NULL) {
		return (FUNC_UNKNOWN);
	} else {
		f[i].func();
	}

	return (0);
}

/*
 * Pass a pointer to an integer to keep up with the size,
 * the destination (known) array pointer, and the thing you want to
 * append.  Returns the resulting string.
 */
char   *
addtostr(int *size, char *dest, char *str)
{
	int     new = 0;

	_ndebug(5, ("addtostr(%d, %s, %s);\n", *size, dest, str));

	if (*size == 0) {
		_ndebug(5, ("Doing initial malloc\n"));

		*size = DEFAULT_STRLEN;
		dest = (char *) malloc(*size * sizeof(char));
		if (dest == NULL) {
			perror("malloc");
			exit(1);
		}
		new = 1;
	}
	if (strlen(dest) + strlen(str) >= (size_t) * size) {
		_ndebug(4, ("Realloc'in to %d bytes, need more than %d bytes\n",
			*size << 1, *size));

		*size <<= 1;
		dest = realloc(dest, *size * sizeof(char));
		if (dest == NULL) {
			perror("realloc");
			exit(1);
		}
	}
	if (new)
		strcpy(dest, str);
	else
		strcat(dest, str);

	return (dest);
}

/* This is a function instead of a macro because as a macro it
 * forces all constant strings to be duplicated at compile time
 */

int
puttext(int s, char *txt)
{
	return (write(s, txt, strlen(txt)));
}

int
gettext(int s, char *buf)
{
	int     size;
	if ((size = read(s, buf, BUFLEN - 1)) > 0) {
		buf[size] = 0x00;
		kw(buf);
		return (size);
	} else {
		/* Pipe breaking bastard */
		exit(0);
	}

	_ndebug(1, ("gettext() received:\n\t``%s''\n", buf));

	return (size);
}

int
gettextcr(int s, char *buf)
{
	int     size = 1, len = 0, toobig = 0;
	char    c = 0;

	/* eat any extra CR's and LF's */
	while ((len = read(s, buf, 1)) > 0) {
		if (buf[size - 1] == '\r') {
			size = 0;
			break;
		} else if (buf[size - 1] == '\n') {
			size = 0;
			break;
		} else {
			break;
		}
	}

	if (len == 0) {
		_ndebug(0, ("Broken pipe?\n"));
		exit(0);
	}
	while ((len = read(s, &c, 1)) > 0) {
		if (len == 0) {
			_ndebug(0, ("Broken pipe?\n"));
			exit(0);
		}
		size += len;

		if (!toobig) {

			if (size >= BUFLEN) {
				_ndebug(3, ("Truncating input, too long.\n"));
				buf[BUFLEN - 1] = 0x00;
				toobig = 1;
			}
			buf[size - 1] = c;
			buf[size] = 0x00;
		}
		if (c == '\r' || c == '\n')
			break;
	}

	kw(buf);

	_ndebug(1, ("gettextcr() received:\n\t``%s''\n", buf));

	return (size);
}

/* kill whitey, eat all the whitespace on the end of a string */

char   *
kw(char *in)
{
	/* bounds checking */
	if (strlen(in) == 0)
		return (in);

	while (isspace(in[strlen(in) - 1])) {
		/* bounds checking */
		if (strlen(in) == 0)
			return (in);

		in[strlen(in) - 1] = 0x00;
	}

	return (in);
}

int
f_exists(char *file)
{
	return (access(file, F_OK) == 0);
}

int
bit_set(int map, int bit)
{
	map >>= bit;
	map &= 1;

	return (map);
}

static int
set_bit(int map, int bit)
{
	int     blah;

	blah = 1;
	blah <<= bit;

	map |= blah;

	return (map);
}

/* Yeah, there's probably a better way to do this, but I had this code, and
 * it worked at least at one time for me...
 */
char  **
split(char c, char *string)
{
	int     i, j = 0, k = 0, length;
	char  **ret;
	char   *p;

	length = strlen(string);

	p = string + length - 1;

	/* how many we got? */
	for (i = 0; i < length; i++) {
		if (string[i] == c) {
			string[i] = 0x00;
			j++;
		}
	}

	j++;

	ret = (char **) malloc((j + 1) * sizeof(char *));
	ret[j--] = NULL;

	for (; j >= 0; j--) {
		while (*p && p >= string)
			p--;
		ret[j] = strdup(p + 1);
		p--;
	}

	return (ret);
}

/* free a char **, like one returned from the above routine */
void
freeptrlist(char **list)
{
	int     i;

	if (list == NULL)
		return;

	for (i = 0; list[i]; i++) {
		free(list[i]);
	}
	free(list);
}

/* binary -> ascii */

char   *
hexprint(int size, char *buf)
{
	int     i, j = 0;
	static char r[1024];
	static char *map = "0123456789abcdef";

	for (i = 0; i < size; i++) {
		r[j++] = map[((buf[i] & 0xf0) >> 4)];
		r[j++] = map[(buf[i] & 0x0f)];
	}
	r[j] = 0x00;
	return (r);
}

/* ascii -> binary */

char   *
unhexprint(int size, char *buf)
{
	int     i, j = 0;
	static char r[1024];
	int     map[256];

	for (i = 0; i < 256; i++) {
		if (i >= '0' && i <= '9') {
			map[i] = i - '0';
		} else if (i >= 'a' && i <= 'f') {
			map[i] = (i - 'a') + 10;
		} else if (i >= 'A' && i <= 'F') {
			map[i] = (i - 'A') + 10;
		} else {
			map[i] = -1;
		}
	}

	for (i = 0; i < size * 2; i += 2) {
		assert(map[buf[i]] >= 0 && map[buf[i + 1]] >= 0);
		r[j++] = (map[buf[i]] << 4 | map[buf[i + 1]]);
	}

	return (r);
}
