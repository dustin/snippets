/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: parselist.c,v 1.10 1999/05/10 18:20:51 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dlfcn.h>

#include "parselist.h"

/*
 * Getfunc grabs the function from the shared object and returns it.  It's
 * pretty happy to receive a NULL lib pointer, at which point it returns a
 * NULL function.  A NULL function should probably not be executed.
 */
static void *
getfunc(void *lib)
{
	void   *func;

	if (lib == NULL) {
		return (NULL);
	}
	func = dlsym(lib, THEFUNC);
	if (func == NULL) {
		/* try the lib with an underscore */
		func = dlsym(lib, "_"THEFUNC);
	}
	return (func);
}

/*
 * Get the last modification date of a file.
 */
static time_t
lastmod(char *file)
{
	struct stat st;
	if (stat(file, &st) < 0) {
		return (0);
	}
	return (st.st_mtime);
}

/* this is in case we can't load the shared library, just print out an
 * error */
static void emergency(void)
{
	char buf[80], *tmp;
	time_t t;

	t=time(0);

	/* We'll run the emergency loop based on time */
	while( (time(0)-t)<EMERGENCY_TIME ) {
		tmp=fgets(buf, 79, stdin);
		puts("");
	}
}

int
main(int argc, char **argv)
{
	void   *lib = 0;
	void    (*func) (int argc, char **argv);
	time_t  mod = 0;

	mod = lastmod(THELIB);
	if (mod > 0) {
		printf("Last modified: %s", ctime(&mod));
	}
	/* initial dl opening */
	lib = dlopen(THELIB, RTLD_LAZY);
	if (lib == NULL) {
		puts(dlerror());
	}
	for (;;) {
		/* If it's been modified since we last recorded mod date... */
		if (lastmod(THELIB) > mod) {
			printf("Last modified: %s", ctime(&mod));
			/* record a new mod date */
			mod = lastmod(THELIB);
			/* close the library */
			if(lib)
				dlclose(lib);
			/* open a new one. we don't care if it succeeds after start */
			lib = dlopen(THELIB, RTLD_LAZY);
		}
		func = getfunc(lib);
		if (func != NULL) {
			func(argc, argv);
		} else {
			/* Emergency function, just read and report an error */
			emergency();
		}
	}

	/* close it, we're leaving now, not that any of this will ever happen */
	/* NOTREACHED */
	if(lib)
		dlclose(lib);
	return(0);
}
