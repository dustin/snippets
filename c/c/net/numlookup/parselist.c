/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: parselist.c,v 1.4 1999/05/08 08:35:48 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dlfcn.h>

#define THELIB "./libparselist.so"
#define THEFUNC "main"

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
		/* fprintf(stderr, "Trying _libtest_main\n"); */
		func = dlsym(lib, THEFUNC);
		/*
		 * if(func==NULL) {
		 * fprintf(stderr, "Damnit...couldn't find it there, either\n");
		 * }
		 */
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

void
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
		exit(1);
	}
	for (;;) {
		/* If it's been modified since we last recorded mod date... */
		if (lastmod(THELIB) > mod) {
			printf("Last modified: %s", ctime(&mod));
			/* record a new mod date */
			mod = lastmod(THELIB);
			/* close the library */
			dlclose(lib);
			/* open a new one. we don't care if it succeeds after start */
			lib = dlopen(THELIB, RTLD_LAZY);
		}
		func = getfunc(lib);
		if (func != NULL) {
			func(argc, argv);
		} else {
			/* Take a nap if we couldn't find the function */
			sleep(1);
		}
	}

	/* close it, we're leaving now, not that any of this will ever happen */
	dlclose(lib);
	exit(0);
}
