/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dltest.c,v 1.2 1999/05/08 04:23:58 dustin Exp $
 */

#include <stdio.h>
#include <dlfcn.h>

void main(void)
{
    void *libtest;
    void (*func)(void);

    libtest=dlopen("./libtest.so", DL_LAZY);
    if(libtest==NULL) {
        puts(dlerror());
		exit(1);
    }

    func=dlsym(libtest, "libtest_main");
	if(func==NULL) {
		fprintf(stderr, "Trying _libtest_main\n");
		func=dlsym(libtest, "_libtest_main");
		if(func==NULL) {
			fprintf(stderr, "Damnit...couldn't find it there, either\n");
			exit(1);
		}
	}
    func();

    dlclose(libtest);
}
