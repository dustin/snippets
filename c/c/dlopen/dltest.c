/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: dltest.c,v 1.1 1997/08/26 06:00:52 dustin Exp $
 */

#include <stdio.h>
#include <dlfcn.h>

void main(void)
{
    void *libtest;
    void (*func)(void);

    libtest=dlopen("./libtest.so", RTLD_LAZY);
    if(libtest==NULL)
    {
        puts(dlerror());
    }

    func=dlsym(libtest, "libtest_main");
    func();

    dlclose(libtest);
}
