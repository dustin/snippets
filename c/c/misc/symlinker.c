/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: symlinker.c,v 1.1 1997/08/26 05:31:28 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>

#define LINKSIZE 8192

void do_dir(char *prev, char *dirname)
{
    DIR *dir;
    struct direct *d;
    struct stat st;
    char realname[LINKSIZE];
    char current[LINKSIZE];
    char filename[LINKSIZE];

    strcpy(current, prev);
    if(strlen(dirname))
    {
        strcat(current, "/");
        strcat(current, dirname);
    }

    dir=opendir(current);

    if(dir==NULL)
        return;

    while( (d=readdir(dir)) )
    {
        if(
            ( strcmp(d->d_name, ".")  != 0) &&
            ( strcmp(d->d_name, "..") != 0)
          )
        {
	    strcpy(filename, current);
	    strcat(filename, "/");
	    strcat(filename, d->d_name);

            if( lstat(filename, &st) == 0)
            {
                if(S_ISDIR(st.st_mode))
                {
                    do_dir(current, d->d_name);
                }
                else if(S_ISLNK(st.st_mode))
                {
		    realname[readlink(filename, realname, LINKSIZE)]=0x00;
                    printf("%s:%s\n", filename, realname);
                }
            }
        }
    }

    closedir(dir);
}

void main(void)
{
    do_dir(".", "");
}
