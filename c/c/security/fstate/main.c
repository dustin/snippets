/*
 * Copyright (c) 1998  Dustin Sallings <dustin@spy.net>
 *
 * $Id: main.c,v 1.1 1998/10/24 21:30:21 dustin Exp $
 */

#include <stdio.h>
#include <md5.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <limits.h>

/* print a stat structure */

void printstat(char *path, struct stat s)
{
	printf("%s=%s:%s=%d:%s=%d:%s=0%o:%s=%d:%s=%d:%s=%d:%s=%d:%s=%d:%s=%d\n",
		"path", path,
		"dev", s.st_dev, "inode", s.st_ino, "mode", s.st_mode&0777,
		"nlink", s.st_nlink, "uid", s.st_uid, "gid", s.st_gid,
		"rdev", s.st_rdev, "size", s.st_size, "blocks", s.st_blocks
	);
}

/* The recursive routine, dirty talker, directory walker */
void dowork(char *path)
{
	DIR *dir;
	struct dirent *d;
	struct stat s;
	char buf[PATH_MAX];

	/* See what the thing passed in is */
	if(stat(path, &s)<0) {
		perror(path);
		return;
	}

	/* We want to do something different for directories and other things. */
	if(S_ISDIR(s.st_mode)) {
		dir=opendir(path);
		if(dir==NULL) {
			perror(path);
		    return;
		}
		while(d=readdir(dir)) {
			if(! (strcmp(d->d_name, ".")==0||strcmp(d->d_name, "..")==0) ) {
				if(strlen(path)+strlen(d->d_name)+2<PATH_MAX) {
					if(path[strlen(path)-1]=='/') {
						sprintf(buf, "%s%s", path, d->d_name);
					} else {
						sprintf(buf, "%s/%s", path, d->d_name);
					}
					dowork(buf);
				} else {
				printf("ERROR!!!  Filename too long for buffer: %s/%s\n",
			path, d->d_name);
				}
			}
		}
		closedir(dir);
	} else {
		printstat(path, s);
	}
}

int main(int argc, char **argv)
{
	dowork(argv[1]);
}
