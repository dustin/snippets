/*
 * Copyright (c) 1998  Dustin Sallings <dustin@spy.net>
 *
 * $Id: main.c,v 1.2 2001/07/05 08:33:50 dustin Exp $
 */

#include <stdio.h>
#include <md5.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <limits.h>
#include <assert.h>

static char* md5dump(const unsigned char *md5)
{
	static char data[(16*2)+1];
	static char *map="0123456789abcdef";
	int i=0, j=0;

	for(i=0; i<16; i++) {
		data[j++]=map[((md5[i]&0xf0)>>4)];
		data[j++]=map[(md5[i]&0x0f)];
	}
	data[j]=0x00;
	return(data);
}

/* print a stat structure */
static void printstat(const char *path, const unsigned char *md5,
	const struct stat s)
{
	assert(path);
	assert(md5);

	printf("%s:" /* path */
		"md5=%s:" /* MD5 */
		"%s=%d:" /* dev */
		"%s=%d:" /* inode */
		"%s=0%o:" /* mode */
		"%s=%d:" /* nlink */
		"%s=%d:" /* uid */
		"%s=%d:" /* gid */
		"%s=%d:" /* rdev */
		"%s=%d:" /* size */
		"%s=%d\n", /* blocks */
		path,
		md5dump(md5),
		"dev", (long)s.st_dev,
		"inode", (long)s.st_ino,
		"mode", (long)s.st_mode&0777,
		"nlink", (long)s.st_nlink,
		"uid", (long)s.st_uid,
		"gid", (long)s.st_gid,
		"rdev", (long)s.st_rdev,
		"size", (long)s.st_size,
		"blocks", (long)s.st_blocks
	);
}

static void check(const char *path, const struct stat s)
{
	MD5_CTX ctx;
	unsigned char md5sig[16];

	assert(path);

	MD5Init(&ctx);
	MD5Update(&ctx, path, strlen(path));
	MD5Update(&ctx, (unsigned char *)&s, sizeof(s));
	MD5Final(&md5sig, &ctx);

	printstat(path, md5sig, s);
}

/* The recursive routine, dirty talker, directory walker */
static void dowork(const char *path)
{
	DIR *dir=NULL;
	struct dirent *d=NULL;
	struct stat s;
	char buf[PATH_MAX];

	assert(path);

	/* See what the thing passed in is */
	if(lstat(path, &s)<0) {
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
		while((d=readdir(dir)) != NULL) {
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
		check(path, s);
	}
}

int main(int argc, char **argv)
{
	dowork(argv[1]);
}
