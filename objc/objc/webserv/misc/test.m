#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/uio.h>
#include <unistd.h>
#include <dString.h>

void main(int argc, char **argv)
{
    int f, size;
    char buf[1024];
    id string=[dString alloc];

    if(argc>1)
    {
	[string init: atoi(argv[1])];
    }
    else
    {
	[string init];
    }

    f=open("/usr/dict/words", O_RDONLY, 0);
    for(;;)
    {
	size=read(f, buf, 1023);
	if(size!=1023)
	    break;
        [string appendString:buf];
    }
    close(f);


    [string lowercase];
    [string print];
    [string clear];
}
