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

    [string setto: "This is the dictionary:\n\n"];

    [string readfile:"/usr/dict/words"];

    [string lowercase];
    [string print];

    [string setto: "End of file"];
    [string print];
    [string clear];
}
