#include <stdio.h>
#include <stdlib.h>
#include "data.h"

char *addtostr(int *size, char *dest, char *str)
{
    int new=0;

    _ndebug(5, ("addtostr(%d, %s, %s);\n", *size, dest, str));

    if(*size==0)
    {
        _ndebug(5, ("Doing initial malloc\n"));

        *size=4;
        dest=(char *)malloc(*size*sizeof(char));
        if(dest==NULL)
        {
            perror("malloc");
            exit(1);
        }
        new=1;
    }

    if(strlen(dest)+strlen(str)>=(size_t)*size)
    {
        _ndebug(4, ("Realloc'in to %d bytes, need more than %d bytes\n",
                    *size<<1, *size));

        *size<<=1;
        dest=realloc(dest, *size*sizeof(char));
        if(dest==NULL)
        {
            perror("realloc");
            exit(1);
        }
    }

    if(new)
        strcpy(dest, str);
    else
        strcat(dest, str);

    return(dest);
}

void main(int argc, char **argv)
{
    char *str=NULL;
    int i, size=0;

    if(argc<2)
    {
	printf("Give me garbage to hash, bitch\n");
	exit(0);
    }

    for(i=1; i<argc; i++)
    {
	str=addtostr(&size, str, argv[i]);
	str=addtostr(&size, str, " ");
    }

    printf("In:  %s\n", str);
    printf("Out:  %d\n", _do_hash(str));
    free(str);
}
