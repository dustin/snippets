#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include "data.h"

void main(int argc, char **argv)
{
    char buf[1024];
    int stats[HASHSIZE], i, highest, lowest, average;
    FILE *f;

    memset(&stats, 0x00, HASHSIZE*sizeof(int));

    if(argc<2)
    {
	printf("Give me garbage to hash, bitch\n");
	exit(0);
    }

    f=fopen(argv[1], "r");
    assert(f);

    printf("Max is %d\n", HASHSIZE);

    for(;;)
    {
	fgets(buf, 1024, f);
	if(feof(f))
	    break;
	if(_do_hash(buf)>HASHSIZE){
	    printf("%d is too big for %s", _do_hash(buf), buf);
	    exit(1);
	}
        stats[_do_hash(buf)]++;
	/* printf("%d for %s", _do_hash(buf), buf); */
    }

    highest=0;
    average=0;
    lowest=INT_MAX;

    for(i=0; i<HASHSIZE; i++)
    {
	if(stats[i]>0)
	    printf("%3x:  %d\n", i, stats[i]);
	if(stats[i]<lowest)
	    lowest=stats[i];
	if(stats[i]>highest)
	    highest=stats[i];
	average+=stats[i];
    }

    printf("\nHighest:  %d\nLowest:   %d\nAverage:  %d\n",
	   highest, lowest, average/HASHSIZE);
}
