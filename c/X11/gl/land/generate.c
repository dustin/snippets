/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: generate.c,v 1.1 1997/08/26 05:59:56 dustin Exp $
 */

#include <stdio.h>
#include <sys/types.h>

#include "land.h"

l_point **makepoints(l_header *head)
{
    l_point **points;
    int i, j, direction, distance;
    time_t t;

    time(&t);
    srand(t);

    points=(l_point **)malloc(sizeof(l_point)*head->width);
    if(points == NULL)
    {
        perror("malloc");
        exit(1);
    }

    for(i=0; i<head->height; i++)
    {
	points[i]=(l_point *)malloc(sizeof(l_point)*head->height);
	if(points[i]==NULL)
	{
	    perror("malloc");
	    exit(1);
	}
    }

    for(j=0; j<head->height; j++)
    {
        for(i=0; i<head->width; i++)
	{
	    direction=rand()%10;
	    direction= (direction>=5 ? 1 : -1);
	    distance=rand()%10;

	    if(j>0)
	        points[j][i].z=(points[j-1][i].z+(distance*direction)>0 ?
		    points[j-1][i].z+(distance*direction) : 0);
            else
		points[j][i].z=0;

	    if(points[j][i].z>head->maxz)
	       head->maxz=points[j][i].z;
	}
    }

    return(points);
}

void main(int argc, char **argv)
{
    FILE *f;
    l_header head;
    l_point **points;
    int i;

    if(argc<3)
    {
       puts("Wrong number of args, must specify the outfile, width and height");
       exit(1);
    }

    head.width=atoi(argv[2]);
    head.height=atoi(argv[3]);
    head.numpoints=head.width*head.height;

    points=makepoints(&head);

    f=fopen(argv[1], "wb");
    if(f==NULL)
    {
        perror("fopen");
        exit(1);
    }

    printf("Header is %d bytes\nBody will be %d bytes\n", sizeof(head),
        (sizeof(l_point)*head.numpoints));

    fwrite(&head, sizeof(head), 1, f);

    for(i=0; i<head.width; i++)
    {
        fwrite(points[i], sizeof(l_point), head.height, f);
    }

    fclose(f);

    for(i=0; i<head.width; i++)
	free(points[i]);
    free(points);
}
