/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: generate2.c,v 1.1 1997/08/26 05:59:56 dustin Exp $
 */

#include <stdio.h>
#include <sys/types.h>

#include "land.h"

void setheight(l_point **points, int x1, int y1, int x2, int y2)
{
    int xm, ym;
    time_t t;
    float median;

    time(&t);
    srand(t);

    printf("(%d,%d) (%d,%d)\n", x1, y1, x2, y2);

    if( ((x2-x1)<1) || ((y2-y1)<1))
	return;

    xm=x1+(x2-x1)/2;
    ym=y1+(y2-y1)/2;

    if(xm==x1 || ym==y1)
	return;

    printf("Got %f, %f, %f, %f\n", points[y1][x1].z, points[y1][x2].z,
	points[y2][x1].z, points[y2][x2].z);

    median=points[y1][x1].z+points[y1][x2].z+points[y2][x1].z+points[y2][x2].z;
    median/=4;

    #define sign ( (rand()%50 > 2 )? 1 : -1 )

    points[ym][xm-1].z=median+( (rand()%12)* sign);
    points[ym-1][xm-1].z=median+( (rand()%12) *sign);
    points[ym+1][xm-1].z=median+( (rand()%12) *sign);

    points[ym+1][xm+1].z=median+( (rand()%12) *sign);
    points[ym-1][xm+1].z=median+( (rand()%12) *sign);
    points[ym][xm+1].z=median+( (rand()%12) *sign);

    points[ym+1][xm].z=median+( (rand()%12) *sign);
    points[ym-1][xm].z=median+( (rand()%12) *sign);
    points[ym][xm].z=median+( (rand()%12) *sign);

    printf("Doing %d, %d %f\n", xm, ym, points[ym][xm].z);

    setheight(points, x1, y1, xm, ym);
    setheight(points, xm, y1, x2, ym);
    setheight(points, x1, ym, xm, y2);
    setheight(points, xm, ym, x2, y2);
}

l_point **makepoints(l_header *head)
{
    l_point **points;
    int i, j, direction, distance;

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

    points[0][0].z=(float)(rand()%50);
    printf("Doing %d,%d %f\n", 0, 0, points[0][0].z);

    points[0][head->width-1].z=(float)(rand()%50);
    printf("Doing %d,%d %f\n", head->width-1, 0, points[0][head->width-1].z);

    points[head->height-1][0].z=(float)(rand()%50);
    printf("Doing %d,%d %f\n", 0, head->height-1, points[head->height-1][0].z);

    points[head->height-1][head->width-1].z=(float)(rand()%50);
    printf("Doing %d,%d %f\n", head->width-1, head->height-1,
	points[head->height-1][head->width-1].z);

    setheight(points, 0, 0, head->width-1, head->height-1);

    for(j=0; j<head->height; j++)
    {
        for(i=0; i<head->width; i++)
	{
	    if(points[j][i].z>head->maxz)
	       head->maxz=(int)points[j][i].z+1;
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
