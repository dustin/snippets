/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: data.c,v 1.1 1997/08/26 05:59:56 dustin Exp $
 */

#include <stdio.h>

#include "land.h"

l_point **openfile(char *filename, l_header *head)
{
    FILE *f;
    l_point **points;
    int i;

    f=fopen(filename, "rb");
    if(f==NULL)
    {
        perror("fopen");
        exit(1);
    }

    fread(head, sizeof(l_header), 1, f);

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

    for(i=0; i<head->width; i++)
    {
        fread(points[i], sizeof(l_point), head->height, f);
    }

    fclose(f);
    return(points);
}
