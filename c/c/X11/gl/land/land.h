/*
 * Copyright(c) 1997  Dustin Sallings
 *
 * $Id: land.h,v 1.1 1997/08/26 05:59:55 dustin Exp $
 */

typedef struct {
    float z;
} l_point;

typedef struct {
    char magic[20]; int numpoints;
    int maxz;
    int minz;
    int width;
    int height;
} l_header;

struct config {
    int wire;
    int model;
};

#define EXIT  0
#define WIRE  1
#define MODEL 2
#define REDRAW 3

l_point **openfile(char *filename, l_header *head);
