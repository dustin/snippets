//
//  AquaGraphics.m
//  OSXMountains
//
//  Created by Dustin Sallings on Tue Apr 08 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "AquaGraphics.h"


@implementation AquaGraphics

#include "paint.h"

Graph g={
1024,
768,
0,
0.3,
1.0,
0.3,
0.6,
2.5,
4.0,
(40.0 * PI)/180.0,
0.0,
0.5,
0.0,
0.6,
DEF_COL,
60,
10,
2,
FALSE,
TRUE,
20,
0,
0
};

/* Blank a region */
void blank_region(lx,ly,ux,uy)
int lx,ly,ux,uy;
{
    NSEraseRect(NSMakeRect(lx, ly, (ux-lx), (uy-ly)));
}

void flush_region(x, y, w, h)
int x;
int y;
int w;
int h;
{
  /* flush outstanding plots */
  /*
  plot_pixel(-1,0,0);
  XCopyArea(dpy,pix,win,gc,x,y,w,h,x,y);
  */
}

void plot_pixel( x, y, value )
int x;
int y;
Gun value;
{
    /* Insert implementation here */
}

void scroll_screen( dist )
int dist;
{
    /* Insert scrolling implementation here */
}

@end
