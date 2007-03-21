//
//  AquaGraphics.m
//  OSXMountains
//
//  Created by Dustin Sallings on Tue Apr 08 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "AquaGraphics.h"

@implementation AquaGraphics

/* Blank a region */
void blank_region(lx,ly,ux,uy)
int lx,ly,ux,uy; 
{
	NSLog(@"Blanking %d,%d to %d,%d", lx, ly, ux, uy);
    NSEraseRect(NSMakeRect(lx, ly, (ux-lx), (uy-ly)));
}

void flush_region(x, y, w, h)
int x;
int y;
int w;
int h;
{
  /* flush outstanding plots */
	NSLog(@"Flushing %d,%d to %d,%d", x, y, w, h);
  /*
  plot_pixel(-1,0,0);
  XCopyArea(dpy,pix,win,gc,x,y,w,h,x,y);
  */
}

/*
void plot_pixel( x, y, value )
int x;
int y;
Gun value;
{
    // Insert implementation here
	NSLog(@"Plotting at %d,%d", x, y);
}
*/

void scroll_screen(NSRect bounds, int dist )
{
    /* Insert scrolling implementation here */
	NSLog(@"Scrolling %d", dist);
	NSRect portion=NSMakeRect(dist, 0, bounds.size.width-dist,
		bounds.size.height);
	// NSPoint p=NSMakePoint(0, bounds.size.height);
	NSPoint p=NSMakePoint(0, 0);
	NSCopyBits(0, portion, p);
	blank_region((int)bounds.size.width-dist, 0,
		(int)bounds.size.width, (int)bounds.size.height);
}

@end
