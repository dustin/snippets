//
//  Fold.h
//  OSXMountains
//
//  Created by Dustin Sallings on Tue Apr 08 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "Mountain.h"

#define START 0
#define STORE 1
#define NSTRIP 8

@interface Fold : NSObject {

	Mountain *p;
	int count;
	int stop;
	double length;
	double scale;
	double midscale;
	/* This should be an array of arrays of doubles */
	NSArray *s;
	/* This should be an array of doubles */
	NSArray *save;
	int state;
	int level;
	Fold *next;

}

/* Create a fold */
-(id)initFold:(Mountain *)param levels:(int)lv stop:(int)s length:(double)l;
-(void)clear;
-(void)sync;
-(NSArray *)nextStrip;

@end
