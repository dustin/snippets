//
//  Mountain.h
//  OSXMountains
//
//  Created by Dustin Sallings on Tue Apr 08 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "Fold.h"

#define MEAN 0.0
#define MWIDTH 1.0
#define FORCEVAL 0.0

@interface Mountain : NSObject {

	bool rg1;
	bool rg2;
	bool rg3;
	bool cross;
	int force_front;
	int force_back;
	double mix;
	double midmix;
	double fdim;
	int levels;
	int stop;
	int width;
	double fwidth;

	Fold *f;
}

/* Get a mountain with an fdim, number of levels, and a stop value */
-(id)initMountain:(double)f levels:(int)lv stop:(int)st;

@end
