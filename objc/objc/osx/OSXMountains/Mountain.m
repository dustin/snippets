//
//  Mountain.m
//  OSXMountains
//
//  Created by Dustin Sallings on Tue Apr 08 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "Mountain.h"


@implementation Mountain

-(id)initMountain:(double)f levels:(int)lv stop:(int)st
{
	id rv=[super init];

	// Default parameters
	rg1=false;
	rg2=false;
	rg3=false;
	cross=true;
	force_front=1;
	force_back=0;
	mix=0.0;
	midmix=0.0;

	/* Parameters */
	fdim=f;
	levels=lv;
	stop=st;

	/* Further initialization */
	pwid = 1 + (1 << (levels-stop));
	width = 1 + (1 << levels);
	fwidth = mwidth * (double) width/(double) pwid;
	len = mwidth/(double) pwid;

	// Get the initial fold
	f=[[Fold alloc] initFold:self levels:levels stop:stop length:len];
	[f sync];

	return(rv);
}

@end
