//
//  SizeScaler.m
//  PhotoUpload
//
//  Created by Dustin Sallings on Sun Oct 06 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import "SizeScaler.h"


@implementation SizeScaler

-initWithSize: (NSSize)base
{
    [super init];
    baseSize=base;
    return(self);
}

-(NSSize)scaleTo: (NSSize)size
{
    float x=(float)baseSize.width;
    float y=(float)baseSize.height;
    float aspect=x/y;
    int newx=(int)x;
    int newy=(int)y;

    if(size.width <= newx || size.height <= newy) {

        newx=size.width;
        newy=(int)((float)newx/aspect);

        // If it exceeds the boundaries, do it the other way.
        if(newx > size.width || newy > size.height) {
            newy=size.height;
            newx=(int)((float)newy*aspect);
        }
    }

    NSSize rv;
    rv.width=newx;
    rv.height=newy;

    return(rv);
}

@end
