//
//  Plot.m
//  MemWatch
//
//  Created by Dustin Sallings on Tue Dec 09 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import "Plot.h"
#import "Datum.h"

@implementation Plot

- (id)initWithFrame:(NSRect)frame {
    data = [[NSMutableArray alloc] init];
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    return self;
}


- (void)drawRect:(NSRect)rect {
    // Drawing code here.
    // Convert the numbers to points
    // NSLog(@"Drawing area is %.0fx%.0f", rect.size.width, rect.size.height);
    float pixdiff=rect.size.width/((float)[data count]-1.0);
    // NSLog(@"There are %f pixels between points", pixdiff);
    // Do the conversion
    NSEnumerator *enumerator = [data objectEnumerator];
    id object;
    float x=0;
    NSBezierPath *path=[NSBezierPath bezierPath];
    while (object = [enumerator nextObject]) {
        int v=[object intValue];
        // NSLog(@"Plotting %d", v);
        NSPoint p=NSMakePoint(x, ((float)v/(float)maxVal)*rect.size.height);
        // NSLog(@"Point at %f,%f", p.x, p.y);
        if(x == 0) {
            [path moveToPoint: p];
        } else {
            [path lineToPoint: p];
        }
        x+=pixdiff;
    }
    [path stroke];
}

-(void)addDatum: (int)val {
    [data addObject: [[Datum alloc] initWithInt: val]];
    if(val > maxVal) {
        maxVal = val;
    }
    if([data count] > MAX_OBJS) {
        [data removeObjectAtIndex: 0];
    }
    [self setNeedsDisplay: true];
}

-(void)setMaxVal: (int) val {
    maxVal=val;
}

-(void)clear
{
    [data removeAllObjects];
    [self setNeedsDisplay: true];
}

@end
