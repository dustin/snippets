//
//  Plot.h
//  MemWatch
//
//  Created by Dustin Sallings on Tue Dec 09 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import <AppKit/AppKit.h>

#define MAX_OBJS 128

@interface Plot : NSView {
    NSMutableArray *data;
    int maxVal;
}

-(void)addDatum: (int) val;
-(void)setMaxVal: (int) val;

@end
