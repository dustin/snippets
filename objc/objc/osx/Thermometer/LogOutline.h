//
//  LogOutline.h
//  Thermometer
//
//  Created by Dustin Sallings on Mon Mar 10 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "ThermometerView.h"


@interface LogOutline : NSObject {

    NSArray *array;

}

-(id)initWithArray:(NSArray *)a;

- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item;
- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item;
- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item;
- (id)outlineView:(NSOutlineView *)outlineView
    objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item;

@end
