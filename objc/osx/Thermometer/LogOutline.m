//
//  LogOutline.m
//  Thermometer
//
//  Created by Dustin Sallings on Mon Mar 10 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "LogOutline.h"

@implementation LogOutline

-(id)initWithArray:(NSArray *)a
{
	id rv=[super init];
    array=a;
    [array retain];
	return(rv);
}


- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item
{
    return([array objectAtIndex: index]);
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
    return(true);
}

- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
    return([[item lastReadings] count]);
}

// Just get the name of the item
- (id)outlineView:(NSOutlineView *)outlineView
    objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
    return([item name]);
}


@end
