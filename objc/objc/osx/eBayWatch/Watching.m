//
//  Watching.m
//  eBayWatch
//
//  Created by Dustin Sallings on Wed Mar 05 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "Watching.h"


@implementation Watching

- (id)init
{
    contents=[[NSMutableArray alloc] initWithCapacity: 10];
    return(self);
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
    // NSLog(@"Asking for the number of rows (%d)", [contents count]);
    return([contents count]);
}

- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex
{
    id rv=nil;

    NSLog(@"Asking for row %d of %@", rowIndex, [aTableColumn identifier]);
    id theItem=[contents objectAtIndex: rowIndex];
    if([[aTableColumn identifier] isEqualToString: @"description"]) {
        rv=[theItem description];
    } else {
        rv=[NSString stringWithFormat: @"$ %.02f", [theItem price]];
    }

    return(rv);
}

-(void)addItem: (Item *)item
{
    NSLog(@"Adding %@", item);
    [contents addObject: item];
}

-(NSEnumerator *)objectEnumerator
{
    return([contents objectEnumerator]);
}


@end
